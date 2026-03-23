use alloc::{boxed::Box, format, string::String, sync::Arc, vec::Vec};

use crate::{
    error::{Error, Result},
    expr::Expr,
    op::{BinOp, UnaryOp},
    token::Token,
    val::Val,
};

const MAX_PARSE_DEPTH: usize = 128;
const MAX_BINARY_CHAIN_LEN: usize = 256;
const MAX_AT_PATH_SEGMENTS: usize = 256;
static NIL_TOKEN: Token = Token::Nil;

pub struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
    len: usize,
    depth: usize,
}

impl<'a> Parser<'a> {
    fn shared_str_expr(value: &Arc<str>) -> Expr {
        Expr::Val(Val::Str(Arc::clone(value)))
    }

    fn current_token_or_nil(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or(&NIL_TOKEN)
    }

    fn consume_if(&mut self, token: &Token) -> bool {
        if self.tokens.get(self.pos) == Some(token) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn ensure_not_eof(&self, msg: &str) -> Result<()> {
        if self.eof() {
            return Err(Error::Parse(self.err(msg)));
        }
        Ok(())
    }

    fn expect_punctuation(&mut self, token: Token, symbol: &str) -> Result<()> {
        if self.consume_if(&token) {
            return Ok(());
        }

        let msg = format!("Expecting '{symbol}', found {:?}", self.current_token_or_nil());
        Err(Error::Parse(self.err(&msg)))
    }

    fn expect_expr_start(&self, msg: &str) -> Result<()> {
        if self.is_valid_expr_start() {
            return Ok(());
        }

        let msg = format!("{msg}: {:?}", self.current_token_or_nil());
        Err(Error::Parse(self.err(&msg)))
    }

    fn finish_parenthesized_expr(&mut self) -> Result<Expr> {
        let expr = self.parse_expr()?;
        self.expect_punctuation(Token::RParen, ")")?;
        Ok(expr)
    }

    fn expect_at_path_start(&self) -> Result<()> {
        match self.current_token_or_nil() {
            Token::Id(_) | Token::LParen | Token::Str(_) => Ok(()),
            other => {
                let msg = format!("Expecting field name, found {:?}", other);
                Err(Error::Parse(self.err(&msg)))
            }
        }
    }

    fn invalid_list_separator_message(&self) -> String {
        format!(
            "Invalid separator in list: {:?}. Use ',' to separate elements",
            self.current_token_or_nil()
        )
    }

    fn parse_comma_separated<T, F>(
        &mut self,
        closing: Token,
        closing_label: &str,
        first_item_msg: &str,
        after_comma_msg: &str,
        invalid_separator_msg: Option<fn(&Self) -> String>,
        mut parse_item: F,
    ) -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>,
    {
        let mut items = Vec::new();
        if self.consume_if(&closing) {
            return Ok(items);
        }

        self.expect_expr_start(first_item_msg)?;

        loop {
            items.push(parse_item(self)?);

            if self.consume_if(&closing) {
                return Ok(items);
            }

            if self.eof() {
                let msg = format!("Expecting '{closing_label}', found {:?}", self.current_token_or_nil());
                return Err(Error::Parse(self.err(&msg)));
            }

            if self.consume_if(&Token::Comma) {
                if self.consume_if(&closing) {
                    return Ok(items);
                }
                self.expect_expr_start(after_comma_msg)?;
                continue;
            }

            let msg = invalid_separator_msg.map(|build| build(self)).unwrap_or_else(|| {
                format!(
                    "Expecting ',' or '{closing_label}', found {:?}",
                    self.current_token_or_nil()
                )
            });
            return Err(Error::Parse(self.err(&msg)));
        }
    }

    fn parse_map_pair(&mut self) -> Result<(Box<Expr>, Box<Expr>)> {
        let key = Box::new(self.parse_expr()?);
        self.expect_punctuation(Token::Colon, ":")?;
        self.expect_expr_start("Invalid map value after ':'")?;
        let value = Box::new(self.parse_expr()?);
        Ok((key, value))
    }

    pub fn parse(&mut self) -> Result<Expr> {
        if self.eof() {
            return Ok(Expr::Val(Val::Nil));
        }

        let exp = self.parse_expr()?;

        if !self.eof() {
            return Err(Error::Parse(self.err("Unexpected tokens at end")));
        }

        // All sub-expressions parsed, apply constant folding optimization
        Ok(exp.fold_constants())
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        self.enter_nested("Expression nesting is too deep")?;
        let result = self.parse_ternary();
        self.leave_nested();
        result
    }

    /// `expr ? expr : expr` (right-associative ternary)
    fn parse_ternary(&mut self) -> Result<Expr> {
        let expr = self.parse_coalesce()?;
        if !self.eof() && self.tokens[self.pos] == Token::Question {
            self.pos += 1;
            let true_expr = self.parse_ternary()?;
            self.expect_punctuation(Token::Colon, ":")?;
            let false_expr = self.parse_ternary()?;
            Ok(Expr::Ternary(Box::new(expr), Box::new(true_expr), Box::new(false_expr)))
        } else {
            Ok(expr)
        }
    }

    /// `expr ?? expr` (left-associative nullish coalescing)
    fn parse_coalesce(&mut self) -> Result<Expr> {
        let mut expr = self.parse_or()?;
        let mut chain_len = 0usize;
        while !self.eof() {
            match self.tokens[self.pos] {
                Token::QuestionQuestion => {
                    chain_len += 1;
                    if chain_len > MAX_BINARY_CHAIN_LEN {
                        return Err(Error::Parse(self.err("Coalesce chain is too long")));
                    }
                    self.pos += 1;
                    let right = self.parse_or()?;
                    expr = Expr::Coalesce(Box::new(expr), Box::new(right));
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    /// - `expr || expr`
    fn parse_or(&mut self) -> Result<Expr> {
        let mut expr = self.parse_and()?;
        let mut chain_len = 0usize;
        while !self.eof() {
            match self.tokens[self.pos] {
                Token::Or => {
                    chain_len += 1;
                    if chain_len > MAX_BINARY_CHAIN_LEN {
                        return Err(Error::Parse(self.err("Logical OR chain is too long")));
                    }
                    self.pos += 1;
                    let right = self.parse_and()?;
                    expr = Expr::Or(Box::new(expr), Box::new(right));
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    /// `expr && expr`
    fn parse_and(&mut self) -> Result<Expr> {
        let mut expr = self.parse_cmp()?;
        let mut chain_len = 0usize;
        while !self.eof() {
            match self.tokens[self.pos] {
                Token::And => {
                    chain_len += 1;
                    if chain_len > MAX_BINARY_CHAIN_LEN {
                        return Err(Error::Parse(self.err("Logical AND chain is too long")));
                    }
                    self.pos += 1;
                    let right = self.parse_cmp()?;
                    expr = Expr::And(Box::new(expr), Box::new(right));
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    /// - `expr == expr`
    /// - `expr != expr`
    ///   ...
    fn parse_cmp(&mut self) -> Result<Expr> {
        let mut expr = self.parse_add_sub()?;
        let mut chain_len = 0usize;
        while !self.eof() {
            let op = match self.tokens[self.pos] {
                Token::Eq => BinOp::Eq,
                Token::Ne => BinOp::Ne,
                Token::Gt => BinOp::Gt,
                Token::Lt => BinOp::Lt,
                Token::Ge => BinOp::Ge,
                Token::Le => BinOp::Le,
                Token::In => BinOp::In,
                _ => break,
            };
            chain_len += 1;
            if chain_len > MAX_BINARY_CHAIN_LEN {
                return Err(Error::Parse(self.err("Comparison chain is too long")));
            }
            self.pos += 1;
            let right = self.parse_add_sub()?;
            expr = Expr::Bin(Box::new(expr), op, Box::new(right));
        }
        Ok(expr)
    }

    /// - `expr + expr`
    /// - `expr - expr`
    fn parse_add_sub(&mut self) -> Result<Expr> {
        let mut expr = self.parse_mul_div()?;
        let mut chain_len = 0usize;
        while !self.eof() {
            let op = match self.tokens[self.pos] {
                Token::Add => BinOp::Add,
                Token::Sub => BinOp::Sub,
                _ => break,
            };
            chain_len += 1;
            if chain_len > MAX_BINARY_CHAIN_LEN {
                return Err(Error::Parse(self.err("Add/sub chain is too long")));
            }
            self.pos += 1;
            let right = self.parse_mul_div()?;
            expr = Expr::Bin(Box::new(expr), op, Box::new(right));
        }
        Ok(expr)
    }

    /// - `expr * expr`
    /// - `expr / expr`
    fn parse_mul_div(&mut self) -> Result<Expr> {
        let mut expr = self.parse_unary()?;
        let mut chain_len = 0usize;
        while !self.eof() {
            let op = match self.tokens[self.pos] {
                Token::Mul => BinOp::Mul,
                Token::Div => BinOp::Div,
                Token::Mod => BinOp::Mod,
                _ => break,
            };
            chain_len += 1;
            if chain_len > MAX_BINARY_CHAIN_LEN {
                return Err(Error::Parse(self.err("Mul/div chain is too long")));
            }
            self.pos += 1;
            let right = self.parse_unary()?;
            expr = Expr::Bin(Box::new(expr), op, Box::new(right));
        }
        Ok(expr)
    }

    /// - `!expr`
    /// - `-expr`
    /// - `expr`
    fn parse_unary(&mut self) -> Result<Expr> {
        if self.eof() {
            return Err(Error::Parse(self.err("Unexpected end of expression")));
        }

        match &self.tokens[self.pos] {
            Token::Not => {
                self.enter_nested("Unary nesting is too deep")?;
                self.pos += 1;
                let expr = self.parse_unary();
                self.leave_nested();
                let expr = expr?;
                Ok(Expr::Unary(UnaryOp::Not, Box::new(expr)))
            }
            Token::Sub => {
                self.enter_nested("Unary nesting is too deep")?;
                self.pos += 1;
                let expr = self.parse_unary();
                self.leave_nested();
                let expr = expr?;
                Ok(Expr::Unary(UnaryOp::Neg, Box::new(expr)))
            }
            _ => self.parse_postfix(),
        }
    }

    /// - `primary`
    /// - `primary.field`
    /// - `primary.field.field`
    fn parse_postfix(&mut self) -> Result<Expr> {
        let mut expr = self.parse_primary()?;

        // 处理点访问
        while !self.eof() && self.tokens[self.pos] == Token::Dot {
            self.pos += 1;
            self.ensure_not_eof("Expecting field after '.'")?;

            let field = self.parse_field_accessor()?;

            match expr {
                Expr::At(mut paths) => {
                    paths.push(Box::new(field));
                    expr = Expr::At(paths);
                }
                _ => {
                    expr = Expr::Access(Box::new(expr), Box::new(field));
                }
            }
        }

        Ok(expr)
    }

    /// - `nil`
    /// - `true`
    /// - `false`
    /// - `1`
    /// - `1.2`
    /// - `"str"`
    /// - `[...]`
    /// - `{...}`
    fn parse_primary(&mut self) -> Result<Expr> {
        if self.eof() {
            return Err(Error::Parse(self.err("Unexpected end of expression")));
        }

        let token = &self.tokens[self.pos];
        let expr = match token {
            Token::Nil => {
                self.pos += 1;
                Expr::Val(Val::Nil)
            }
            Token::Bool(b) => {
                self.pos += 1;
                Expr::Val(Val::Bool(*b))
            }
            Token::Int(i) => {
                self.pos += 1;
                Expr::Val(Val::Int(*i))
            }
            Token::Float(f) => {
                self.pos += 1;
                Expr::Val(Val::Float(*f))
            }
            Token::Str(s) => {
                self.pos += 1;
                Self::shared_str_expr(s)
            }
            Token::At => self.parse_at()?,
            Token::LBracket => self.parse_list()?,
            Token::LBrace => self.parse_map()?,
            _ => self.parse_paren()?,
        };
        Ok(expr)
    }

    /// - `(expr)`
    /// - `expr`
    fn parse_paren(&mut self) -> Result<Expr> {
        if self.consume_if(&Token::LParen) {
            Ok(Expr::Paren(Box::new(self.finish_parenthesized_expr()?)))
        } else {
            // This is where the recursion issue was - we need a terminal case
            match &self.tokens[self.pos] {
                Token::Id(id) => {
                    let expr = Self::shared_str_expr(id);
                    self.pos += 1;
                    Ok(expr)
                }
                _ => {
                    let msg = format!("Unexpected token: {:?}", self.tokens[self.pos]);
                    Err(Error::Parse(self.err(&msg)))
                }
            }
        }
    }

    /// Parse list literal: `[expr, expr, ...]`
    fn parse_list(&mut self) -> Result<Expr> {
        self.expect_punctuation(Token::LBracket, "[")?;
        let elements = self.parse_comma_separated(
            Token::RBracket,
            "]",
            "Invalid list element start",
            "Invalid list element after comma",
            Some(Self::invalid_list_separator_message),
            |parser| parser.parse_expr().map(Box::new),
        )?;
        Ok(Expr::List(elements))
    }

    /// Parse map literal: `{key: value, key: value, ...}`
    fn parse_map(&mut self) -> Result<Expr> {
        self.expect_punctuation(Token::LBrace, "{")?;
        let pairs = self.parse_comma_separated(
            Token::RBrace,
            "}",
            "Invalid map key start",
            "Invalid map key after comma",
            None,
            Self::parse_map_pair,
        )?;
        Ok(Expr::Map(pairs))
    }

    /// Parse a field accessor in an @ expression
    fn parse_field_accessor(&mut self) -> Result<Expr> {
        self.ensure_not_eof("Unexpected end of expression")?;

        match &self.tokens[self.pos] {
            Token::Id(id) => {
                let expr = Self::shared_str_expr(id);
                self.pos += 1;
                Ok(expr)
            }
            Token::Str(s) => {
                let expr = Self::shared_str_expr(s);
                self.pos += 1;
                Ok(expr)
            }
            Token::Int(i) => {
                let expr = Expr::Val(Val::Int(*i));
                self.pos += 1;
                Ok(expr)
            }
            Token::LParen => {
                self.pos += 1;
                self.finish_parenthesized_expr()
            }
            Token::At => self.parse_at(),
            _ => {
                let msg = format!("Unexpected token in field accessor: {:?}", self.tokens[self.pos]);
                Err(Error::Parse(self.err(&msg)))
            }
        }
    }

    /// - `@user.name`
    /// - `@user.emails.0.company`
    /// - `@user.subscribers.(@record.sender).name`
    /// - `@(1 + "1")`
    fn parse_at(&mut self) -> Result<Expr> {
        self.enter_nested("Context access nesting is too deep")?;

        let result = (|| {
            self.expect_punctuation(Token::At, "@")?;
            self.ensure_not_eof("Expecting field after '@'")?;

            // Pre-allocate a reasonable size for the paths vector
            let mut paths = Vec::with_capacity(4);

            while !self.eof() {
                // Check the first path must be Str
                if paths.is_empty() {
                    self.expect_at_path_start()?;
                }

                if paths.len() >= MAX_AT_PATH_SEGMENTS {
                    return Err(Error::Parse(self.err("Context access path is too deep")));
                }

                paths.push(Box::new(self.parse_field_accessor()?));

                if self.eof() || self.tokens[self.pos] != Token::Dot {
                    break;
                }
                self.pos += 1;
                self.ensure_not_eof("Expecting field after '.'")?;
            }

            Ok(Expr::At(paths))
        })();

        self.leave_nested();
        result
    }

    /// Check if the current token can start a valid expression
    fn is_valid_expr_start(&self) -> bool {
        if self.eof() {
            return false;
        }

        matches!(
            self.tokens[self.pos],
            Token::Nil
                | Token::Bool(_)
                | Token::Int(_)
                | Token::Float(_)
                | Token::Str(_)
                | Token::Id(_)
                | Token::At
                | Token::LBracket
                | Token::LBrace
                | Token::LParen
                | Token::Not
                | Token::Sub
        )
    }
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        let len = tokens.len();
        Self {
            tokens,
            pos: 0,
            len,
            depth: 0,
        }
    }

    fn eof(&self) -> bool {
        self.pos >= self.len
    }

    fn err(&self, msg: &str) -> String {
        let r_idx = if self.pos + 5 < self.len {
            self.pos + 5
        } else {
            self.len
        };
        let l_idx = self.pos.saturating_sub(5);
        let r_idx = if r_idx > self.len { self.len } else { r_idx };
        let chars = &self.tokens[l_idx..r_idx];
        let chars: Vec<_> = chars.iter().collect();
        let c = self.tokens.get(self.pos);
        let ctx = if let Some(c) = c {
            format!("'{:?}' at index {}, near '{:?}'", c, self.pos, chars)
        } else {
            format!("at end, near '{:?}'", chars)
        };
        format!("Syntax error: {} ({})", msg, ctx)
    }

    fn enter_nested(&mut self, msg: &str) -> Result<()> {
        if self.depth >= MAX_PARSE_DEPTH {
            return Err(Error::Parse(self.err(msg)));
        }
        self.depth += 1;
        Ok(())
    }

    fn leave_nested(&mut self) {
        self.depth = self.depth.saturating_sub(1);
    }
}
