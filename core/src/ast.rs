use crate::{
    expr::Expr,
    op::{BinOp, UnaryOp},
    token::Token,
    val::Val,
};
use anyhow::{Result, anyhow};
use std::sync::Arc;

pub struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
    len: usize,
    token_spans: Option<&'a [crate::error::Span]>,
}

impl<'a> Parser<'a> {
    pub fn parse(&mut self) -> Result<Expr> {
        if self.eof() {
            return Ok(Expr::Val(Val::Nil));
        }

        let exp = self.parse_expr()?;

        if !self.eof() {
            return Err(anyhow!(self.err("Unexpected tokens at end")));
        }

        // All sub-expressions parsed, apply constant folding optimization
        Ok(exp.fold_constants())
    }

    /// Parse with enhanced error information that includes position
    pub fn parse_with_enhanced_errors(
        &mut self,
        input: &str,
    ) -> std::result::Result<Expr, crate::error::ParseError> {
        if self.eof() {
            return Ok(Expr::Val(Val::Nil));
        }

        let exp = match self.parse_expr() {
            Ok(expr) => expr,
            Err(err) => {
                // Prefer precise token span if available; otherwise, fall back to offset estimation
                if let Some(spans) = &self.token_spans && self.pos < spans.len() {
                    return Err(crate::error::ParseError::with_span(
                        err.to_string(),
                        spans[self.pos].clone(),
                    ));
                }
                let position = crate::error::offset_to_position(
                    input,
                    if self.pos < self.tokens.len() && self.pos > 0 {
                        self.pos * input.len() / self.tokens.len().max(1)
                    } else {
                        input.len()
                    },
                );
                return Err(crate::error::ParseError::with_position(
                    err.to_string(),
                    position,
                ));
            }
        };

        if !self.eof() {
            if let Some(spans) = &self.token_spans && self.pos < spans.len() {
                return Err(crate::error::ParseError::with_span(
                    "Unexpected tokens at end".to_string(),
                    spans[self.pos].clone(),
                ));
            }
            let position = crate::error::offset_to_position(
                input,
                if self.pos < self.tokens.len() {
                    self.pos * input.len() / self.tokens.len().max(1)
                } else {
                    input.len()
                },
            );
            return Err(crate::error::ParseError::with_position(
                "Unexpected tokens at end".to_string(),
                position,
            ));
        }

        // All sub-expressions parsed, apply constant folding optimization
        Ok(exp.fold_constants())
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        self.parse_or()
    }

    /// - `expr || expr`
    fn parse_or(&mut self) -> Result<Expr> {
        let mut expr = self.parse_and()?;
        while !self.eof() {
            match self.tokens[self.pos] {
                Token::Or => {
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
        while !self.eof() {
            match self.tokens[self.pos] {
                Token::And => {
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
        while !self.eof() {
            let op = match self.tokens[self.pos] {
                Token::Add => BinOp::Add,
                Token::Sub => BinOp::Sub,
                _ => break,
            };
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
        while !self.eof() {
            let op = match self.tokens[self.pos] {
                Token::Mul => BinOp::Mul,
                Token::Div => BinOp::Div,
                Token::Mod => BinOp::Mod,
                _ => break,
            };
            self.pos += 1;
            let right = self.parse_unary()?;
            expr = Expr::Bin(Box::new(expr), op, Box::new(right));
        }
        Ok(expr)
    }

    /// - `!expr`
    /// - `expr`
    fn parse_unary(&mut self) -> Result<Expr> {
        if self.eof() {
            return Err(anyhow!(self.err("Expected expression")));
        }
        let token = &self.tokens[self.pos];
        match token {
            Token::Not => {
                self.pos += 1;
                let expr = self.parse_unary()?;
                Ok(Expr::Unary(UnaryOp::Not, Box::new(expr)))
            }
            _ => self.parse_postfix(),
        }
    }

    /// - `primary`
    /// - `primary.field`
    /// - `primary.field.field`
    /// - `func_name(args)`
    fn parse_postfix(&mut self) -> Result<Expr> {
        let mut expr = self.parse_primary()?;

        loop {
            if !self.eof() && self.tokens[self.pos] == Token::LParen {
                // Function call
                self.pos += 1; // skip '('

                let mut args = Vec::new();

                // Parse arguments
                while !self.eof() && self.tokens[self.pos] != Token::RParen {
                    args.push(Box::new(self.parse_expr()?));

                    if !self.eof() && self.tokens[self.pos] == Token::Comma {
                        self.pos += 1;
                    } else if self.tokens[self.pos] != Token::RParen {
                        return Err(anyhow!(self.err("Expected ',' or ')' in function call")));
                    }
                }

                if self.eof() || self.tokens[self.pos] != Token::RParen {
                    return Err(anyhow!(self.err("Expected ')' to close function call")));
                }
                self.pos += 1; // skip ')'

                expr = Expr::CallExpr(Box::new(expr), args);
            } else if !self.eof() && self.tokens[self.pos] == Token::Dot {
                // Dot access
                self.pos += 1;

                if self.eof() {
                    return Err(anyhow!(self.err("Expecting field after '.'")));
                }

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
            } else {
                break; // No more postfix operations
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
            return Err(anyhow!(self.err("Unexpected end of input")));
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
                Expr::Val(Val::Str(Arc::from(s.as_str())))
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
        if self.tokens[self.pos] == Token::LParen {
            self.pos += 1;
            let expr = self.parse_expr()?;
            if self.eof() || self.tokens[self.pos] != Token::RParen {
                let msg = format!(
                    "Expecting ')', found {:?}",
                    if self.eof() {
                        &Token::Nil
                    } else {
                        &self.tokens[self.pos]
                    }
                );
                return Err(anyhow!(self.err(&msg)));
            }
            self.pos += 1;
            Ok(Expr::Paren(Box::new(expr)))
        } else {
            match &self.tokens[self.pos] {
                Token::Id(id) => {
                    let expr = Expr::Var(id.clone());
                    self.pos += 1;
                    Ok(expr)
                }
                _ => {
                    let msg = format!("Unexpected token: {:?}", self.tokens[self.pos]);
                    Err(anyhow!(self.err(&msg)))
                }
            }
        }
    }

    /// Parse list literal: `[expr, expr, ...]`
    fn parse_list(&mut self) -> Result<Expr> {
        if self.tokens[self.pos] != Token::LBracket {
            let msg = format!("Expecting '[', found {:?}", self.tokens[self.pos]);
            return Err(anyhow!(self.err(&msg)));
        }
        self.pos += 1;

        let mut elements = Vec::new();

        // Handle empty list
        if !self.eof() && self.tokens[self.pos] == Token::RBracket {
            self.pos += 1;
            return Ok(Expr::List(elements));
        }

        // Parse first element
        if !self.eof() {
            if !self.is_valid_expr_start() {
                let msg = format!("Invalid list element start: {:?}", self.tokens[self.pos]);
                return Err(anyhow!(self.err(&msg)));
            }

            elements.push(Box::new(self.parse_expr()?));

            // Parse remaining elements
            while !self.eof() {
                match self.tokens[self.pos] {
                    Token::Comma => {
                        self.pos += 1;
                        // Handle trailing comma
                        if !self.eof() && self.tokens[self.pos] == Token::RBracket {
                            break;
                        }

                        if self.eof() || !self.is_valid_expr_start() {
                            let msg = format!(
                                "Invalid list element after comma: {:?}",
                                if self.eof() {
                                    &Token::Nil
                                } else {
                                    &self.tokens[self.pos]
                                }
                            );
                            return Err(anyhow!(self.err(&msg)));
                        }

                        elements.push(Box::new(self.parse_expr()?));
                    }
                    Token::RBracket => break,
                    _ => {
                        let msg = if self.is_invalid_separator() {
                            format!(
                                "Invalid separator in list: {:?}. Use ',' to separate elements",
                                self.tokens[self.pos]
                            )
                        } else {
                            format!("Expecting ',' or ']', found {:?}", self.tokens[self.pos])
                        };
                        return Err(anyhow!(self.err(&msg)));
                    }
                }
            }
        }

        if self.eof() || self.tokens[self.pos] != Token::RBracket {
            let msg = format!(
                "Expecting ']', found {:?}",
                if self.eof() {
                    &Token::Nil
                } else {
                    &self.tokens[self.pos]
                }
            );
            return Err(anyhow!(self.err(&msg)));
        }
        self.pos += 1;

        Ok(Expr::List(elements))
    }

    /// Parse map literal: `{key: value, key: value, ...}`
    fn parse_map(&mut self) -> Result<Expr> {
        if self.tokens[self.pos] != Token::LBrace {
            let msg = format!("Expecting '{{', found {:?}", self.tokens[self.pos]);
            return Err(anyhow!(self.err(&msg)));
        }
        self.pos += 1;

        let mut pairs = Vec::new();

        // Handle empty map
        if !self.eof() && self.tokens[self.pos] == Token::RBrace {
            self.pos += 1;
            return Ok(Expr::Map(pairs));
        }

        // Parse first key-value pair
        if !self.eof() {
            if !self.is_valid_expr_start() {
                let msg = format!("Invalid map key start: {:?}", self.tokens[self.pos]);
                return Err(anyhow!(self.err(&msg)));
            }

            let key = Box::new(self.parse_expr()?);

            if self.eof() || self.tokens[self.pos] != Token::Colon {
                let msg = format!(
                    "Expecting ':', found {:?}",
                    if self.eof() {
                        &Token::Nil
                    } else {
                        &self.tokens[self.pos]
                    }
                );
                return Err(anyhow!(self.err(&msg)));
            }
            self.pos += 1;

            if self.eof() || !self.is_valid_expr_start() {
                let msg = format!(
                    "Invalid map value after ':', {:?}",
                    if self.eof() {
                        &Token::Nil
                    } else {
                        &self.tokens[self.pos]
                    }
                );
                return Err(anyhow!(self.err(&msg)));
            }

            let value = Box::new(self.parse_expr()?);
            pairs.push((key, value));

            // Parse remaining pairs
            while !self.eof() {
                match self.tokens[self.pos] {
                    Token::Comma => {
                        self.pos += 1;
                        // Handle trailing comma
                        if !self.eof() && self.tokens[self.pos] == Token::RBrace {
                            break;
                        }

                        if self.eof() || !self.is_valid_expr_start() {
                            let msg = format!(
                                "Invalid map key after comma: {:?}",
                                if self.eof() {
                                    &Token::Nil
                                } else {
                                    &self.tokens[self.pos]
                                }
                            );
                            return Err(anyhow!(self.err(&msg)));
                        }

                        let key = Box::new(self.parse_expr()?);

                        if self.eof() || self.tokens[self.pos] != Token::Colon {
                            let msg = format!(
                                "Expecting ':', found {:?}",
                                if self.eof() {
                                    &Token::Nil
                                } else {
                                    &self.tokens[self.pos]
                                }
                            );
                            return Err(anyhow!(self.err(&msg)));
                        }
                        self.pos += 1;

                        if self.eof() || !self.is_valid_expr_start() {
                            let msg = format!(
                                "Invalid map value after ':', {:?}",
                                if self.eof() {
                                    &Token::Nil
                                } else {
                                    &self.tokens[self.pos]
                                }
                            );
                            return Err(anyhow!(self.err(&msg)));
                        }

                        let value = Box::new(self.parse_expr()?);
                        pairs.push((key, value));
                    }
                    Token::RBrace => break,
                    _ => {
                        let msg =
                            format!("Expecting ',' or '}}', found {:?}", self.tokens[self.pos]);
                        return Err(anyhow!(self.err(&msg)));
                    }
                }
            }
        }

        if self.eof() || self.tokens[self.pos] != Token::RBrace {
            let msg = format!(
                "Expecting '}}', found {:?}",
                if self.eof() {
                    &Token::Nil
                } else {
                    &self.tokens[self.pos]
                }
            );
            return Err(anyhow!(self.err(&msg)));
        }
        self.pos += 1;

        Ok(Expr::Map(pairs))
    }

    /// Parse a field accessor in an @ expression
    fn parse_field_accessor(&mut self) -> Result<Expr> {
        match &self.tokens[self.pos] {
            Token::Id(id) => {
                let expr = Expr::Val(Val::Str(Arc::from(id.as_str())));
                self.pos += 1;
                Ok(expr)
            }
            Token::Str(s) => {
                let expr = Expr::Val(Val::Str(Arc::from(s.as_str())));
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
                let expr = self.parse_expr()?;
                if self.eof() || self.tokens[self.pos] != Token::RParen {
                    let msg = format!(
                        "Expecting ')', found {:?}",
                        if self.eof() {
                            &Token::Nil
                        } else {
                            &self.tokens[self.pos]
                        }
                    );
                    return Err(anyhow!(self.err(&msg)));
                }
                self.pos += 1;
                Ok(expr)
            }
            Token::At => self.parse_at(),
            _ => {
                let msg = format!(
                    "Unexpected token in field accessor: {:?}",
                    self.tokens[self.pos]
                );
                Err(anyhow!(self.err(&msg)))
            }
        }
    }

    /// - `@user.name`
    /// - `@user.emails.0.company`
    /// - `@user.subscribers.(@record.sender).name`
    /// - `@(1 + "1")`
    fn parse_at(&mut self) -> Result<Expr> {
        if self.tokens[self.pos] != Token::At {
            let msg = format!("Expecting @, found {:?}", self.tokens[self.pos]);
            return Err(anyhow!(self.err(&msg)));
        }
        self.pos += 1;

        if self.eof() {
            return Err(anyhow!(self.err("Expecting field after '@'")));
        }

        // Pre-allocate a reasonable size for the paths vector
        let mut paths = Vec::with_capacity(4);

        while !self.eof() {
            // Check the first path must be Str
            if paths.is_empty() {
                let first = &self.tokens[self.pos];
                match first {
                    Token::Id(_) | Token::LParen | Token::Str(_) => {}
                    _ => {
                        let msg = format!("Expecting field name, found {:?}", first);
                        return Err(anyhow!(self.err(&msg)));
                    }
                }
            }

            paths.push(Box::new(self.parse_field_accessor()?));

            if self.eof() || self.tokens[self.pos] != Token::Dot {
                break;
            }
            self.pos += 1;
        }

        Ok(Expr::At(paths))
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
        )
    }

    /// Check if the current token is an invalid separator
    fn is_invalid_separator(&self) -> bool {
        if self.eof() {
            return false;
        }

        matches!(self.tokens[self.pos], Token::Semicolon)
    }

    /// Recovering expression analysis: collect multiple parse errors across expression segments
    /// without building a final AST. Uses shallow segmentation on common boundaries to surface
    /// multiple issues within a single line/chunk.
    pub fn recover_expression_errors(
        tokens: &'a [Token],
        spans: &'a [crate::error::Span],
        input: &str,
    ) -> Vec<crate::error::ParseError> {
        let mut errors = Vec::new();
        let len = tokens.len();
        let mut i = 0usize;

        // Track depth for (), [], {} to decide boundaries at depth 0
        let mut paren: i32;
        let mut bracket: i32;
        let mut brace: i32;

        fn is_hard_boundary(tok: &Token) -> bool {
            matches!(
                tok,
                Token::Comma
                    | Token::Semicolon
                    | Token::RParen
                    | Token::RBracket
                    | Token::RBrace
                    | Token::Else
            )
        }

        fn is_soft_boundary(tok: &Token) -> bool {
            matches!(
                tok,
                Token::Eq
                    | Token::Ne
                    | Token::Gt
                    | Token::Lt
                    | Token::Ge
                    | Token::Le
                    | Token::In
                    | Token::And
                    | Token::Or
            )
        }

        while i < len {
            // Skip immediate boundaries to avoid empty segments
            while i < len && is_hard_boundary(&tokens[i]) {
                i += 1;
            }
            if i >= len {
                break;
            }

            // Determine a segment [i, j)
            let seg_start = i;
            let mut j = i;
            paren = 0;
            bracket = 0;
            brace = 0;
            while j < len {
                match &tokens[j] {
                    Token::LParen => {
                        paren += 1;
                        j += 1;
                    }
                    Token::RParen => {
                        if paren > 0 {
                            paren -= 1;
                        }
                        if paren == 0 && bracket == 0 && brace == 0 {
                            j += 1;
                            break;
                        }
                        j += 1;
                    }
                    Token::LBracket => {
                        bracket += 1;
                        j += 1;
                    }
                    Token::RBracket => {
                        if bracket > 0 {
                            bracket -= 1;
                        }
                        if paren == 0 && bracket == 0 && brace == 0 {
                            j += 1;
                            break;
                        }
                        j += 1;
                    }
                    Token::LBrace => {
                        brace += 1;
                        j += 1;
                    }
                    Token::RBrace => {
                        if brace > 0 {
                            brace -= 1;
                        }
                        if paren == 0 && bracket == 0 && brace == 0 {
                            break;
                        }
                        j += 1;
                    }
                    t if is_hard_boundary(t) => {
                        break;
                    }
                    t if is_soft_boundary(t) && paren == 0 && bracket == 0 && brace == 0 => {
                        break;
                    }
                    _ => {
                        j += 1;
                    }
                }
            }
            if j == seg_start {
                i = j + 1;
                continue;
            }

            // Attempt to parse the segment
            let seg_tokens = &tokens[seg_start..j];
            let seg_spans = &spans[seg_start..j];
            if !seg_tokens.is_empty() {
                let mut p = Parser::new_with_spans(seg_tokens, seg_spans);
                match p.parse_with_enhanced_errors(input) {
                    Ok(_) => {}
                    Err(e) => errors.push(e),
                }
            }

            // Advance to next segment; if current position is at a soft boundary, skip it
            i = j;
            if i < len && (is_soft_boundary(&tokens[i]) || is_hard_boundary(&tokens[i])) {
                i += 1;
            }
        }

        errors
    }
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        let len = tokens.len();
        Self {
            tokens,
            pos: 0,
            len,
            token_spans: None,
        }
    }

    /// Create a parser with token spans for precise error reporting
    pub fn new_with_spans(tokens: &'a [Token], spans: &'a [crate::error::Span]) -> Self {
        let len = tokens.len();
        Self {
            tokens,
            pos: 0,
            len,
            token_spans: Some(spans),
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
}
