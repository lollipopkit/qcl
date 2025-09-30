use crate::{
    ast::Parser as ExprParser,
    expr::Expr,
    op::BinOp,
    stmt::{ForPattern, Program, Stmt},
    stmt::{ImportItem, ImportSource, ImportStmt},
    token::{ParseError, Position, Span, Token, offset_to_position},
    val::Type,
};
use anyhow::{Result, anyhow};

/// Statement Parser - 解析语句的解析器
pub struct StmtParser<'a> {
    tokens: &'a [Token],
    pos: usize,
    len: usize,
    token_spans: Option<&'a [Span]>,
}

impl<'a> StmtParser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        let len = tokens.len();
        Self {
            tokens,
            pos: 0,
            len,
            token_spans: None,
        }
    }

    /// Create a statement parser with token spans for precise error reporting
    pub fn new_with_spans(tokens: &'a [Token], spans: &'a [Span]) -> Self {
        let len = tokens.len();
        Self {
            tokens,
            pos: 0,
            len,
            token_spans: Some(spans),
        }
    }

    /// 解析整个程序
    pub fn parse_program(&mut self) -> Result<Program> {
        let mut statements = Vec::new();

        while !self.eof() {
            // 跳过空语句
            if self.tokens[self.pos] == Token::Semicolon {
                statements.push(Box::new(Stmt::Empty));
                self.pos += 1;
                continue;
            }

            statements.push(Box::new(self.parse_statement()?));
        }

        Program::new(statements)
    }

    /// Parse program with enhanced error reporting
    pub fn parse_program_with_enhanced_errors(&mut self, input: &str) -> std::result::Result<Program, ParseError> {
        let mut statements = Vec::new();

        while !self.eof() {
            // 跳过空语句
            if self.tokens[self.pos] == Token::Semicolon {
                statements.push(Box::new(Stmt::Empty));
                self.pos += 1;
                continue;
            }

            let stmt = match self.parse_statement() {
                Ok(s) => s,
                Err(err) => {
                    // Prefer precise token span if available; otherwise, fall back to offset estimation
                    if let Some(spans) = &self.token_spans
                        && self.pos < spans.len()
                    {
                        return Err(ParseError::with_span(err.to_string(), spans[self.pos].clone()));
                    }
                    let position = offset_to_position(
                        input,
                        if self.pos < self.tokens.len() && self.pos > 0 {
                            self.pos * input.len() / self.tokens.len().max(1)
                        } else {
                            input.len()
                        },
                    );
                    return Err(ParseError::with_position(err.to_string(), position));
                }
            };
            statements.push(Box::new(stmt));
        }

        Program::new(statements).map_err(|e| {
            // If we have more tokens at current position, use its span; otherwise fallback to start
            if let Some(spans) = &self.token_spans
                && self.pos < spans.len()
            {
                return ParseError::with_span(e.to_string(), spans[self.pos].clone());
            }
            ParseError::with_position(
                e.to_string(),
                Position {
                    line: 0,
                    column: 0,
                    offset: 0,
                },
            )
        })
    }

    /// Recovering parse: continue after errors using simple synchronization points to collect multiple errors.
    /// Returns a flat list of statements (without label map validation) and a list of parse errors with spans.
    pub fn parse_program_recovering_with_enhanced_errors(&mut self, input: &str) -> (Vec<Box<Stmt>>, Vec<ParseError>) {
        let mut statements = Vec::new();
        let mut errors = Vec::new();

        while !self.eof() {
            // Skip standalone semicolons
            if self.tokens[self.pos] == Token::Semicolon {
                statements.push(Box::new(Stmt::Empty));
                self.pos += 1;
                continue;
            }

            match self.parse_statement() {
                Ok(stmt) => statements.push(Box::new(stmt)),
                Err(err) => {
                    // Build precise error using token span if possible
                    let parse_err = if let Some(spans) = &self.token_spans {
                        let span = if self.pos < spans.len() {
                            spans[self.pos].clone()
                        } else if !spans.is_empty() {
                            // Fallback to last known span
                            spans[spans.len() - 1].clone()
                        } else {
                            // Ultimate fallback to end-of-input position
                            let pos = offset_to_position(input, input.len());
                            Span::single(pos)
                        };
                        ParseError::with_span(err.to_string(), span)
                    } else {
                        // Estimate position if spans unavailable
                        let position = offset_to_position(
                            input,
                            if self.pos < self.tokens.len() && self.pos > 0 {
                                self.pos * input.len() / self.tokens.len().max(1)
                            } else {
                                input.len()
                            },
                        );
                        ParseError::with_position(err.to_string(), position)
                    };
                    errors.push(parse_err);

                    // Error recovery: advance to next sync point to avoid infinite loop.
                    // Sync when encountering a ';' at depth 0 (consume it) or an '}' that likely closes the current block.
                    // Track simple nesting for (), [], {} to avoid syncing mid-expression.
                    if !self.eof() {
                        // Ensure we always advance at least one token to make progress
                        self.pos = (self.pos + 1).min(self.len);
                    }
                    let mut paren: i32 = 0;
                    let mut bracket: i32 = 0;
                    let mut brace: i32 = 0;
                    let mut seen_block: bool = false;
                    while !self.eof() {
                        match self.tokens[self.pos] {
                            Token::LParen => {
                                paren += 1;
                                self.pos += 1;
                            }
                            Token::RParen => {
                                if paren > 0 {
                                    paren -= 1;
                                }
                                self.pos += 1;
                            }
                            Token::LBracket => {
                                bracket += 1;
                                self.pos += 1;
                            }
                            Token::RBracket => {
                                if bracket > 0 {
                                    bracket -= 1;
                                }
                                self.pos += 1;
                            }
                            Token::LBrace => {
                                brace += 1;
                                seen_block = true;
                                self.pos += 1;
                            }
                            Token::RBrace => {
                                // If we have seen a block start and this '}' closes it (brace would go from 1->0),
                                // break here to avoid skipping the following statement.
                                if seen_block && brace == 1 && paren == 0 && bracket == 0 {
                                    self.pos += 1; // consume '}'
                                    break;
                                }
                                if brace > 0 {
                                    brace -= 1;
                                }
                                self.pos += 1;
                            }
                            Token::Semicolon => {
                                if paren == 0 && bracket == 0 && brace == 0 {
                                    self.pos += 1; // consume ';'
                                    break;
                                } else {
                                    self.pos += 1;
                                }
                            }
                            _ => {
                                self.pos += 1;
                            }
                        }
                    }
                }
            }
        }

        (statements, errors)
    }

    /// 解析单个语句
    pub fn parse_statement(&mut self) -> Result<Stmt> {
        if self.eof() {
            return Ok(Stmt::Empty);
        }

        match &self.tokens[self.pos] {
            Token::Import => self.parse_import_stmt(),
            Token::If => self.parse_if_stmt(),
            Token::While => self.parse_while_stmt(),
            Token::For => self.parse_for_stmt(),
            Token::Let => self.parse_let_stmt(),
            Token::Break => self.parse_break_stmt(),
            Token::Continue => self.parse_continue_stmt(),
            Token::Return => self.parse_return_stmt(),
            Token::Fn => self.parse_function_stmt(),
            Token::LBrace => self.parse_block_stmt(),
            Token::Id(id) => {
                // 优先解析短声明 `id := expr` 以避免与标签 `id:` 冲突
                if self.peek_ahead(1) == Some(&Token::Colon) && self.peek_ahead(2) == Some(&Token::Assign) {
                    self.parse_define_stmt_with_id(id.clone())
                } else if self.peek_ahead(1) == Some(&Token::Assign) {
                    // 赋值 (id = expr;)
                    self.parse_assign_stmt_with_id(id.clone())
                } else if matches!(
                    self.peek_ahead(1),
                    Some(&Token::AddAssign)
                        | Some(&Token::SubAssign)
                        | Some(&Token::MulAssign)
                        | Some(&Token::DivAssign)
                        | Some(&Token::ModAssign)
                ) {
                    // 复合赋值 (id += expr;)
                    self.parse_compound_assign_stmt_with_id(id.clone())
                } else {
                    // 作为表达式语句处理
                    self.parse_expr_stmt()
                }
            }
            _ => self.parse_expr_stmt(), // 默认作为表达式语句处理
        }
    }

    /// 解析 if 语句
    fn parse_if_stmt(&mut self) -> Result<Stmt> {
        self.expect_token(Token::If)?;

        // Check if this is an "if let" statement
        if !self.eof() && self.tokens[self.pos] == Token::Let {
            self.pos += 1; // consume 'let'

            // Parse the pattern
            let pattern = self.parse_pattern()?;

            // Expect '='
            self.expect_token(Token::Assign)?;

            // Parse the value expression (stop at LBrace for if let body)
            let value = self.parse_expression_with_options(true)?;

            // Parse then statement (no parentheses for if let)
            let then_stmt = Box::new(self.parse_statement()?);

            // Parse optional else statement
            let else_stmt = if !self.eof() && self.tokens[self.pos] == Token::Else {
                self.pos += 1;
                Some(Box::new(self.parse_statement()?))
            } else {
                None
            };

            Ok(Stmt::IfLet {
                pattern,
                value: Box::new(value),
                then_stmt,
                else_stmt,
            })
        } else {
            // Regular if statement
            let condition = if !self.eof() && self.tokens[self.pos] == Token::LParen {
                // Standard form: if (cond) stmt
                self.pos += 1; // consume '('
                let cond = self.parse_expression()?;
                self.expect_token(Token::RParen)?;
                cond
            } else {
                // Also support: if cond { ... } (without parentheses)
                // Stop parsing the condition at '{' when at top-level
                self.parse_expression_with_options(true)?
            };

            let then_stmt = Box::new(self.parse_statement()?);

            let else_stmt = if !self.eof() && self.tokens[self.pos] == Token::Else {
                self.pos += 1;
                Some(Box::new(self.parse_statement()?))
            } else {
                None
            };

            Ok(Stmt::If {
                condition: Box::new(condition),
                then_stmt,
                else_stmt,
            })
        }
    }

    /// 解析 while 语句
    fn parse_while_stmt(&mut self) -> Result<Stmt> {
        self.expect_token(Token::While)?;

        // Check if this is a "while let" statement
        if !self.eof() && self.tokens[self.pos] == Token::Let {
            self.pos += 1; // consume 'let'

            // Parse the pattern
            let pattern = self.parse_pattern()?;

            // Expect '='
            self.expect_token(Token::Assign)?;

            // Parse the value expression (stop at LBrace for while let body)
            let value = self.parse_expression_with_options(true)?;

            // Parse body statement (no parentheses for while let)
            let body = Box::new(self.parse_statement()?);

            Ok(Stmt::WhileLet {
                pattern,
                value: Box::new(value),
                body,
            })
        } else {
            // Regular while statement
            self.expect_token(Token::LParen)?;

            let condition = self.parse_expression()?;

            self.expect_token(Token::RParen)?;
            let body = Box::new(self.parse_statement()?);

            Ok(Stmt::While {
                condition: Box::new(condition),
                body,
            })
        }
    }

    /// 解析 for 语句
    fn parse_for_stmt(&mut self) -> Result<Stmt> {
        self.expect_token(Token::For)?; // 消费 'for'

        // 解析模式 (变量名或解构)
        let pattern = self.parse_for_pattern()?;

        self.expect_token(Token::In)?; // 消费 'in'

        // 解析可迭代表达式 - 在for循环中遇到LBrace时停止
        let iterable = self.parse_expression_with_options(true)?;

        // 解析循环体
        let body = Box::new(self.parse_statement()?);

        Ok(Stmt::For {
            pattern,
            iterable: Box::new(iterable),
            body,
        })
    }

    /// 解析 for 循环的模式
    fn parse_for_pattern(&mut self) -> Result<ForPattern> {
        match &self.tokens[self.pos] {
            // 忽略模式: _
            Token::Id(name) if name == "_" => {
                self.pos += 1;
                Ok(ForPattern::Ignore)
            }
            // 简单变量: identifier
            Token::Id(name) => {
                let var_name = name.clone();
                self.pos += 1;
                Ok(ForPattern::Variable(var_name))
            }
            // 元组模式: (a, b, c)
            Token::LParen => {
                self.pos += 1; // 消费 '('
                let mut patterns = Vec::new();

                // 处理空元组 ()
                if !self.eof() && self.tokens[self.pos] == Token::RParen {
                    self.pos += 1;
                    return Ok(ForPattern::Tuple(patterns));
                }

                loop {
                    patterns.push(self.parse_for_pattern()?);

                    if self.eof() {
                        return Err(anyhow!(self.err("Expected ')' in tuple pattern")));
                    }

                    match &self.tokens[self.pos] {
                        Token::Comma => {
                            self.pos += 1; // 消费 ','
                            // 允许尾随逗号: (a, b,)
                            if !self.eof() && self.tokens[self.pos] == Token::RParen {
                                break;
                            }
                            continue;
                        }
                        Token::RParen => break,
                        _ => return Err(anyhow!(self.err("Expected ',' or ')' in tuple pattern"))),
                    }
                }

                self.pos += 1; // 消费 ')'
                Ok(ForPattern::Tuple(patterns))
            }
            // 数组模式: [a, b] 或 [a, b, ..rest]
            Token::LBracket => {
                self.pos += 1; // 消费 '['
                let mut patterns = Vec::new();
                let mut rest = None;

                // 处理空数组 []
                if !self.eof() && self.tokens[self.pos] == Token::RBracket {
                    self.pos += 1;
                    return Ok(ForPattern::Array { patterns, rest });
                }

                loop {
                    // 检查剩余模式 ..
                    if !self.eof() && self.tokens[self.pos] == Token::Range {
                        self.pos += 1; // 消费 '..'

                        // 可选的剩余变量名
                        if !self.eof()
                            && let Token::Id(name) = &self.tokens[self.pos]
                        {
                            rest = Some(name.clone());
                            self.pos += 1;
                        }

                        // 剩余模式后不能再有其他模式
                        if self.eof() {
                            return Err(anyhow!(self.err("Expected ']' after rest pattern")));
                        }

                        match &self.tokens[self.pos] {
                            Token::RBracket => break,
                            Token::Comma => {
                                self.pos += 1;
                                if !self.eof() && self.tokens[self.pos] == Token::RBracket {
                                    break;
                                } else {
                                    return Err(anyhow!(self.err("No patterns allowed after rest pattern")));
                                }
                            }
                            _ => {
                                return Err(anyhow!(self.err("Expected ']' or ',' after rest pattern")));
                            }
                        }
                    } else {
                        patterns.push(self.parse_for_pattern()?);
                    }

                    if self.eof() {
                        return Err(anyhow!(self.err("Expected ']' in array pattern")));
                    }

                    match &self.tokens[self.pos] {
                        Token::Comma => {
                            self.pos += 1; // 消费 ','
                            // 允许尾随逗号: [a, b,]
                            if !self.eof() && self.tokens[self.pos] == Token::RBracket {
                                break;
                            }
                            continue;
                        }
                        Token::RBracket => break,
                        _ => return Err(anyhow!(self.err("Expected ',' or ']' in array pattern"))),
                    }
                }

                self.pos += 1; // 消费 ']'
                Ok(ForPattern::Array { patterns, rest })
            }
            // 对象模式: {"k1": v1, "k2": v2}
            Token::LBrace => {
                self.pos += 1; // 消费 '{'
                let mut entries: Vec<(String, ForPattern)> = Vec::new();

                // 处理空对象 {}
                if !self.eof() && self.tokens[self.pos] == Token::RBrace {
                    self.pos += 1;
                    return Ok(ForPattern::Object(entries));
                }

                loop {
                    if self.eof() {
                        return Err(anyhow!(self.err("Expected string key in object pattern")));
                    }

                    // 键必须是字符串字面量
                    let key = if let Token::Str(s) = &self.tokens[self.pos] {
                        let k = s.clone();
                        self.pos += 1;
                        k
                    } else {
                        return Err(anyhow!(self.err("Expected string key in object pattern")));
                    };

                    // 冒号
                    self.expect_token(Token::Colon)?;

                    // 值部分可以是任意 for 模式（变量、_、元组、数组、嵌套对象等）
                    let value_pattern = self.parse_for_pattern()?;

                    entries.push((key, value_pattern));

                    if self.eof() {
                        return Err(anyhow!(self.err("Expected '}' in object pattern")));
                    }

                    match &self.tokens[self.pos] {
                        Token::Comma => {
                            self.pos += 1; // 继续解析下一个键值
                            // 允许尾随逗号
                            if !self.eof() && self.tokens[self.pos] == Token::RBrace {
                                break;
                            }
                            continue;
                        }
                        Token::RBrace => break,
                        _ => {
                            return Err(anyhow!(self.err("Expected ',' or '}' in object pattern")));
                        }
                    }
                }

                self.pos += 1; // 消费 '}'
                Ok(ForPattern::Object(entries))
            }
            _ => Err(anyhow!(self.err("Expected pattern after 'for'"))),
        }
    }

    /// 解析 let 语句
    fn parse_let_stmt(&mut self) -> Result<Stmt> {
        self.expect_token(Token::Let)?;

        // Parse pattern for let statement until a top-level ':' (type annotation)
        // or '=' (assignment). Do NOT stop on ':' inside nested structures.
        let start_pos = self.pos;
        let mut end_pos = start_pos;
        let mut paren: i32 = 0;
        let mut bracket: i32 = 0;
        let mut brace: i32 = 0;

        while end_pos < self.len {
            match &self.tokens[end_pos] {
                Token::LParen => {
                    paren += 1;
                    end_pos += 1;
                }
                Token::RParen => {
                    if paren > 0 {
                        paren -= 1;
                    }
                    end_pos += 1;
                }
                Token::LBracket => {
                    bracket += 1;
                    end_pos += 1;
                }
                Token::RBracket => {
                    if bracket > 0 {
                        bracket -= 1;
                    }
                    end_pos += 1;
                }
                Token::LBrace => {
                    brace += 1;
                    end_pos += 1;
                }
                Token::RBrace => {
                    if brace > 0 {
                        brace -= 1;
                    }
                    end_pos += 1;
                }
                Token::Assign if paren == 0 && bracket == 0 && brace == 0 => {
                    break;
                }
                Token::Colon if paren == 0 && bracket == 0 && brace == 0 => {
                    break;
                }
                _ => {
                    end_pos += 1;
                }
            }
        }

        if end_pos == start_pos {
            return Err(anyhow!(self.err("Expected pattern after 'let'")));
        }

        // Use AST parser to parse the pattern
        let pattern_tokens = &self.tokens[start_pos..end_pos];
        let mut ast_parser = ExprParser::new(pattern_tokens);
        let pattern = ast_parser.parse_pattern()?;

        // Update position
        self.pos = end_pos;

        // Optional type annotation at top-level
        let type_annotation = if !self.eof() && self.tokens[self.pos] == Token::Colon {
            self.pos += 1; // consume ':'
            Some(self.parse_type_annotation()?)
        } else {
            None
        };

        self.expect_token(Token::Assign)?;

        let value = self.parse_expression()?;
        self.expect_token(Token::Semicolon)?;

        Ok(Stmt::Let {
            pattern,
            type_annotation,
            value: Box::new(value),
            span: self.current_span(),
        })
    }

    /// 解析赋值语句（已匹配标识符）
    fn parse_assign_stmt_with_id(&mut self, name: String) -> Result<Stmt> {
        // 我们已经在parse_statement中匹配了Id，现在跳过它并继续解析赋值
        self.pos += 1; // 跳过已匹配的 Id token
        self.expect_token(Token::Assign)?;

        let value = self.parse_expression()?;
        self.expect_token(Token::Semicolon)?;

        Ok(Stmt::Assign {
            name,
            value: Box::new(value),
            span: self.current_span(),
        })
    }

    /// 解析复合赋值语句: id += expr;
    fn parse_compound_assign_stmt_with_id(&mut self, name: String) -> Result<Stmt> {
        // 我们已经在parse_statement中匹配了Id，现在跳过它并继续解析复合赋值
        self.pos += 1; // 跳过已匹配的 Id token

        // 获取复合赋值操作符
        let op = match &self.tokens[self.pos] {
            Token::AddAssign => BinOp::Add,
            Token::SubAssign => BinOp::Sub,
            Token::MulAssign => BinOp::Mul,
            Token::DivAssign => BinOp::Div,
            Token::ModAssign => BinOp::Mod,
            _ => return Err(anyhow!("Expected compound assignment operator")),
        };
        self.pos += 1; // 跳过复合赋值操作符

        let value = self.parse_expression()?;
        self.expect_token(Token::Semicolon)?;

        Ok(Stmt::CompoundAssign {
            name,
            op,
            value: Box::new(value),
            span: self.current_span(),
        })
    }

    /// 解析短声明语句: id := expr;
    fn parse_define_stmt_with_id(&mut self, name: String) -> Result<Stmt> {
        // consume Id (already peeked), ':' and '='
        self.pos += 1; // Id
        self.expect_token(Token::Colon)?;
        self.expect_token(Token::Assign)?;

        let value = self.parse_expression()?;
        self.expect_token(Token::Semicolon)?;
        Ok(Stmt::Define {
            name,
            value: Box::new(value),
        })
    }

    /// 解析 break 语句
    fn parse_break_stmt(&mut self) -> Result<Stmt> {
        self.expect_token(Token::Break)?;
        self.expect_token(Token::Semicolon)?;
        Ok(Stmt::Break)
    }

    /// 解析 continue 语句
    fn parse_continue_stmt(&mut self) -> Result<Stmt> {
        self.expect_token(Token::Continue)?;
        self.expect_token(Token::Semicolon)?;
        Ok(Stmt::Continue)
    }

    /// 解析 return 语句
    fn parse_return_stmt(&mut self) -> Result<Stmt> {
        self.expect_token(Token::Return)?;

        // 检查是否有返回值（如果下一个token不是分号，则有返回值）
        let value = if !self.eof() && self.tokens[self.pos] != Token::Semicolon {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };

        self.expect_token(Token::Semicolon)?;

        Ok(Stmt::Return { value })
    }

    /// 解析函数定义语句
    fn parse_function_stmt(&mut self) -> Result<Stmt> {
        self.expect_token(Token::Fn)?;

        // 解析函数名
        let name = if let Token::Id(id) = &self.tokens[self.pos] {
            let name = id.clone();
            self.pos += 1;
            name
        } else {
            return Err(anyhow!(self.err("Expected function name")));
        };

        // 解析参数列表
        self.expect_token(Token::LParen)?;
        let mut params: Vec<String> = Vec::new();
        let mut param_types: Vec<Option<Type>> = Vec::new();

        while !self.eof() && self.tokens[self.pos] != Token::RParen {
            // 参数名
            let param_name = if let Token::Id(param) = &self.tokens[self.pos] {
                let p = param.clone();
                self.pos += 1;
                p
            } else {
                return Err(anyhow!(self.err("Expected parameter name")));
            };

            // 可选的参数类型注解 `: Type`
            let mut parsed_type: Option<Type> = None;
            if !self.eof() && self.tokens[self.pos] == Token::Colon {
                self.pos += 1; // consume ':'
                let ty = self.parse_inline_type_until_param_delim()?;
                parsed_type = Some(ty);
            }

            params.push(param_name);
            param_types.push(parsed_type);

            // 分隔符：逗号或结束
            if !self.eof() && self.tokens[self.pos] == Token::Comma {
                self.pos += 1; // 继续下一个参数
            } else if !self.eof() && self.tokens[self.pos] == Token::RParen {
                // end of params
            } else if self.eof() {
                return Err(anyhow!(self.err("Unexpected end while parsing parameters")));
            } else {
                return Err(anyhow!(self.err("Expected ',' or ')' in parameter list")));
            }
        }

        self.expect_token(Token::RParen)?;

        // 可选的返回类型 `-> Type`
        let mut return_type: Option<Type> = None;
        if !self.eof() && self.tokens[self.pos] == Token::FnArrow {
            self.pos += 1; // consume '->'
            let ty = self.parse_inline_type_until_block_start()?;
            return_type = Some(ty);
        }

        // 解析函数体 (必须是块语句)
        let body = Box::new(self.parse_block_stmt()?);

        Ok(Stmt::Function {
            name,
            params,
            param_types,
            return_type,
            body,
        })
    }

    /// 解析块语句
    fn parse_block_stmt(&mut self) -> Result<Stmt> {
        self.expect_token(Token::LBrace)?;

        let mut statements = Vec::new();
        while !self.eof() && self.tokens[self.pos] != Token::RBrace {
            // 跳过空语句
            if self.tokens[self.pos] == Token::Semicolon {
                statements.push(Box::new(Stmt::Empty));
                self.pos += 1;
                continue;
            }

            let stmt = self.parse_statement()?;
            statements.push(Box::new(stmt));
        }

        self.expect_token(Token::RBrace)?;

        Ok(Stmt::Block { statements })
    }

    /// 解析表达式语句
    fn parse_expr_stmt(&mut self) -> Result<Stmt> {
        let expr = self.parse_expression()?;
        self.expect_token(Token::Semicolon)?;
        Ok(Stmt::Expr(Box::new(expr)))
    }

    /// 使用现有的表达式解析器来解析表达式
    fn parse_expression(&mut self) -> Result<Expr> {
        self.parse_expression_with_options(false)
    }

    /// 使用现有的表达式解析器来解析表达式，带选项
    fn parse_expression_with_options(&mut self, stop_at_for_loop_body: bool) -> Result<Expr> {
        // 找到表达式的结束位置
        let start_pos = self.pos;
        let mut depth = 0;
        let mut end_pos = start_pos;

        while end_pos < self.len {
            let token = &self.tokens[end_pos];

            match token {
                Token::LBrace if depth == 0 && stop_at_for_loop_body => {
                    break; // for循环体的开始
                }
                Token::LParen | Token::LBrace | Token::LBracket => {
                    depth += 1;
                    end_pos += 1;
                }
                Token::RParen => {
                    if depth == 0 {
                        break; // 条件表达式的结束
                    }
                    depth -= 1;
                    end_pos += 1;
                }
                Token::RBrace => {
                    if depth == 0 {
                        break; // 块的结束
                    }
                    depth -= 1;
                    end_pos += 1;
                }
                Token::RBracket => {
                    depth -= 1;
                    end_pos += 1;
                }
                Token::Semicolon if depth == 0 => {
                    break;
                }
                Token::Else if depth == 0 => {
                    break;
                }
                _ => {
                    end_pos += 1;
                }
            }
        }

        if end_pos == start_pos {
            return Err(anyhow!(self.err("Expected expression")));
        }

        // 使用表达式解析器解析这部分 tokens
        let expr_tokens = &self.tokens[start_pos..end_pos];
        let mut expr_parser = ExprParser::new(expr_tokens);
        let expr = expr_parser.parse()?;

        // 更新位置
        self.pos = end_pos;

        Ok(expr)
    }

    /// 辅助方法
    fn eof(&self) -> bool {
        self.pos >= self.len
    }

    fn expect_token(&mut self, expected: Token) -> Result<()> {
        if self.eof() {
            return Err(anyhow!(
                self.err(&format!("Expected {:?}, found end of input", expected))
            ));
        }

        if std::mem::discriminant(&self.tokens[self.pos]) != std::mem::discriminant(&expected) {
            return Err(anyhow!(
                self.err(&format!("Expected {:?}, found {:?}", expected, self.tokens[self.pos]))
            ));
        }

        self.pos += 1;
        Ok(())
    }

    fn peek_ahead(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.pos + offset)
    }

    /// 解析 import 语句
    fn parse_import_stmt(&mut self) -> Result<Stmt> {
        self.expect_token(Token::Import)?;

        // Check for different import patterns
        let import_stmt = match &self.tokens[self.pos] {
            // import "path";
            Token::Str(path) => {
                let path = path.clone();
                self.pos += 1;
                ImportStmt::File { path }
            }
            // import { ... } from source
            Token::LBrace => {
                self.pos += 1; // consume {
                let items = self.parse_import_items()?;
                self.expect_token(Token::RBrace)?;
                self.expect_token(Token::From)?;
                let source = self.parse_import_source()?;
                ImportStmt::Items { items, source }
            }
            // import * as alias from source
            Token::Mul => {
                self.pos += 1; // consume *
                self.expect_token(Token::As)?;
                let alias = self.expect_id()?;
                self.expect_token(Token::From)?;
                let source = self.parse_import_source()?;
                ImportStmt::Namespace { alias, source }
            }
            // import module; or import module as alias;
            Token::Id(module) => {
                let module = module.clone();
                self.pos += 1;

                if !self.eof() && self.tokens[self.pos] == Token::As {
                    self.pos += 1; // consume 'as'
                    let alias = self.expect_id()?;
                    ImportStmt::ModuleAlias { module, alias }
                } else {
                    ImportStmt::Module { module }
                }
            }
            _ => {
                return Err(anyhow!(self.err("Expected import specifier")));
            }
        };

        self.expect_token(Token::Semicolon)?;
        Ok(Stmt::Import(import_stmt))
    }

    /// Parse import items list: name, name as alias, ...
    fn parse_import_items(&mut self) -> Result<Vec<ImportItem>> {
        let mut items = Vec::new();

        loop {
            let name = self.expect_id()?;
            let alias = if !self.eof() && self.tokens[self.pos] == Token::As {
                self.pos += 1; // consume 'as'
                Some(self.expect_id()?)
            } else {
                None
            };

            items.push(ImportItem { name, alias });

            // Check for more items
            if !self.eof() && self.tokens[self.pos] == Token::Comma {
                self.pos += 1; // consume comma
            } else {
                break;
            }
        }

        Ok(items)
    }

    /// Parse import source (module name or file path)
    fn parse_import_source(&mut self) -> Result<ImportSource> {
        match &self.tokens[self.pos] {
            Token::Str(path) => {
                let path = path.clone();
                self.pos += 1;
                Ok(ImportSource::File(path))
            }
            Token::Id(name) => {
                let name = name.clone();
                self.pos += 1;
                Ok(ImportSource::Module(name))
            }
            _ => Err(anyhow!(self.err("Expected module name or file path"))),
        }
    }

    /// Helper to expect an identifier token
    fn expect_id(&mut self) -> Result<String> {
        if self.eof() {
            return Err(anyhow!(self.err("Expected identifier")));
        }

        match &self.tokens[self.pos] {
            Token::Id(id) => {
                let id = id.clone();
                self.pos += 1;
                Ok(id)
            }
            _ => Err(anyhow!(self.err("Expected identifier"))),
        }
    }

    /// Parse a type annotation, handling union types and other complex type syntax
    fn parse_type_annotation(&mut self) -> Result<Type> {
        let mut type_tokens = Vec::new();

        // Collect tokens that make up the type annotation until we hit a token that can't be part of a type
        while !self.eof() {
            match &self.tokens[self.pos] {
                Token::Id(_) => {
                    type_tokens.push(&self.tokens[self.pos]);
                    self.pos += 1;
                }
                Token::Lt =>
                // For generic types like List<Int>
                {
                    type_tokens.push(&self.tokens[self.pos]);
                    self.pos += 1;
                }
                Token::Gt =>
                // For generic types like List<Int>
                {
                    type_tokens.push(&self.tokens[self.pos]);
                    self.pos += 1;
                }
                Token::Comma =>
                // For generic types like Map<String, Int>
                {
                    type_tokens.push(&self.tokens[self.pos]);
                    self.pos += 1;
                }
                Token::LParen | Token::RParen =>
                // For function types
                {
                    type_tokens.push(&self.tokens[self.pos]);
                    self.pos += 1;
                }
                Token::Arrow =>
                // For function types
                {
                    type_tokens.push(&self.tokens[self.pos]);
                    self.pos += 1;
                }
                Token::Question =>
                // For optional types
                {
                    type_tokens.push(&self.tokens[self.pos]);
                    self.pos += 1;
                }
                Token::Pipe =>
                // For union types
                {
                    type_tokens.push(&self.tokens[self.pos]);
                    self.pos += 1;
                }
                // Stop at any other token (like =, ;, etc.)
                _ => break,
            }
        }

        if type_tokens.is_empty() {
            return Err(anyhow!(self.err("Expected type annotation")));
        }

        // Convert tokens back to string and parse
        let type_str = self.tokens_to_type_string(&type_tokens);
        let parsed_type = Type::parse(&type_str);
        parsed_type.ok_or_else(|| anyhow!(self.err(&format!("Invalid type: {}", type_str))))
    }

    /// Convert a sequence of tokens back to a type string for parsing
    fn tokens_to_type_string(&self, tokens: &[&Token]) -> String {
        let mut result = String::new();

        for (i, token) in tokens.iter().enumerate() {
            if i > 0 {
                // Add space before pipe for union types
                match token {
                    // Union types
                    Token::Pipe => result.push_str(" | "),
                    // Do NOT insert space before '<'
                    Token::Lt => result.push('<'),
                    // No leading space before these closers / separators
                    Token::Gt | Token::Comma | Token::RParen | Token::RBracket | Token::RBrace => {
                        result.push_str(&self.token_to_string(token));
                    }
                    // Default: insert a single space unless previous was '<'
                    _ => {
                        if !matches!(tokens.get(i - 1), Some(Token::Lt)) {
                            result.push(' ');
                        }
                        result.push_str(&self.token_to_string(token));
                    }
                }
            } else {
                result.push_str(&self.token_to_string(token));
            }
        }

        result
    }

    /// Convert a single token to its string representation
    fn token_to_string(&self, token: &Token) -> String {
        match token {
            Token::Id(name) => name.clone(),
            Token::Str(s) => format!("\"{}\"", s),
            Token::Int(i) => i.to_string(),
            Token::Float(f) => f.to_string(),
            Token::Bool(b) => b.to_string(),
            Token::LParen => "(".to_string(),
            Token::RParen => ")".to_string(),
            Token::LBrace => "{".to_string(),
            Token::RBrace => "}".to_string(),
            Token::LBracket => "[".to_string(),
            Token::RBracket => "]".to_string(),
            Token::Comma => ",".to_string(),
            Token::Colon => ":".to_string(),
            Token::Pipe => "|".to_string(),
            Token::Question => "?".to_string(),
            Token::FnArrow => "->".to_string(),
            Token::Lt => "<".to_string(),
            Token::Gt => ">".to_string(),
            _ => format!("{:?}", token),
        }
    }

    /// Parse an inline type annotation inside parameter list until reaching a comma or ')'
    /// at zero nesting depth for (), [] and <>. Does not consume the delimiter.
    fn parse_inline_type_until_param_delim(&mut self) -> Result<Type> {
        let start_pos = self.pos;
        let mut tokens: Vec<&Token> = Vec::new();
        let mut paren: i32 = 0;
        let mut bracket: i32 = 0;
        let mut angle: i32 = 0;
        let mut guard: usize = 0;

        while !self.eof() {
            guard += 1;
            if guard > 1000 {
                // hard stop to avoid pathological scans
                break;
            }
            let t = &self.tokens[self.pos];
            match t {
                Token::LParen => {
                    paren += 1;
                    tokens.push(t);
                    self.pos += 1;
                }
                Token::RParen => {
                    if paren == 0 && bracket == 0 && angle == 0 {
                        break;
                    }
                    if paren > 0 {
                        paren -= 1;
                    }
                    tokens.push(t);
                    self.pos += 1;
                }
                Token::LBracket => {
                    bracket += 1;
                    tokens.push(t);
                    self.pos += 1;
                }
                Token::RBracket => {
                    if bracket > 0 {
                        bracket -= 1;
                    }
                    tokens.push(t);
                    self.pos += 1;
                }
                Token::Lt => {
                    angle += 1;
                    tokens.push(t);
                    self.pos += 1;
                }
                Token::Gt => {
                    if angle > 0 {
                        angle -= 1;
                    }
                    tokens.push(t);
                    self.pos += 1;
                }
                Token::Comma if paren == 0 && bracket == 0 && angle == 0 => {
                    break;
                }
                _ => {
                    tokens.push(t);
                    self.pos += 1;
                }
            }
        }

        if tokens.is_empty() {
            // reset pos to start to avoid desync
            self.pos = start_pos;
            return Err(anyhow!(self.err("Expected type annotation")));
        }

        let type_str = self.tokens_to_type_string(&tokens);
        Type::parse(&type_str).ok_or_else(|| anyhow!(self.err(&format!("Invalid type: {}", type_str))))
    }

    /// Parse a return type until the start of the function body '{' at zero depth.
    /// Does not consume the '{'.
    fn parse_inline_type_until_block_start(&mut self) -> Result<Type> {
        let start_pos = self.pos;
        let mut tokens: Vec<&Token> = Vec::new();
        let mut paren: i32 = 0;
        let mut bracket: i32 = 0;
        let mut angle: i32 = 0;
        let mut guard: usize = 0;

        while !self.eof() {
            guard += 1;
            if guard > 2000 {
                // hard stop to avoid pathological scans
                break;
            }
            let t = &self.tokens[self.pos];
            match t {
                Token::LBrace if paren == 0 && bracket == 0 && angle == 0 => {
                    break;
                }
                Token::LParen => {
                    paren += 1;
                    tokens.push(t);
                    self.pos += 1;
                }
                Token::RParen => {
                    if paren > 0 {
                        paren -= 1;
                    }
                    tokens.push(t);
                    self.pos += 1;
                }
                Token::LBracket => {
                    bracket += 1;
                    tokens.push(t);
                    self.pos += 1;
                }
                Token::RBracket => {
                    if bracket > 0 {
                        bracket -= 1;
                    }
                    tokens.push(t);
                    self.pos += 1;
                }
                Token::Lt => {
                    angle += 1;
                    tokens.push(t);
                    self.pos += 1;
                }
                Token::Gt => {
                    if angle > 0 {
                        angle -= 1;
                    }
                    tokens.push(t);
                    self.pos += 1;
                }
                _ => {
                    tokens.push(t);
                    self.pos += 1;
                }
            }
        }

        if tokens.is_empty() {
            self.pos = start_pos;
            return Err(anyhow!(self.err("Expected return type after '->'")));
        }

        let type_str = self.tokens_to_type_string(&tokens);
        Type::parse(&type_str).ok_or_else(|| anyhow!(self.err(&format!("Invalid return type: {}", type_str))))
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

    /// Get the current token span if available
    fn current_span(&self) -> Option<Span> {
        if let Some(spans) = &self.token_spans {
            if self.pos < spans.len() {
                Some(spans[self.pos].clone())
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Parse a pattern for if let expressions
    /// Delegates to the AST parser's pattern parsing functionality
    fn parse_pattern(&mut self) -> Result<crate::expr::Pattern> {
        // Find the end of the pattern by looking for the '=' token
        let start_pos = self.pos;
        let mut end_pos = start_pos;
        let mut depth = 0;

        while end_pos < self.len {
            match &self.tokens[end_pos] {
                Token::LParen | Token::LBrace | Token::LBracket => {
                    depth += 1;
                    end_pos += 1;
                }
                Token::RParen | Token::RBrace | Token::RBracket => {
                    depth -= 1;
                    end_pos += 1;
                }
                Token::Assign if depth == 0 => {
                    break; // Found the '=' at top level, pattern ends here
                }
                _ => {
                    end_pos += 1;
                }
            }
        }

        if end_pos == start_pos {
            return Err(anyhow!(self.err("Expected pattern before '='")));
        }

        // Use AST parser to parse the pattern
        let pattern_tokens = &self.tokens[start_pos..end_pos];
        let mut ast_parser = ExprParser::new(pattern_tokens);
        let pattern = ast_parser.parse_pattern()?;

        // Update position
        self.pos = end_pos;

        Ok(pattern)
    }
}
