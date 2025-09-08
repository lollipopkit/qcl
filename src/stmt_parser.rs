use crate::{
    ast::Parser as ExprParser,
    expr::Expr,
    stmt::{Stmt, Program},
    token::Token,
    val::Type,
};
use anyhow::{Result, anyhow};

/// Statement Parser - 解析语句的解析器
pub struct StmtParser<'a> {
    tokens: &'a [Token],
    pos: usize,
    len: usize,
}

impl<'a> StmtParser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        let len = tokens.len();
        Self {
            tokens,
            pos: 0,
            len,
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

            let stmt = self.parse_statement()?;
            statements.push(Box::new(stmt));
        }

        Program::new(statements)
    }

    /// 解析单个语句
    pub fn parse_statement(&mut self) -> Result<Stmt> {
        if self.eof() {
            return Ok(Stmt::Empty);
        }

        match &self.tokens[self.pos] {
            Token::If => self.parse_if_stmt(),
            Token::While => self.parse_while_stmt(),
            Token::Let => self.parse_let_stmt(),
            Token::Break => self.parse_break_stmt(),
            Token::Continue => self.parse_continue_stmt(),
            Token::Return => self.parse_return_stmt(),
            Token::Goto => self.parse_goto_stmt(),
            Token::Fn => self.parse_function_stmt(),
            Token::LBrace => self.parse_block_stmt(),
            Token::Id(id) => {
                // 检查是否为标签 (id:) 或赋值语句 (id = expr;)
                if self.peek_ahead(1) == Some(&Token::Colon) {
                    self.parse_label_stmt_with_id(id.clone())
                } else if self.peek_ahead(1) == Some(&Token::Assign) {
                    self.parse_assign_stmt_with_id(id.clone())
                } else {
                    // 表达式语句
                    self.parse_expr_stmt()
                }
            }
            _ => self.parse_expr_stmt(), // 默认作为表达式语句处理
        }
    }

    /// 解析 if 语句
    fn parse_if_stmt(&mut self) -> Result<Stmt> {
        self.expect_token(Token::If)?;
        self.expect_token(Token::LParen)?;
        
        let condition = self.parse_expression()?;
        
        self.expect_token(Token::RParen)?;
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

    /// 解析 while 语句
    fn parse_while_stmt(&mut self) -> Result<Stmt> {
        self.expect_token(Token::While)?;
        self.expect_token(Token::LParen)?;
        
        let condition = self.parse_expression()?;
        
        self.expect_token(Token::RParen)?;
        let body = Box::new(self.parse_statement()?);

        Ok(Stmt::While {
            condition: Box::new(condition),
            body,
        })
    }

    /// 解析 let 语句
    fn parse_let_stmt(&mut self) -> Result<Stmt> {
        self.expect_token(Token::Let)?;
        
        let name = if let Token::Id(id) = &self.tokens[self.pos] {
            let name = id.clone();
            self.pos += 1;
            name
        } else {
            return Err(anyhow!(self.err("Expected variable name after 'let'")));
        };

        // Check for optional type annotation
        let type_annotation = if !self.eof() && self.tokens[self.pos] == Token::Colon {
            self.pos += 1; // consume ':'
            
            if let Token::Id(type_name) = &self.tokens[self.pos] {
                let typ = Type::from_str(type_name)
                    .ok_or_else(|| anyhow!(self.err(&format!("Unknown type: {}", type_name))))?;
                self.pos += 1;
                Some(typ)
            } else {
                return Err(anyhow!(self.err("Expected type name after ':'")));
            }
        } else {
            None
        };

        self.expect_token(Token::Assign)?;
        let value = self.parse_expression()?;
        self.expect_token(Token::Semicolon)?;

        Ok(Stmt::Let {
            name,
            type_annotation,
            value: Box::new(value),
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
        })
    }


    /// 解析标签语句（已匹配标识符）
    fn parse_label_stmt_with_id(&mut self, name: String) -> Result<Stmt> {
        // 我们已经在parse_statement中匹配了Id，现在跳过它并继续解析标签
        self.pos += 1; // 跳过已匹配的 Id token
        self.expect_token(Token::Colon)?;

        Ok(Stmt::Label { name })
    }

    /// 解析 goto 语句
    fn parse_goto_stmt(&mut self) -> Result<Stmt> {
        self.expect_token(Token::Goto)?;
        
        let label = if let Token::Id(id) = &self.tokens[self.pos] {
            let label = id.clone();
            self.pos += 1;
            label
        } else {
            return Err(anyhow!(self.err("Expected label name after 'goto'")));
        };

        self.expect_token(Token::Semicolon)?;

        Ok(Stmt::Goto { label })
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
        let mut params = Vec::new();
        
        while !self.eof() && self.tokens[self.pos] != Token::RParen {
            if let Token::Id(param) = &self.tokens[self.pos] {
                params.push(param.clone());
                self.pos += 1;
                
                // 如果下一个token是逗号，则继续解析参数
                if !self.eof() && self.tokens[self.pos] == Token::Comma {
                    self.pos += 1;
                } else if self.tokens[self.pos] != Token::RParen {
                    return Err(anyhow!(self.err("Expected ',' or ')' in parameter list")));
                }
            } else {
                return Err(anyhow!(self.err("Expected parameter name")));
            }
        }
        
        self.expect_token(Token::RParen)?;
        
        // 解析函数体 (必须是块语句)
        let body = Box::new(self.parse_block_stmt()?);
        
        Ok(Stmt::Function { name, params, body })
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
        // 找到表达式的结束位置
        let start_pos = self.pos;
        let mut depth = 0;
        let mut end_pos = start_pos;

        while end_pos < self.len {
            match &self.tokens[end_pos] {
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
                Token::Semicolon if depth == 0 => break,
                Token::Else if depth == 0 => break,
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
            return Err(anyhow!(self.err(&format!(
                "Expected {:?}, found {:?}",
                expected, self.tokens[self.pos]
            ))));
        }

        self.pos += 1;
        Ok(())
    }

    fn peek_ahead(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.pos + offset)
    }

    fn err(&self, msg: &str) -> String {
        let r_idx = if self.pos + 5 < self.len {
            self.pos + 5
        } else {
            self.len
        };
        let l_idx = if self.pos > 5 { self.pos - 5 } else { 0 };
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