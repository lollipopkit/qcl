use crate::{token::Tokenizer, ast::Parser, expr::Expr, val::Val};
use std::collections::HashMap;
use std::sync::Arc;

#[cfg(test)]
#[test]
fn debug_at_expression() {
    // Test tokenization
    let input = "@req.user.name";
    let tokens = Tokenizer::tokenize(input).unwrap();
    println!("Tokens: {:?}", tokens);
    
    // Test parsing
    let expr = Parser::new(&tokens).parse().unwrap();
    println!("Parsed expr: {:?}", expr);
    
    // Test context
    let mut ctx_map = HashMap::new();
    let mut user_map = HashMap::new();
    user_map.insert("name".to_string(), Val::Str("Alice Johnson".into()));
    let mut req_map = HashMap::new();
    req_map.insert("user".to_string(), Val::Map(Arc::new(user_map)));
    ctx_map.insert("req".to_string(), Val::Map(Arc::new(req_map)));
    let ctx = Val::Map(Arc::new(ctx_map));
    
    // Test evaluation
    let result = expr.eval(&ctx);
    println!("Result: {:?}", result);
}
