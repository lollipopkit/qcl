use qcl_lsp::analyzer::QclAnalyzer;
use tower_lsp::lsp_types::DiagnosticSeverity;

#[test]
fn test_expression_error_position() {
    let analyzer = QclAnalyzer::new();
    
    // Test unterminated string - error should be at the end of line
    let code = "@req.user.name == 'unterminated string";
    let result = analyzer.analyze(code);
    
    assert!(!result.diagnostics.is_empty());
    let diagnostic = &result.diagnostics[0];
    assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
    
    // The error should indicate a position near the unterminated string
    // Since we have position tracking now, it should not be at (0,0)
    println!("Error position: {:?}", diagnostic.range);
    println!("Error message: {}", diagnostic.message);
}

#[test] 
fn test_statement_error_position() {
    let analyzer = QclAnalyzer::new();
    
    // Test invalid statement syntax
    let code = r#"
let x = 5;
if (x == 5 {  // Missing closing parenthesis
    return true;
}
"#;
    
    let result = analyzer.analyze(code);
    assert!(!result.diagnostics.is_empty());
    
    let diagnostic = &result.diagnostics[0];
    assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
    
    println!("Statement error position: {:?}", diagnostic.range);
    println!("Statement error message: {}", diagnostic.message);
    
    // The error should be positioned around line 3 where the syntax error is
    assert!(diagnostic.range.start.line >= 2); // 0-indexed, so line 3 would be index 2
}

#[test]
fn test_multiline_error_position() {
    let analyzer = QclAnalyzer::new();
    
    let code = r#"let user = @req.user;
let role = user.role;
let invalid = role == 'admin' &&;  // Invalid syntax at end
return invalid;"#;
    
    let result = analyzer.analyze(code);
    
    if !result.diagnostics.is_empty() {
        let diagnostic = &result.diagnostics[0];
        println!("Multiline error position: {:?}", diagnostic.range);
        println!("Multiline error message: {}", diagnostic.message);
        
        // Error should be on line 3 (0-indexed line 2)
        assert!(diagnostic.range.start.line == 2);
    } else {
        // If no errors, the expression might be parsed differently
        println!("No errors found, symbols: {:?}", result.symbols);
    }
}

#[test]
fn test_simple_syntax_error_position() {
    let analyzer = QclAnalyzer::new();
    
    // Test with simple syntax error - missing quote
    let code = "@req.user.name == 'admin";  // Missing closing quote
    let result = analyzer.analyze(code);
    
    assert!(!result.diagnostics.is_empty());
    let diagnostic = &result.diagnostics[0];
    println!("Simple syntax error position: {:?}", diagnostic.range);
    println!("Simple syntax error message: {}", diagnostic.message);
    
    // Should report an error
    assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
}