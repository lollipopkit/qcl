use std::io::Write;
use std::process::{Command, Output};

fn create_cargo_command(args: &[&str]) -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_qcl"));
    cmd.args(args);
    cmd
}

fn run_cli(args: &[&str], stdin_bytes: &[u8]) -> Output {
    let mut cmd = create_cargo_command(args)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("Failed to spawn command");

    let stdin = cmd.stdin.as_mut().expect("Failed to get stdin");
    stdin.write_all(stdin_bytes).expect("Failed to write to stdin");
    stdin.flush().expect("Failed to flush stdin");
    let _ = stdin;

    cmd.wait_with_output().expect("Failed to wait for command")
}

#[test]
#[cfg(feature = "json")]
fn test_cli_json_default_parser() {
    let output = run_cli(&["@name == \"test\""], b"{\"name\": \"test\", \"age\": 25}");
    let stdout = String::from_utf8_lossy(&output.stdout);

    assert!(output.status.success());
    assert!(stdout.contains("true"));
}

#[test]
#[cfg(feature = "json")]
fn test_cli_json_explicit_flag() {
    let output = run_cli(&["--json", "@name == \"test\""], b"{\"name\": \"test\", \"age\": 25}");
    let stdout = String::from_utf8_lossy(&output.stdout);

    assert!(output.status.success());
    assert!(stdout.contains("true"));
}

#[test]
#[cfg(all(feature = "json", feature = "yaml"))]
fn test_cli_default_parser_rejects_yaml() {
    let output = run_cli(&["@name == \"test\""], b"name: test\nage: 25");
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(!output.status.success());
    assert!(!stderr.trim().is_empty());
}

#[test]
#[cfg(feature = "yaml")]
fn test_cli_yaml_explicit_flag() {
    let output = run_cli(&["--yaml", "@name == \"test\""], b"name: test\nage: 25");
    let stdout = String::from_utf8_lossy(&output.stdout);

    assert!(output.status.success());
    assert!(stdout.contains("true"));
}

#[test]
#[cfg(all(feature = "json", feature = "toml"))]
fn test_cli_default_parser_rejects_toml() {
    let output = run_cli(&["@name == \"test\""], b"name = \"test\"\nage = 25");
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(!output.status.success());
    assert!(!stderr.trim().is_empty());
}

#[test]
#[cfg(feature = "toml")]
fn test_cli_toml_explicit_flag() {
    let output = run_cli(&["--toml", "@name == \"test\""], b"name = \"test\"\nage = 25");
    let stdout = String::from_utf8_lossy(&output.stdout);

    assert!(output.status.success());
    assert!(stdout.contains("true"));
}

#[test]
#[cfg(feature = "yaml")]
fn test_cli_yaml_nested_structures() {
    let output = run_cli(
        &["--yaml", "@req.user.role == \"admin\""],
        b"req:\n  user:\n    role: admin\n    name: test",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);

    assert!(output.status.success());
    assert!(stdout.contains("true"));
}

#[test]
#[cfg(feature = "toml")]
fn test_cli_toml_nested_structures() {
    let output = run_cli(
        &["--toml", "@req.user.role == \"admin\""],
        b"[req.user]\nrole = \"admin\"\nname = \"test\"",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);

    assert!(output.status.success());
    assert!(stdout.contains("true"));
}

#[test]
#[cfg(feature = "yaml")]
fn test_cli_yaml_arrays() {
    let output = run_cli(
        &["--yaml", "@permissions.0 == \"read\""],
        b"permissions:\n  - read\n  - write\n  - execute",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);

    assert!(output.status.success());
    assert!(stdout.contains("true"));
}

#[test]
#[cfg(feature = "toml")]
fn test_cli_toml_arrays() {
    let output = run_cli(
        &["--toml", "@permissions.0 == \"read\""],
        b"permissions = [\"read\", \"write\", \"execute\"]",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);

    assert!(output.status.success());
    assert!(stdout.contains("true"));
}

#[test]
#[cfg(feature = "toml")]
fn test_cli_toml_table_arrays() {
    let output = run_cli(
        &["--toml", "@users.0.name == \"alice\""],
        b"[[users]]\nname = \"alice\"\nrole = \"admin\"\n\n[[users]]\nname = \"bob\"\nrole = \"user\"",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);

    assert!(output.status.success());
    assert!(stdout.contains("true"));
}

#[test]
#[cfg(feature = "yaml")]
fn test_cli_error_handling() {
    let mut cmd = create_cargo_command(&["@name == \"test\""])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("Failed to spawn command");

    let stdin = cmd.stdin.as_mut().expect("Failed to get stdin");
    stdin
        .write_all(b"invalid: [\n  - yaml\n  - structure\n")
        .expect("Failed to write to stdin");
    stdin.flush().expect("Failed to flush stdin");
    let _ = stdin;

    let output = cmd.wait_with_output().expect("Failed to wait for command");

    // Should exit with non-zero code for invalid input
    assert!(!output.status.success());
}

#[test]
#[cfg(any(feature = "json", feature = "yaml", feature = "toml"))]
fn test_cli_usage_message() {
    let output = create_cargo_command(&[]).output().expect("Failed to execute command");

    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(stderr.contains("Usage:"));
    // Check that the usage message contains the enabled formats
    #[cfg(feature = "json")]
    assert!(stderr.contains("json"));
    #[cfg(feature = "yaml")]
    assert!(stderr.contains("yaml"));
    #[cfg(feature = "toml")]
    assert!(stderr.contains("toml"));
}

#[test]
#[cfg(feature = "json")]
fn test_cli_ternary_operator() {
    let output = run_cli(&["@role == 'admin' ? 'allowed' : 'denied'"], b"{\"role\": \"admin\"}");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(output.status.success());
    assert!(stdout.contains("allowed"));
}

#[test]
#[cfg(feature = "json")]
fn test_cli_nullish_coalesce() {
    let output = run_cli(&["@missing ?? 'fallback'"], b"{\"present\": 1}");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(output.status.success());
    assert!(stdout.contains("fallback"));
}

#[test]
#[cfg(feature = "json")]
fn test_cli_negative_indexing() {
    let output = run_cli(&["@items.-1 == 'c'"], b"{\"items\": [\"a\", \"b\", \"c\"]}");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(output.status.success());
    assert!(stdout.contains("true"));
}

#[test]
#[cfg(feature = "json")]
fn test_cli_hex_octal_literals() {
    let output = run_cli(&["@val == 0xFF"], b"{\"val\": 255}");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(output.status.success());
    assert!(stdout.contains("true"));
}

#[test]
#[cfg(feature = "json")]
fn test_cli_block_comment() {
    let output = run_cli(&["@val /* check value */ == 1"], b"{\"val\": 1}");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(output.status.success());
    assert!(stdout.contains("true"));
}

#[test]
#[cfg(feature = "json")]
fn test_cli_unicode_escape() {
    let output = run_cli(&[r#"@name == "\u0041lice""#], b"{\"name\": \"Alice\"}");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(output.status.success());
    assert!(stdout.contains("true"));
}
