use std::process::Command;
use std::io::Write;

#[test]
fn test_cli_json_auto_detection() {
    let mut cmd = Command::new("cargo")
        .args(&["run", "--", "@name == \"test\""])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("Failed to spawn command");

    let stdin = cmd.stdin.as_mut().expect("Failed to get stdin");
    stdin.write_all(b"{\"name\": \"test\", \"age\": 25}").expect("Failed to write to stdin");
    stdin.flush().expect("Failed to flush stdin");
    let _ = stdin;

    let output = cmd.wait_with_output().expect("Failed to wait for command");
    let stdout = String::from_utf8_lossy(&output.stdout);
    
    assert!(stdout.contains("true"));
}

#[test]
fn test_cli_json_explicit_flag() {
    let mut cmd = Command::new("cargo")
        .args(&["run", "--", "--json", "@name == \"test\""])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("Failed to spawn command");

    let stdin = cmd.stdin.as_mut().expect("Failed to get stdin");
    stdin.write_all(b"{\"name\": \"test\", \"age\": 25}").expect("Failed to write to stdin");
    stdin.flush().expect("Failed to flush stdin");
    let _ = stdin;

    let output = cmd.wait_with_output().expect("Failed to wait for command");
    let stdout = String::from_utf8_lossy(&output.stdout);
    
    assert!(stdout.contains("true"));
}

#[test]
fn test_cli_yaml_auto_detection() {
    let mut cmd = Command::new("cargo")
        .args(&["run", "--", "@name == \"test\""])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("Failed to spawn command");

    let stdin = cmd.stdin.as_mut().expect("Failed to get stdin");
    stdin.write_all(b"name: test\nage: 25").expect("Failed to write to stdin");
    stdin.flush().expect("Failed to flush stdin");
    let _ = stdin;

    let output = cmd.wait_with_output().expect("Failed to wait for command");
    let stdout = String::from_utf8_lossy(&output.stdout);
    
    assert!(stdout.contains("true"));
}

#[test]
fn test_cli_yaml_explicit_flag() {
    let mut cmd = Command::new("cargo")
        .args(&["run", "--", "--yaml", "@name == \"test\""])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("Failed to spawn command");

    let stdin = cmd.stdin.as_mut().expect("Failed to get stdin");
    stdin.write_all(b"name: test\nage: 25").expect("Failed to write to stdin");
    stdin.flush().expect("Failed to flush stdin");
    let _ = stdin;

    let output = cmd.wait_with_output().expect("Failed to wait for command");
    let stdout = String::from_utf8_lossy(&output.stdout);
    
    assert!(stdout.contains("true"));
}

#[test]
fn test_cli_toml_auto_detection() {
    let mut cmd = Command::new("cargo")
        .args(&["run", "--", "@name == \"test\""])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("Failed to spawn command");

    let stdin = cmd.stdin.as_mut().expect("Failed to get stdin");
    stdin.write_all(b"name = \"test\"\nage = 25").expect("Failed to write to stdin");
    stdin.flush().expect("Failed to flush stdin");
    let _ = stdin;

    let output = cmd.wait_with_output().expect("Failed to wait for command");
    let stdout = String::from_utf8_lossy(&output.stdout);
    
    assert!(stdout.contains("true"));
}

#[test]
fn test_cli_toml_explicit_flag() {
    let mut cmd = Command::new("cargo")
        .args(&["run", "--", "--toml", "@name == \"test\""])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("Failed to spawn command");

    let stdin = cmd.stdin.as_mut().expect("Failed to get stdin");
    stdin.write_all(b"name = \"test\"\nage = 25").expect("Failed to write to stdin");
    stdin.flush().expect("Failed to flush stdin");
    let _ = stdin;

    let output = cmd.wait_with_output().expect("Failed to wait for command");
    let stdout = String::from_utf8_lossy(&output.stdout);
    
    assert!(stdout.contains("true"));
}

#[test]
fn test_cli_yaml_nested_structures() {
    let mut cmd = Command::new("cargo")
        .args(&["run", "--", "@req.user.role == \"admin\""])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("Failed to spawn command");

    let stdin = cmd.stdin.as_mut().expect("Failed to get stdin");
    stdin.write_all(b"req:\n  user:\n    role: admin\n    name: test").expect("Failed to write to stdin");
    stdin.flush().expect("Failed to flush stdin");
    let _ = stdin;

    let output = cmd.wait_with_output().expect("Failed to wait for command");
    let stdout = String::from_utf8_lossy(&output.stdout);
    
    assert!(stdout.contains("true"));
}

#[test]
fn test_cli_toml_nested_structures() {
    let mut cmd = Command::new("cargo")
        .args(&["run", "--", "@req.user.role == \"admin\""])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("Failed to spawn command");

    let stdin = cmd.stdin.as_mut().expect("Failed to get stdin");
    stdin.write_all(b"[req.user]\nrole = \"admin\"\nname = \"test\"").expect("Failed to write to stdin");
    stdin.flush().expect("Failed to flush stdin");
    let _ = stdin;

    let output = cmd.wait_with_output().expect("Failed to wait for command");
    let stdout = String::from_utf8_lossy(&output.stdout);
    
    assert!(stdout.contains("true"));
}

#[test]
fn test_cli_yaml_arrays() {
    let mut cmd = Command::new("cargo")
        .args(&["run", "--", "@permissions.0 == \"read\""])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("Failed to spawn command");

    let stdin = cmd.stdin.as_mut().expect("Failed to get stdin");
    stdin.write_all(b"permissions:\n  - read\n  - write\n  - execute").expect("Failed to write to stdin");
    stdin.flush().expect("Failed to flush stdin");
    let _ = stdin;

    let output = cmd.wait_with_output().expect("Failed to wait for command");
    let stdout = String::from_utf8_lossy(&output.stdout);
    
    assert!(stdout.contains("true"));
}

#[test]
fn test_cli_toml_arrays() {
    let mut cmd = Command::new("cargo")
        .args(&["run", "--", "@permissions.0 == \"read\""])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("Failed to spawn command");

    let stdin = cmd.stdin.as_mut().expect("Failed to get stdin");
    stdin.write_all(b"permissions = [\"read\", \"write\", \"execute\"]").expect("Failed to write to stdin");
    stdin.flush().expect("Failed to flush stdin");
    let _ = stdin;

    let output = cmd.wait_with_output().expect("Failed to wait for command");
    let stdout = String::from_utf8_lossy(&output.stdout);
    
    assert!(stdout.contains("true"));
}

#[test]
fn test_cli_toml_table_arrays() {
    let mut cmd = Command::new("cargo")
        .args(&["run", "--", "@users.0.name == \"alice\""])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("Failed to spawn command");

    let stdin = cmd.stdin.as_mut().expect("Failed to get stdin");
    stdin.write_all(b"[[users]]\nname = \"alice\"\nrole = \"admin\"\n\n[[users]]\nname = \"bob\"\nrole = \"user\"").expect("Failed to write to stdin");
    stdin.flush().expect("Failed to flush stdin");
    let _ = stdin;

    let output = cmd.wait_with_output().expect("Failed to wait for command");
    let stdout = String::from_utf8_lossy(&output.stdout);
    
    assert!(stdout.contains("true"));
}

#[test]
fn test_cli_error_handling() {
    let mut cmd = Command::new("cargo")
        .args(&["run", "--", "@name == \"test\""])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("Failed to spawn command");

    let stdin = cmd.stdin.as_mut().expect("Failed to get stdin");
    stdin.write_all(b"invalid: [\n  - yaml\n  - structure\n").expect("Failed to write to stdin");
    stdin.flush().expect("Failed to flush stdin");
    let _ = stdin;

    let output = cmd.wait_with_output().expect("Failed to wait for command");
    
    // Should exit with non-zero code for invalid input
    assert!(!output.status.success());
}

#[test]
fn test_cli_usage_message() {
    let output = Command::new("cargo")
        .args(&["run", "--"])
        .output()
        .expect("Failed to execute command");

    let stderr = String::from_utf8_lossy(&output.stderr);
    
    assert!(stderr.contains("Usage:"));
    assert!(stderr.contains("json|yaml|toml"));
    assert!(stderr.contains("--json|--yaml|--toml"));
}