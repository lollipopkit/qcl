use std::process::Command;
use std::io::Write;

fn create_cargo_command(args: &[&str]) -> Command {
    let mut cmd = Command::new("cargo");
    cmd.arg("run");
    
    // Add feature flags based on what's enabled
    let mut features = Vec::new();
    #[cfg(feature = "json")]
    features.push("json");
    #[cfg(feature = "yaml")]
    features.push("yaml");
    #[cfg(feature = "toml")]
    features.push("toml");
    #[cfg(feature = "sem_arith")]
    features.push("sem_arith");
    #[cfg(feature = "adv_arith")]
    features.push("adv_arith");
    
    if !features.is_empty() {
        cmd.arg("--features");
        cmd.arg(features.join(","));
    }
    
    cmd.arg("--");
    cmd.args(args);
    cmd
}

#[test]
#[cfg(feature = "json")]
fn test_cli_json_auto_detection() {
    let mut cmd = create_cargo_command(&["@name == \"test\""])
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
#[cfg(feature = "json")]
fn test_cli_json_explicit_flag() {
    let mut cmd = create_cargo_command(&["--json", "@name == \"test\""])
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
#[cfg(feature = "yaml")]
fn test_cli_yaml_auto_detection() {
    let mut cmd = create_cargo_command(&["@name == \"test\""])
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
#[cfg(feature = "yaml")]
fn test_cli_yaml_explicit_flag() {
    let mut cmd = create_cargo_command(&["--yaml", "@name == \"test\""])
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
#[cfg(feature = "toml")]
fn test_cli_toml_auto_detection() {
    let mut cmd = create_cargo_command(&["@name == \"test\""])
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
#[cfg(feature = "toml")]
fn test_cli_toml_explicit_flag() {
    let mut cmd = create_cargo_command(&["--toml", "@name == \"test\""])
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
#[cfg(feature = "yaml")]
fn test_cli_yaml_nested_structures() {
    let mut cmd = create_cargo_command(&["@req.user.role == \"admin\""])
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
#[cfg(feature = "toml")]
fn test_cli_toml_nested_structures() {
    let mut cmd = create_cargo_command(&["@req.user.role == \"admin\""])
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
#[cfg(feature = "yaml")]
fn test_cli_yaml_arrays() {
    let mut cmd = create_cargo_command(&["@permissions.0 == \"read\""])
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
#[cfg(feature = "toml")]
fn test_cli_toml_arrays() {
    let mut cmd = create_cargo_command(&["@permissions.0 == \"read\""])
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
#[cfg(feature = "toml")]
fn test_cli_toml_table_arrays() {
    let mut cmd = create_cargo_command(&["@users.0.name == \"alice\""])
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
#[cfg(feature = "yaml")]
fn test_cli_error_handling() {
    let mut cmd = create_cargo_command(&["@name == \"test\""])
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
#[cfg(any(feature = "json", feature = "yaml", feature = "toml"))]
fn test_cli_usage_message() {
    let output = create_cargo_command(&[])
        .output()
        .expect("Failed to execute command");

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