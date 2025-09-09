# Repository Guidelines

## Project Structure & Module Organization
- Workspace crates: `core` (parsing, eval, types), `stdlib` (standard modules), `cli` (binary `qcl`), `lsp` (binary `qcl-lsp`). VS Code extension lives in `vscode-qcl/`. Docs in `docs/`, examples in `examples/`, benches in `core/benches/`.
- Tests: unit tests co-located in `core/src/*_test.rs`, `stdlib/src/*_test.rs`; integration tests in `lsp/tests/`.

## Build, Test, and Development Commands
- Build all crates: `cargo build --workspace`
- Run all tests: `cargo test --workspace`
- Run CLI: `echo '{"req":{"user":{"id":1}}}' | cargo run -p qcl-cli -- --expr "@req.user.id == 1"`
- Run LSP server: `cargo run -p qcl-lsp`
- VS Code extension: `cd vscode-qcl && npm run compile` (package with `vsce package`)

## Coding Style & Naming Conventions
- Rust edition: 2024 (LSP crate uses 2021). Format with `cargo fmt --all`. Lint with `cargo clippy --workspace --all-features -D warnings`.
- Naming: crates `qcl-*`; modules/functions `snake_case`; types/enums `UpperCamelCase`; constants `SCREAMING_SNAKE_CASE`.
- Error handling: prefer `anyhow::Result<T>` and avoid `unwrap()`/`expect()` in library code.

## Testing Guidelines
- Write focused unit tests near code or in `tests/` for integration (see `lsp/tests/`). Name tests `test_*` and cover parsing, evaluation, and error paths.
- Run by crate: `cargo test -p qcl-core`, `cargo test -p qcl-stdlib`, `cargo test -p qcl-lsp`; or all: `cargo test --workspace`.
- Benchmarks: `cargo bench -p qcl-core` (uses Criterion).

## Commit & Pull Request Guidelines
- Commit prefixes observed: `fix:`, `feat:`, `new:`, `opt.:`, `chore:`. Use short, imperative subjects; add details in the body if needed.
- PRs must include: summary, linked issues, affected crates, test plan (`cargo test --workspace`), and screenshots/GIFs for `vscode-qcl` UI changes. Ensure format/lint pass.

## Security & Configuration Tips
- CLI file inputs: use relative, sanitized paths only (no absolute paths or `..`). Features gate input formats: enable `json`, `yaml`, or `toml` as needed.
- Avoid introducing `unsafe` unless justified; keep dependencies within workspace standards.

