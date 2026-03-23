#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

run_case() {
    local name="$1"
    local expr="$2"
    local ctx_file="$3"

    echo "== $name =="
    set +e
    cargo run --quiet --features all -- --json "$expr" <"$ctx_file"
    local code=$?
    set -e
    echo "exit_code=$code"
    echo
}

run_timeout_case() {
    local name="$1"
    local expr="$2"
    local seconds="$3"
    local ctx_file="$4"

    echo "== $name =="
    set +e
    timeout "${seconds}"s cargo run --quiet --features all -- --json "$expr" <"$ctx_file"
    local code=$?
    set -e
    echo "exit_code=$code"
    echo
}

run_release_case() {
    local name="$1"
    local expr="$2"
    local ctx_file="$3"

    echo "== $name =="
    set +e
    cargo run --quiet --release --features all -- --json "$expr" <"$ctx_file"
    local code=$?
    set -e
    echo "exit_code=$code"
    echo
}

printf '{}' >"$TMP_DIR/empty.json"
printf '{"req":{"user":{"id":7}}}' >"$TMP_DIR/missing_status.json"
printf '{"req":{"user":{"id":1}},"record":{"granted":{"1":true}}}' >"$TMP_DIR/map_granted.json"
printf '{"req":{"user":{"level":1}},"record":{"required_level":5}}' >"$TMP_DIR/adv_in.json"
printf '{"req":{"user":{"used":9223372036854775807,"limit":0}},"record":{"request":1}}' >"$TMP_DIR/wrap_acl.json"
cat >"$TMP_DIR/auto_yaml_bypass.yaml" <<'EOF'
req:
  user:
    id: 1
record:
  granted:
    "1": true
EOF

cd "$ROOT"

run_case "panic-bang" "!" "$TMP_DIR/empty.json"
run_case "panic-dangling-add" "1 +" "$TMP_DIR/empty.json"
run_case "panic-mod-zero" "1 % 0" "$TMP_DIR/empty.json"
run_case "overflow-add" "9223372036854775807 + 1" "$TMP_DIR/empty.json"
run_case "overflow-mul" "3037000500 * 3037000500" "$TMP_DIR/empty.json"
run_case "acl-bypass-missing-status" '@req.user.status != "blocked" && @req.user.id == 7' "$TMP_DIR/missing_status.json"
run_case "acl-bypass-missing-eq" '@req.user.id == @record.owner.id' "$TMP_DIR/empty.json"
run_case "acl-bypass-map-granted" '@req.user.id in @record.granted' "$TMP_DIR/map_granted.json"
run_case "acl-bypass-advarith-in" '@req.user.level in @record.required_level' "$TMP_DIR/adv_in.json"
run_release_case "release-wrap-const" '9223372036854775807 + 1 < 0' "$TMP_DIR/empty.json"
run_release_case "release-wrap-acl" '@req.user.used + @record.request <= @req.user.limit' "$TMP_DIR/wrap_acl.json"
run_timeout_case "hang-illegal-caret" '^' 3 "$TMP_DIR/empty.json"
run_timeout_case "hang-illegal-dollar" '$' 3 "$TMP_DIR/empty.json"
run_timeout_case "hang-illegal-tilde" '~' 3 "$TMP_DIR/empty.json"

echo "== format-confusion-yaml-auto =="
set +e
cargo run --quiet --features all -- '@req.user.id in @record.granted' <"$TMP_DIR/auto_yaml_bypass.yaml"
echo "exit_code=$?"
set -e
echo

echo "== format-confusion-yaml-forced-json =="
set +e
cargo run --quiet --features all -- --json '@req.user.id in @record.granted' <"$TMP_DIR/auto_yaml_bypass.yaml"
echo "exit_code=$?"
set -e
echo
