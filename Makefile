SHELL := bash

.DEFAULT_GOAL := help

FUZZ_DIR := fuzz
DICT := $(FUZZ_DIR)/qcl.dict
TARGETS := expr_parse expr_eval de_formats deep_access in_operator
DICT_TARGETS := expr_parse expr_eval de_formats in_operator

CARGO ?= cargo
TARGET ?= expr_parse
CORPUS ?= $(FUZZ_DIR)/corpus/$(TARGET)
MAX_TIME ?= 30
RUN_ARGS ?=
ARGS ?=
ARTIFACT_PREFIX ?= $(FUZZ_DIR)/artifacts/$(TARGET)/
CARGO_FUZZ ?= cargo fuzz

DICT_ARG = $(if $(filter $(TARGET),$(DICT_TARGETS)),-dict="$(DICT)",)
FUZZ_ARGS = -artifact_prefix="$(ARTIFACT_PREFIX)" -max_total_time=$(MAX_TIME) $(DICT_ARG) $(RUN_ARGS)

.PHONY: clean help check test fmt fmt-check clippy bench run fuzz fuzz-all fuzz-quick fuzz-check fuzz-install fuzz-clean fuzz-list fuzz-replay validate-fuzz-target wasm ffi python

help:
	@printf '%s\n' \
		'Usage:' \
		'  make all' \
		'  make check' \
		'  make clean' \
		'  make test' \
		'  make fmt' \
		'  make clippy' \
		'  make run ARGS="--help"' \
		'  make fuzz TARGET=expr_parse MAX_TIME=30 RUN_ARGS="-runs=1000"' \
		'  make fuzz-all MAX_TIME=60' \
		'' \
		'Targets:' \
		'  help         Show this message' \
		'  check        cargo check --features all' \
		'  clean        cargo clean plus fuzz outputs' \
		'  test         cargo test --features all -- --nocapture' \
		'  fmt          cargo fmt' \
		'  fmt-check    cargo fmt --check' \
		'  clippy       cargo clippy --all-targets --all-features -- -D warnings' \
		'  bench        cargo bench --features all' \
		'  run          cargo run --features all -- $$(ARGS)' \
		'  fuzz         Run one fuzz target with inferred corpus and dict' \
		'  fuzz-all     Run all fuzz targets sequentially' \
		'  fuzz-quick   Short smoke run across all targets (default MAX_TIME=10)' \
		'  fuzz-check   Build-check fuzz targets' \
		'  fuzz-install Install cargo-fuzz' \
		'  fuzz-clean   Remove fuzz build output, artifacts, and coverage' \
		'  fuzz-list    Print supported fuzz targets' \
		'  fuzz-replay  Replay saved red-team commands' \
		'' \
		'Bindings:' \
		'  wasm         Build WASM package (requires wasm-pack)' \
		'  ffi          Build C shared library (libqcl.so / libqcl.dylib)' \
		'  python       Build Python wheel and install into active venv (requires maturin)' \
		'' \
		'Variables:' \
		'  TARGET       One of: $(TARGETS)' \
		'  CORPUS       Defaults to fuzz/corpus/$(TARGET)' \
		'  MAX_TIME     libFuzzer -max_total_time, default 30 seconds' \
		'  RUN_ARGS     Extra libFuzzer args, e.g. "-runs=1000 -seed=1"' \
		'  ARGS         Extra args passed to cargo run after --' \
		'  ARTIFACT_PREFIX Defaults to fuzz/artifacts/$(TARGET)/'

check:
	$(CARGO) check --features all

test:
	$(CARGO) test --features all -- --nocapture

fmt:
	$(CARGO) fmt

fmt-check:
	$(CARGO) fmt --check

clippy:
	$(CARGO) clippy --all-targets --all-features -- -D warnings

bench:
	$(CARGO) bench --features all

run:
	$(CARGO) run --features all -- $(ARGS)

# --- Binding targets ---

wasm:
	wasm-pack build --target web --release -- --features wasm

ffi:
	$(CARGO) build --lib --features ffi --release

python:
	maturin develop --features python --release

# --- Fuzz targets ---

validate-fuzz-target:
	@if [ -z "$(filter $(TARGET),$(TARGETS))" ]; then \
		printf '%s\n' "Unknown TARGET '$(TARGET)'. Choose one of: $(TARGETS)"; \
		exit 2; \
	fi

fuzz: validate-fuzz-target
	@mkdir -p "$(ARTIFACT_PREFIX)" "$(CORPUS)"
	$(CARGO_FUZZ) run $(TARGET) "$(CORPUS)" -- $(FUZZ_ARGS)

fuzz-all:
	@set -e; \
	for target in $(TARGETS); do \
		corpus="$(FUZZ_DIR)/corpus/$$target"; \
		artifact="$(FUZZ_DIR)/artifacts/$$target/"; \
		args="-artifact_prefix=$$artifact -max_total_time=$(MAX_TIME)"; \
		case " $(DICT_TARGETS) " in \
			*" $$target "*) args="$$args -dict=$(DICT)" ;; \
		esac; \
		if [ -n "$(RUN_ARGS)" ]; then \
			args="$$args $(RUN_ARGS)"; \
		fi; \
		mkdir -p "$$corpus" "$$artifact"; \
		echo "==> $(CARGO_FUZZ) run $$target $$corpus -- $$args"; \
		$(CARGO_FUZZ) run "$$target" "$$corpus" -- $$args; \
	done

fuzz-quick: MAX_TIME = 10
fuzz-quick: fuzz-all

fuzz-check:
	$(CARGO) check --manifest-path $(FUZZ_DIR)/Cargo.toml --bins

fuzz-install:
	$(CARGO) install cargo-fuzz

clean: fuzz-clean
	$(CARGO) clean

fuzz-clean:
	@if [ -z "$(FUZZ_DIR)" ] || [ "$(FUZZ_DIR)" = "/" ]; then \
		printf '%s\n' "Refusing fuzz-clean: FUZZ_DIR must be non-empty and not '/' before removing $(FUZZ_DIR)/artifacts $(FUZZ_DIR)/coverage $(FUZZ_DIR)/target"; \
		exit 2; \
	fi
	rm -rf -- "$(FUZZ_DIR)/artifacts" "$(FUZZ_DIR)/coverage" "$(FUZZ_DIR)/target"

fuzz-list:
	@printf '%s\n' $(TARGETS)

fuzz-replay:
	bash "$(FUZZ_DIR)/redteam_replay.sh"
