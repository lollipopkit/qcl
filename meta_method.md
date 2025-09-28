Meta Method Design and Implementation Plan

Goal
- Enable meta-methods by type so that:
  - String: "".len() calls the len() registered for String.
  - Custom types: let c = Custom(); c.run() calls the run() registered for Custom.
- Allow defining custom types (runtime) and registering methods via an API analogous to `impl Custom { ... }`.

Key Decisions
- Dispatch only on method call (foo.bar()) — property access remains unchanged.
- Implement a runtime method registry keyed by type name → { method_name → RustFunction }.
- Add a lightweight runtime Object value to represent custom types and their fields.
- Keep syntax unchanged: Use existing `expr.postfix` to parse `.method()` and intercept at CallExpr for dispatch.

Scope
- Core (qcl-core):
  - Add Val::Object (named runtime object with fields) to represent custom types.
  - Add val::methods registry with:
    - register_method(type_name: &str, method: &str, func: RustFunction)
    - find_method_for_val(receiver: &Val, method: &str) -> Option<RustFunction>
  - Update Expr::CallExpr to detect Access(base, field) and dispatch via registry with receiver as args[0].
- Stdlib (qcl-stdlib):
  - Register selected string functions (len, lower, upper, trim, starts_with, ends_with, contains, replace, substring, split, join) as String methods so that "hello".len() etc. work without importing modules.
  - Register core list functions as List methods: len, push, concat, join, get, first, last.
  - Register core map functions as Map methods: len, keys, values, has, get.

Non-goals (for now)
- No new language syntax for `impl` blocks or type definitions in QCL source. Registration happens via Rust API.
- No trait system at runtime (type_checker has trait scaffolding already; runtime dispatch is per-type method table).

Edge Cases & Behavior
- Property call precedence:
  1) If foo.bar yields a callable value (Closure/RustFunction), call it (existing behavior).
  2) Else, if a meta-method for type(foo) named "bar" exists, dispatch to it with receiver as first arg.
  3) Else, error: undefined method.
- Access without call (foo.bar) does NOT auto-return method function; only foo.bar() dispatches.
- For custom types, represent values as Val::Object { type_name, fields }. Method lookup uses type_name.

API Sketch
- qcl_core::val::methods:
  - fn register_method(type_name: &str, method: &str, func: RustFunction)
  - fn find_method_for_val(receiver: &Val, method: &str) -> Option<RustFunction>
- qcl_core::val::Val additions:
  - fn object(type_name: impl AsRef<str>, fields: HashMap<String, Val>) -> Val

Implementation TODOs
1) Add Val::Object variant and helpers
   - Clone/Display/Serialize/PartialEq/access adjustments
   - Helper constructor: Val::object()
2) Add val::methods module with registry (Lazy<Mutex<HashMap<..>>>)
   - register_method, find_method_for_val
3) Update evaluator dispatch in Expr::CallExpr
   - Detect Access(base, field) + Call; attempt property-as-function; else registry
4) Wire up stdlib string methods to registry (type "String")
   - Also add stdlib list/map modules that register their common methods
5) Tests
   - "hello".len() == 5
   - Other string method samples (e.g., "HELLO".lower())
   - Custom object: register type "Custom" with run(); env.define("c", Val::object("Custom", {})); evaluate c.run()
   - List/Map:
     - [1,2,3].len() == 3
     - ["a","b"].push("c").join(",") == "a,b,c"
     - {"a":1}.len() == 1; m.has("a"); m.get("b") == nil

Follow-ups (Future Work)
- Optional: expose builder-style registration for List/Map/Bool/Int/Float methods.
- Optional: enable returning a bound method function on property access (foo.bar) by returning a callable wrapping the receiver.
- Optional: tie runtime registry to type checker’s TypeRegistry for richer validation and tooling.
