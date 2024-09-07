## QCL

Query - Check - Language -> QCL is a simple language that allows you to query and check the eval result of a query.

### Example

```qcl
@req.user.name == 'bar' && @record.published == true
```

Explanation:

- both `req` and `record` are objects that are passed to the QCL engine as [Context].
- `@req.user.name` is a query that will be evaluated to the user's name.
- `@record.published` is a query that will be evaluated to the record's published status.

Syntax in this example:

- `@` is used to query the context object.
- `.` is used to access the object's properties.
- `==` is used to check the equality of two values.
- `&&` is used to check the logical AND of two values.

### Usage

```rust
let req: Val = Req { user: User { name: "bar".to_string() } }.into();
let record: Val = Record { published: true }.into();

let context = Context::new().with("req", req).with("record", record);
let query = "@req.user.name == 'bar' && @record.published == true";

let qcl = QCL::new();
assert_eq!(qcl.eval(query, context).unwrap(), true.into());
```
