# QCL Expression Examples

This file contains simple expression examples that can be used to test the QCL language.

## Basic Context Access
```qcl
@user.name == "Alice"
```

## Role-based Access Control
```qcl
@req.user.role == "admin" || @req.user.role == "manager"
```

## Permission Checking
```qcl
@req.user.role == "admin" || @req.user.permissions["read"] == true
```

## Numeric Comparisons
```qcl
@user.age >= 18 && @user.age <= 65
```

## String Operations
```qcl
@user.email != nil && @user.email != ""
```

## List Membership
```qcl
@req.user.role in ["admin", "manager", "supervisor"]
```

## Complex Logical Expressions
```qcl
(@user.role == "admin") || 
(@user.department == @resource.department && @req.action == "read") ||
(@resource.owner == @user.id)
```

## Nested Object Access
```qcl
@user.profile.address.country == "US" && @user.profile.verified == true
```

## Mathematical Operations
```qcl
@user.score > 80 && (@user.bonus + @user.base_salary) >= 50000
```

## Time-based Access
```qcl
@context.hour >= 9 && @context.hour <= 17 && @context.weekday in [1, 2, 3, 4, 5]
```

## Resource-specific Permissions
```qcl
@resource.public == true || 
(@resource.department == @user.department) ||
(@user.id in @resource.allowed_users)
```

## Geo-location Based Access
```qcl
@user.location.country in ["US", "CA", "GB"] && 
@user.location.country not in @resource.blocked_countries
```