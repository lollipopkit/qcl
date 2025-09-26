use std::collections::HashMap;
use anyhow::{Result, anyhow};
use crate::val::{Type, Val};

/// Trait definition with method signatures
#[derive(Debug, Clone, PartialEq)]
pub struct TraitDef {
    pub name: String,
    pub methods: HashMap<String, Type>, // method_name -> function_type
}

/// Implementation of a trait for a specific type
#[derive(Debug, Clone, PartialEq)]
pub struct TraitImpl {
    pub trait_name: String,
    pub target_type: Type,
    pub methods: HashMap<String, Val>, // method_name -> function_value
}

/// Type alias definition
#[derive(Debug, Clone, PartialEq)]
pub struct TypeAlias {
    pub name: String,
    pub target_type: Type,
}

/// Registry for managing custom types, traits, and implementations
#[derive(Debug, Clone, Default, PartialEq)]
pub struct TypeRegistry {
    /// Type aliases: type UserId = Int
    type_aliases: HashMap<String, TypeAlias>,

    /// Trait definitions
    traits: HashMap<String, TraitDef>,

    /// Trait implementations per type
    implementations: HashMap<String, Vec<TraitImpl>>, // type_name -> implementations

    /// Type variable counter for fresh variable generation
    type_var_counter: u32,
}

impl TypeRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a type alias
    pub fn register_type_alias(&mut self, alias: TypeAlias) {
        self.type_aliases.insert(alias.name.clone(), alias);
    }

    /// Register a trait definition
    pub fn register_trait(&mut self, trait_def: TraitDef) {
        self.traits.insert(trait_def.name.clone(), trait_def);
    }

    /// Register a trait implementation
    pub fn register_trait_impl(&mut self, impl_def: TraitImpl) {
        let type_name = Self::type_to_string(&impl_def.target_type);
        self.implementations
            .entry(type_name)
            .or_default()
            .push(impl_def);
    }

    /// Resolve a named type to its concrete type
    pub fn resolve_type(&self, name: &str) -> Option<Type> {
        // Check if it's a type alias
        if let Some(alias) = self.type_aliases.get(name) {
            return Some(alias.target_type.clone());
        }

        // Check if it's a trait (traits can be used as types in some contexts)
        if self.traits.contains_key(name) {
            return Some(Type::Named(name.to_string()));
        }

        None
    }

    /// Check if a type implements a trait
    pub fn implements_trait(&self, typ: &Type, trait_name: &str) -> bool {
        let type_name = Self::type_to_string(typ);
        if let Some(impls) = self.implementations.get(&type_name) {
            impls.iter().any(|impl_def| impl_def.trait_name == trait_name)
        } else {
            false
        }
    }

    /// Get the method implementation for a type and method name
    pub fn get_method(&self, typ: &Type, method_name: &str) -> Option<&Val> {
        let type_name = Self::type_to_string(typ);
        if let Some(impls) = self.implementations.get(&type_name) {
            for impl_def in impls {
                if let Some(method) = impl_def.methods.get(method_name) {
                    return Some(method);
                }
            }
        }
        None
    }

    /// Generate a fresh type variable
    pub fn fresh_type_var(&mut self) -> Type {
        let var_name = format!("T{}", self.type_var_counter);
        self.type_var_counter += 1;
        Type::Variable(var_name)
    }

    /// Convert a type to a string representation for indexing
    fn type_to_string(typ: &Type) -> String {
        match typ {
            Type::Named(name) => name.clone(),
            Type::Int => "Int".to_string(),
            Type::Float => "Float".to_string(),
            Type::String => "String".to_string(),
            Type::Bool => "Bool".to_string(),
            Type::Nil => "Nil".to_string(),
            Type::List(inner) => format!("List<{}>", Self::type_to_string(inner)),
            Type::Map(k, v) => format!("Map<{}, {}>", Self::type_to_string(k), Self::type_to_string(v)),
            Type::Function { .. } => "Function".to_string(),
            Type::Task(inner) => format!("Task<{}>", Self::type_to_string(inner)),
            Type::Channel(inner) => format!("Channel<{}>", Self::type_to_string(inner)),
            Type::Union(types) => {
                let type_names: Vec<String> = types.iter().map(Self::type_to_string).collect();
                format!("({})", type_names.join(" | "))
            }
            Type::Optional(inner) => format!("?{}", Self::type_to_string(inner)),
            Type::Variable(name) => format!("'{}", name),
            Type::Generic { name, params } => {
                if params.is_empty() {
                    name.clone()
                } else {
                    let param_names: Vec<String> = params.iter().map(Self::type_to_string).collect();
                    format!("{}<{}>", name, param_names.join(", "))
                }
            }
            Type::Any => "Any".to_string(),
        }
    }

    /// Validate that a trait implementation is correct
    pub fn validate_trait_impl(&self, impl_def: &TraitImpl) -> Result<()> {
        // Check that the trait exists
        let trait_def = self.traits.get(&impl_def.trait_name)
            .ok_or_else(|| anyhow!("Trait '{}' not found", impl_def.trait_name))?;

        // Check that all required methods are implemented
        for method_name in trait_def.methods.keys() {
            if let Some(_impl_method) = impl_def.methods.get(method_name) {
                // TODO: Check that the implementation method matches the expected type
                // This requires type checking logic
            } else {
                return Err(anyhow!(
                    "Method '{}' required by trait '{}' not implemented for type '{}'",
                    method_name,
                    impl_def.trait_name,
                    Self::type_to_string(&impl_def.target_type)
                ));
            }
        }

        Ok(())
    }
}

/// Type inference engine using unification
#[derive(Debug, Clone, PartialEq)]
pub struct TypeInferenceEngine {
    /// Current substitutions for type variables
    substitutions: HashMap<String, Type>,

    /// Constraints to be solved
    constraints: Vec<(Type, Type)>,

    /// Registry for custom types
    registry: TypeRegistry,
}

impl TypeInferenceEngine {
    pub fn new(registry: TypeRegistry) -> Self {
        Self {
            substitutions: HashMap::new(),
            constraints: Vec::new(),
            registry,
        }
    }

    /// Generate a fresh type variable
    pub fn fresh_type_var(&mut self) -> Type {
        self.registry.fresh_type_var()
    }

    /// Add a constraint that two types must be equal
    pub fn add_constraint(&mut self, t1: Type, t2: Type) {
        self.constraints.push((t1, t2));
    }

    /// Solve all constraints using unification
    pub fn solve_constraints(&mut self) -> Result<HashMap<String, Type>> {
        while let Some((t1, t2)) = self.constraints.pop() {
            self.unify(t1, t2)?;
        }
        Ok(self.substitutions.clone())
    }

    /// Unify two types
    fn unify(&mut self, t1: Type, t2: Type) -> Result<()> {
        let t1 = self.apply_substitution(&t1);
        let t2 = self.apply_substitution(&t2);

        match (&t1, &t2) {
            // Same types unify
            (a, b) if a == b => Ok(()),

            // Variable unification
            (Type::Variable(var), typ) | (typ, Type::Variable(var)) => {
                if Self::occurs_check(var, typ) {
                    Err(anyhow!("Occurs check failed: {} occurs in {}", var, typ.display()))
                } else {
                    // Apply the new substitution to existing substitutions
                    let new_substitution = typ.clone();
                    let mut updated_substitutions = HashMap::new();
                    for (existing_var, existing_type) in &self.substitutions {
                        let updated_type = existing_type.substitute(&[(var.clone(), new_substitution.clone())].into_iter().collect());
                        updated_substitutions.insert(existing_var.clone(), updated_type);
                    }
                    // Apply to the substitution itself recursively
                    let final_substitution = new_substitution.substitute(&updated_substitutions);

                    // Update all substitutions
                    for (k, v) in updated_substitutions {
                        self.substitutions.insert(k, v);
                    }
                    self.substitutions.insert(var.clone(), final_substitution);
                    Ok(())
                }
            }

            // Structural unification
            (Type::List(a), Type::List(b)) => self.unify((**a).clone(), (**b).clone()),
            (Type::Map(ak, av), Type::Map(bk, bv)) => {
                self.unify((**ak).clone(), (**bk).clone())?;
                self.unify((**av).clone(), (**bv).clone())
            }
            (Type::Function { params: a_params, return_type: a_ret },
             Type::Function { params: b_params, return_type: b_ret }) => {
                if a_params.len() != b_params.len() {
                    return Err(anyhow!("Function arity mismatch"));
                }
                for (a_param, b_param) in a_params.iter().zip(b_params.iter()) {
                    self.unify(a_param.clone(), b_param.clone())?;
                }
                self.unify((**a_ret).clone(), (**b_ret).clone())
            }
            (Type::Optional(a), Type::Optional(b)) => {
                self.unify((**a).clone(), (**b).clone())
            }
            (Type::Task(a), Type::Task(b)) => {
                self.unify((**a).clone(), (**b).clone())
            }
            (Type::Channel(a), Type::Channel(b)) => {
                self.unify((**a).clone(), (**b).clone())
            }

            // Union type unification (simplified)
            (Type::Union(types), t) | (t, Type::Union(types)) => {
                // For now, just check if t is assignable to any member of the union
                for union_type in types {
                    if t.is_assignable_to(union_type) {
                        return Ok(());
                    }
                }
                Err(anyhow!("Cannot unify {} with union type", t.display()))
            }

            // Generic type unification
            (Type::Generic { name: a_name, params: a_params },
             Type::Generic { name: b_name, params: b_params }) => {
                if a_name != b_name || a_params.len() != b_params.len() {
                    return Err(anyhow!("Generic type mismatch"));
                }
                for (a_param, b_param) in a_params.iter().zip(b_params.iter()) {
                    self.unify(a_param.clone(), b_param.clone())?;
                }
                Ok(())
            }

            // Type mismatch
            _ => Err(anyhow!("Cannot unify {} with {}", t1.display(), t2.display()))
        }
    }

    /// Apply current substitutions to a type
    fn apply_substitution(&self, typ: &Type) -> Type {
        typ.substitute(&self.substitutions)
    }

    /// Occurs check to prevent infinite types
    fn occurs_check(var: &str, typ: &Type) -> bool {
        match typ {
            Type::Variable(v) => v == var,
            Type::List(inner) | Type::Optional(inner) | Type::Task(inner) | Type::Channel(inner) => {
                Self::occurs_check(var, inner)
            }
            Type::Map(k, v) => Self::occurs_check(var, k) || Self::occurs_check(var, v),
            Type::Function { params, return_type } => {
                params.iter().any(|p| Self::occurs_check(var, p)) || Self::occurs_check(var, return_type)
            }
            Type::Union(types) => types.iter().any(|t| Self::occurs_check(var, t)),
            Type::Generic { params, .. } => params.iter().any(|p| Self::occurs_check(var, p)),
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_registry() {
        let mut registry = TypeRegistry::new();

        // Register a type alias
        let alias = TypeAlias {
            name: "UserId".to_string(),
            target_type: Type::Int,
        };
        registry.register_type_alias(alias);

        assert_eq!(registry.resolve_type("UserId"), Some(Type::Int));
        assert_eq!(registry.resolve_type("UnknownType"), None);
    }

    #[test]
    fn test_trait_system() {
        let mut registry = TypeRegistry::new();

        // Define a trait
        let mut methods = HashMap::new();
        methods.insert("display".to_string(), Type::Function {
            params: vec![],
            return_type: Box::new(Type::String),
        });

        let trait_def = TraitDef {
            name: "Display".to_string(),
            methods,
        };
        registry.register_trait(trait_def);

        assert!(registry.traits.contains_key("Display"));
    }

    #[test]
    fn test_type_inference() {
        let registry = TypeRegistry::new();
        let mut engine = TypeInferenceEngine::new(registry);

        let var1 = engine.fresh_type_var();
        let var2 = engine.fresh_type_var();

        // Add constraint: T0 = Int
        engine.add_constraint(var1.clone(), Type::Int);
        // Add constraint: T1 = T0
        engine.add_constraint(var2.clone(), var1.clone());

        let substitutions = engine.solve_constraints().unwrap();

        // Both variables should resolve to Int
        if let Type::Variable(name1) = &var1 {
            assert_eq!(substitutions.get(name1), Some(&Type::Int));
        }
        if let Type::Variable(name2) = &var2 {
            assert_eq!(substitutions.get(name2), Some(&Type::Int));
        }
    }
}