//! Module inclusion checking for ReScript.
//!
//! This module implements signature matching, which verifies that a module
//! implementation conforms to a module signature. This is equivalent to
//! OCaml's `includemod.ml`.
//!
//! The main function is `modtypes` which checks if one module type is
//! a subtype of another.

#![allow(unused_variables)]
#![allow(dead_code)]

use crate::ident::Ident;
use crate::types::ctype::{self, UnifyState};
use crate::types::decl::{
    ExtensionConstructor, ModtypeDeclaration, ModuleType, Signature, SignatureItem,
    TypeDeclaration, ValueDescription,
};
use crate::types::env::Env;
use crate::types::typedtree::ModuleCoercion;
use crate::types::{TypeContext, TypeExprRef};

// ============================================================================
// Error Types
// ============================================================================

/// Result type for inclusion checking.
pub type IncludeModResult<T> = Result<T, IncludeModError>;

/// Errors that can occur during module inclusion checking.
#[derive(Debug, Clone)]
pub enum IncludeModError {
    /// Missing value in implementation.
    MissingValue(Ident),
    /// Missing type in implementation.
    MissingType(Ident),
    /// Missing module in implementation.
    MissingModule(Ident),
    /// Missing module type in implementation.
    MissingModuleType(Ident),
    /// Value type mismatch.
    ValueTypeMismatch {
        name: Ident,
        expected: TypeExprRef,
        found: TypeExprRef,
    },
    /// Type declaration mismatch.
    TypeMismatch { name: Ident, reason: String },
    /// Module type mismatch.
    ModuleTypeMismatch { name: Ident, reason: String },
    /// Functor parameter mismatch.
    FunctorParamMismatch(String),
    /// Functor result mismatch.
    FunctorResultMismatch(String),
    /// Not a functor.
    NotAFunctor,
    /// Signature mismatch with details.
    SignatureMismatch(Vec<IncludeModError>),
}

// ============================================================================
// Module Type Inclusion
// ============================================================================

/// Check if `mty_impl` is a subtype of `mty_sig` (implementation matches signature).
///
/// Returns a coercion that can be applied to convert the implementation
/// to the signature type.
pub fn modtypes<'a>(
    type_ctx: &'a TypeContext<'a>,
    env: &Env,
    mty_impl: &ModuleType,
    mty_sig: &ModuleType,
) -> IncludeModResult<ModuleCoercion> {
    modtypes_inner(type_ctx, env, mty_impl, mty_sig, false)
}

/// Check if `mty_impl` is a subtype of `mty_sig` with optional aliasability.
fn modtypes_inner<'a>(
    type_ctx: &'a TypeContext<'a>,
    env: &Env,
    mty_impl: &ModuleType,
    mty_sig: &ModuleType,
    aliasable: bool,
) -> IncludeModResult<ModuleCoercion> {
    match (mty_impl, mty_sig) {
        // Identical module type identifiers
        (ModuleType::MtyIdent(p1), ModuleType::MtyIdent(p2)) if p1 == p2 => {
            Ok(ModuleCoercion::Tcoerce_none)
        }

        // Alias on the implementation side - expand and check
        (ModuleType::MtyAlias(_, path), mty_sig) => {
            // In a full implementation, we would expand the alias here
            // For now, we treat aliases as opaque
            Ok(ModuleCoercion::Tcoerce_none)
        }

        // Alias on the signature side - expand and check
        (mty_impl, ModuleType::MtyAlias(_, path)) => {
            // In a full implementation, we would expand the alias here
            Ok(ModuleCoercion::Tcoerce_none)
        }

        // Signature matching
        (ModuleType::MtySignature(sig_impl), ModuleType::MtySignature(sig_sig)) => {
            signatures(type_ctx, env, sig_impl, sig_sig)
        }

        // Functor matching
        (
            ModuleType::MtyFunctor {
                param: param_impl,
                param_type: param_type_impl,
                result: result_impl,
            },
            ModuleType::MtyFunctor {
                param: param_sig,
                param_type: param_type_sig,
                result: result_sig,
            },
        ) => {
            // Check parameter (contravariant)
            let param_coercion = match (param_type_impl, param_type_sig) {
                (Some(pt_impl), Some(pt_sig)) => {
                    // Note: parameters are contravariant, so we swap impl/sig
                    modtypes_inner(type_ctx, env, pt_sig, pt_impl, true)?
                }
                (None, None) => ModuleCoercion::Tcoerce_none,
                _ => {
                    return Err(IncludeModError::FunctorParamMismatch(
                        "parameter presence mismatch".to_string(),
                    ));
                }
            };

            // Check result (covariant)
            // In a full implementation, we would extend the environment with the parameter
            let result_coercion = modtypes_inner(type_ctx, env, result_impl, result_sig, false)?;

            Ok(ModuleCoercion::Tcoerce_functor(
                Box::new(param_coercion),
                Box::new(result_coercion),
            ))
        }

        // Signature implementation matches identifier signature
        (ModuleType::MtySignature(sig_impl), ModuleType::MtyIdent(path)) => {
            // Look up the signature in the environment
            // For now, we treat this as compatible
            Ok(ModuleCoercion::Tcoerce_none)
        }

        // Identifier implementation matches signature
        (ModuleType::MtyIdent(path), ModuleType::MtySignature(sig_sig)) => {
            // Look up and expand the module type
            // For now, we treat this as compatible
            Ok(ModuleCoercion::Tcoerce_none)
        }

        // Fallback: incompatible module types
        _ => Err(IncludeModError::ModuleTypeMismatch {
            name: Ident::create_local("module"),
            reason: "incompatible module types".to_string(),
        }),
    }
}

// ============================================================================
// Signature Inclusion
// ============================================================================

/// Check if signature `sig_impl` is a subtype of `sig_sig`.
pub fn signatures<'a>(
    type_ctx: &'a TypeContext<'a>,
    env: &Env,
    sig_impl: &Signature,
    sig_sig: &Signature,
) -> IncludeModResult<ModuleCoercion> {
    let mut errors = Vec::new();
    let mut coercions = Vec::new();

    // For each item in the signature, find the corresponding item in the implementation
    for (pos, sig_item) in sig_sig.iter().enumerate() {
        match signature_item(type_ctx, env, sig_impl, sig_item) {
            Ok((impl_pos, coercion)) => {
                coercions.push((impl_pos, coercion));
            }
            Err(err) => {
                errors.push(err);
            }
        }
    }

    if errors.is_empty() {
        if coercions
            .iter()
            .all(|(_, c)| matches!(c, ModuleCoercion::Tcoerce_none))
        {
            Ok(ModuleCoercion::Tcoerce_none)
        } else {
            Ok(ModuleCoercion::Tcoerce_structure(coercions, vec![]))
        }
    } else {
        Err(IncludeModError::SignatureMismatch(errors))
    }
}

/// Check a single signature item against an implementation signature.
fn signature_item<'a>(
    type_ctx: &'a TypeContext<'a>,
    env: &Env,
    sig_impl: &Signature,
    sig_item: &SignatureItem,
) -> IncludeModResult<(usize, ModuleCoercion)> {
    match sig_item {
        SignatureItem::SigValue(id, val_desc) => {
            // Find the value in the implementation
            for (pos, impl_item) in sig_impl.iter().enumerate() {
                if let SignatureItem::SigValue(impl_id, impl_val_desc) = impl_item {
                    if impl_id.name() == id.name() {
                        // Check that the types are compatible
                        let coercion =
                            value_descriptions(type_ctx, env, id.clone(), impl_val_desc, val_desc)?;
                        return Ok((pos, coercion));
                    }
                }
            }
            Err(IncludeModError::MissingValue(id.clone()))
        }

        SignatureItem::SigType(id, type_decl, rec_status) => {
            // Find the type in the implementation
            for (pos, impl_item) in sig_impl.iter().enumerate() {
                if let SignatureItem::SigType(impl_id, impl_type_decl, _) = impl_item {
                    if impl_id.name() == id.name() {
                        // Check that the type declarations are compatible
                        type_declarations(type_ctx, env, id.clone(), impl_type_decl, type_decl)?;
                        return Ok((pos, ModuleCoercion::Tcoerce_none));
                    }
                }
            }
            Err(IncludeModError::MissingType(id.clone()))
        }

        SignatureItem::SigTypext(id, ext_constr, ext_status) => {
            // Find the extension constructor in the implementation
            for (pos, impl_item) in sig_impl.iter().enumerate() {
                if let SignatureItem::SigTypext(impl_id, impl_ext_constr, _) = impl_item {
                    if impl_id.name() == id.name() {
                        // Check that the extension constructors are compatible
                        extension_constructors(
                            type_ctx,
                            env,
                            id.clone(),
                            impl_ext_constr,
                            ext_constr,
                        )?;
                        return Ok((pos, ModuleCoercion::Tcoerce_none));
                    }
                }
            }
            Err(IncludeModError::MissingValue(id.clone()))
        }

        SignatureItem::SigModule(id, mod_decl, rec_status) => {
            // Find the module in the implementation
            for (pos, impl_item) in sig_impl.iter().enumerate() {
                if let SignatureItem::SigModule(impl_id, impl_mod_decl, _) = impl_item {
                    if impl_id.name() == id.name() {
                        // Check that the module types are compatible
                        let coercion =
                            modtypes(type_ctx, env, &impl_mod_decl.md_type, &mod_decl.md_type)?;
                        return Ok((pos, coercion));
                    }
                }
            }
            Err(IncludeModError::MissingModule(id.clone()))
        }

        SignatureItem::SigModtype(id, modtype_decl) => {
            // Find the module type in the implementation
            for (pos, impl_item) in sig_impl.iter().enumerate() {
                if let SignatureItem::SigModtype(impl_id, impl_modtype_decl) = impl_item {
                    if impl_id.name() == id.name() {
                        // Check that the module type declarations are compatible
                        modtype_declarations(
                            type_ctx,
                            env,
                            id.clone(),
                            impl_modtype_decl,
                            modtype_decl,
                        )?;
                        return Ok((pos, ModuleCoercion::Tcoerce_none));
                    }
                }
            }
            Err(IncludeModError::MissingModuleType(id.clone()))
        }

        // Class and class type are not used in ReScript
        SignatureItem::SigClass | SignatureItem::SigClassType => {
            Ok((0, ModuleCoercion::Tcoerce_none))
        }
    }
}

// ============================================================================
// Component Inclusion
// ============================================================================

/// Check if value description `impl_desc` is a subtype of `sig_desc`.
fn value_descriptions<'a>(
    type_ctx: &'a TypeContext<'a>,
    env: &Env,
    id: Ident,
    impl_desc: &ValueDescription,
    sig_desc: &ValueDescription,
) -> IncludeModResult<ModuleCoercion> {
    // Check that the value types are compatible
    // The implementation type should be more general than (or equal to) the signature type
    let mut unify_state = UnifyState::default();

    // Instantiate both types
    let impl_ty = ctype::instance(type_ctx, impl_desc.val_type);
    let sig_ty = ctype::instance(type_ctx, sig_desc.val_type);

    // Try to unify (signature type should match implementation type)
    if ctype::unify(type_ctx, &mut unify_state, impl_ty, sig_ty).is_err() {
        return Err(IncludeModError::ValueTypeMismatch {
            name: id,
            expected: sig_desc.val_type,
            found: impl_desc.val_type,
        });
    }

    Ok(ModuleCoercion::Tcoerce_none)
}

/// Check if type declaration `impl_decl` is a subtype of `sig_decl`.
fn type_declarations<'a>(
    type_ctx: &'a TypeContext<'a>,
    env: &Env,
    id: Ident,
    impl_decl: &TypeDeclaration,
    sig_decl: &TypeDeclaration,
) -> IncludeModResult<()> {
    // Check arity (number of type parameters)
    if impl_decl.type_params.len() != sig_decl.type_params.len() {
        return Err(IncludeModError::TypeMismatch {
            name: id,
            reason: format!(
                "arity mismatch: expected {} type parameters, found {}",
                sig_decl.type_params.len(),
                impl_decl.type_params.len()
            ),
        });
    }

    // Check variance
    if impl_decl.type_variance.len() == sig_decl.type_variance.len() {
        for (i, (impl_var, sig_var)) in impl_decl
            .type_variance
            .iter()
            .zip(sig_decl.type_variance.iter())
            .enumerate()
        {
            // Implementation variance should be at least as specific as signature variance
            if !impl_var.subset(sig_var) {
                return Err(IncludeModError::TypeMismatch {
                    name: id,
                    reason: format!("variance mismatch at parameter {}", i),
                });
            }
        }
    }

    // Check that the type kinds are compatible
    match (&impl_decl.type_kind, &sig_decl.type_kind) {
        // Abstract in signature allows any implementation
        (_, crate::types::decl::TypeKind::TypeAbstract) => Ok(()),

        // Same kind is fine
        (k1, k2) if std::mem::discriminant(k1) == std::mem::discriminant(k2) => {
            // Would need deeper checking for record/variant fields
            Ok(())
        }

        _ => Err(IncludeModError::TypeMismatch {
            name: id,
            reason: "incompatible type kinds".to_string(),
        }),
    }
}

/// Check if extension constructor `impl_ext` is compatible with `sig_ext`.
fn extension_constructors<'a>(
    type_ctx: &'a TypeContext<'a>,
    env: &Env,
    id: Ident,
    impl_ext: &ExtensionConstructor,
    sig_ext: &ExtensionConstructor,
) -> IncludeModResult<()> {
    // Check that the extension type paths match
    if impl_ext.ext_type_path != sig_ext.ext_type_path {
        return Err(IncludeModError::TypeMismatch {
            name: id,
            reason: "extension type path mismatch".to_string(),
        });
    }

    // Check that the argument types match
    // This would require deeper type checking in a full implementation
    Ok(())
}

/// Check if module type declaration `impl_decl` is compatible with `sig_decl`.
fn modtype_declarations<'a>(
    type_ctx: &'a TypeContext<'a>,
    env: &Env,
    id: Ident,
    impl_decl: &ModtypeDeclaration,
    sig_decl: &ModtypeDeclaration,
) -> IncludeModResult<()> {
    match (&impl_decl.mtd_type, &sig_decl.mtd_type) {
        // Both abstract - compatible
        (None, None) => Ok(()),

        // Signature is abstract - implementation can be anything
        (Some(_), None) => Ok(()),

        // Signature has type but implementation is abstract - error
        (None, Some(_)) => Err(IncludeModError::ModuleTypeMismatch {
            name: id,
            reason: "implementation is abstract but signature requires concrete type".to_string(),
        }),

        // Both have types - check compatibility (bidirectional)
        (Some(impl_mty), Some(sig_mty)) => {
            // Module types must be equivalent (both directions)
            modtypes(type_ctx, env, impl_mty, sig_mty)?;
            modtypes(type_ctx, env, sig_mty, impl_mty)?;
            Ok(())
        }
    }
}

// ============================================================================
// Utilities
// ============================================================================

/// Check if a signature is empty.
pub fn is_empty_signature(sig: &Signature) -> bool {
    sig.is_empty()
}

/// Get all value identifiers from a signature.
pub fn signature_values(sig: &Signature) -> Vec<&Ident> {
    sig.iter()
        .filter_map(|item| {
            if let SignatureItem::SigValue(id, _) = item {
                Some(id)
            } else {
                None
            }
        })
        .collect()
}

/// Get all type identifiers from a signature.
pub fn signature_types(sig: &Signature) -> Vec<&Ident> {
    sig.iter()
        .filter_map(|item| {
            if let SignatureItem::SigType(id, _, _) = item {
                Some(id)
            } else {
                None
            }
        })
        .collect()
}

/// Get all module identifiers from a signature.
pub fn signature_modules(sig: &Signature) -> Vec<&Ident> {
    sig.iter()
        .filter_map(|item| {
            if let SignatureItem::SigModule(id, _, _) = item {
                Some(id)
            } else {
                None
            }
        })
        .collect()
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::IdGenerator;
    use crate::location::Location;
    use crate::types::decl::{RecStatus, TypeKind, UnboxedStatus, ValueKind};
    use crate::types::env::initial_env;

    #[test]
    fn test_empty_signatures_match() {
        let id_gen = IdGenerator::new();
        let type_ctx = TypeContext::new(&id_gen);
        let env = initial_env(&type_ctx);

        let sig_impl: Signature = vec![];
        let sig_sig: Signature = vec![];

        let result = signatures(&type_ctx, &env, &sig_impl, &sig_sig);
        assert!(result.is_ok());
    }

    #[test]
    fn test_missing_value_error() {
        let id_gen = IdGenerator::new();
        let type_ctx = TypeContext::new(&id_gen);
        let env = initial_env(&type_ctx);

        let sig_impl: Signature = vec![];
        let val_ty = type_ctx.new_var(None);
        let sig_sig: Signature = vec![SignatureItem::SigValue(
            Ident::create_local("x"),
            ValueDescription {
                val_type: val_ty,
                val_kind: ValueKind::ValReg,
                val_loc: Location::none(),
                val_attributes: vec![],
            },
        )];

        let result = signatures(&type_ctx, &env, &sig_impl, &sig_sig);
        assert!(matches!(result, Err(IncludeModError::SignatureMismatch(_))));
    }

    #[test]
    fn test_value_match() {
        let id_gen = IdGenerator::new();
        let type_ctx = TypeContext::new(&id_gen);
        let env = initial_env(&type_ctx);

        let val_ty = type_ctx.new_var(None);
        let sig_impl: Signature = vec![SignatureItem::SigValue(
            Ident::create_local("x"),
            ValueDescription {
                val_type: val_ty,
                val_kind: ValueKind::ValReg,
                val_loc: Location::none(),
                val_attributes: vec![],
            },
        )];
        let sig_sig: Signature = vec![SignatureItem::SigValue(
            Ident::create_local("x"),
            ValueDescription {
                val_type: val_ty,
                val_kind: ValueKind::ValReg,
                val_loc: Location::none(),
                val_attributes: vec![],
            },
        )];

        let result = signatures(&type_ctx, &env, &sig_impl, &sig_sig);
        assert!(result.is_ok());
    }

    #[test]
    fn test_functor_match() {
        let id_gen = IdGenerator::new();
        let type_ctx = TypeContext::new(&id_gen);
        let env = initial_env(&type_ctx);

        let param_sig = ModuleType::MtySignature(vec![]);
        let result_sig = ModuleType::MtySignature(vec![]);

        let mty_impl = ModuleType::MtyFunctor {
            param: Ident::create_local("X"),
            param_type: Some(Box::new(param_sig.clone())),
            result: Box::new(result_sig.clone()),
        };

        let mty_sig = ModuleType::MtyFunctor {
            param: Ident::create_local("X"),
            param_type: Some(Box::new(param_sig)),
            result: Box::new(result_sig),
        };

        let result = modtypes(&type_ctx, &env, &mty_impl, &mty_sig);
        assert!(result.is_ok());
    }

    #[test]
    fn test_signature_utilities() {
        let val_ty_placeholder = crate::types::TypeExprRef(0);

        let sig: Signature = vec![
            SignatureItem::SigValue(
                Ident::create_local("x"),
                ValueDescription {
                    val_type: val_ty_placeholder,
                    val_kind: ValueKind::ValReg,
                    val_loc: Location::none(),
                    val_attributes: vec![],
                },
            ),
            SignatureItem::SigType(
                Ident::create_local("t"),
                TypeDeclaration {
                    type_params: vec![],
                    type_arity: 0,
                    type_kind: TypeKind::TypeAbstract,
                    type_private: crate::types::PrivateFlag::Public,
                    type_manifest: None,
                    type_variance: vec![],
                    type_newtype_level: None,
                    type_loc: Location::none(),
                    type_attributes: vec![],
                    type_immediate: false,
                    type_unboxed: UnboxedStatus::FALSE_FALSE,
                    type_inlined_types: vec![],
                },
                RecStatus::TrecNot,
            ),
        ];

        assert!(!is_empty_signature(&sig));
        assert_eq!(signature_values(&sig).len(), 1);
        assert_eq!(signature_types(&sig).len(), 1);
        assert_eq!(signature_modules(&sig).len(), 0);
    }
}
