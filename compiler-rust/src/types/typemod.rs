//! Module type checking for ReScript.
//!
//! This module handles type checking of:
//! - Module expressions (`ModuleExpr`)
//! - Structures (module bodies)
//! - Signatures (module types)
//! - Functor application and coercion

#![allow(unused_variables)]
#![allow(dead_code)]

use crate::ident::Ident;
use crate::location::Location;
use crate::parser::ast::RecFlag;
use crate::parser::ast::{
    ModuleExpr as ParsedModuleExpr, ModuleExprDesc as ParsedModuleExprDesc,
    StructureItem as ParsedStructureItem, StructureItemDesc as ParsedStructureItemDesc,
};
use crate::parser::longident::Longident;
use crate::types::Path;
use crate::types::env::Env;
use crate::types::type_expr::TypeExprRef;
use crate::types::typecore::{TypeCheckContext, TypeCoreError, type_binding, type_expression};
use crate::types::typedtree::{
    EnvRef, Expression, IncludeDeclaration, ModuleBinding, ModuleCoercion, ModuleExpr,
    ModuleExprDesc, ModuleType, ModuleTypeDeclaration, OpenDeclaration, Structure,
    StructureItem, StructureItemDesc, TypeExtension, ValueBinding,
};

// ============================================================================
// Helper Functions for Path Conversion
// ============================================================================

/// Convert a Longident to a Path.
fn path_from_longident(lid: &Longident) -> Path {
    match lid {
        Longident::Lident(name) => Path::pident(Ident::create_local(name)),
        Longident::Ldot(prefix, name) => {
            let prefix_path = path_from_longident(prefix);
            Path::pdot(prefix_path, name.clone())
        }
        Longident::Lapply(functor, arg) => {
            let functor_path = path_from_longident(functor);
            let arg_path = path_from_longident(arg);
            Path::papply(functor_path, arg_path)
        }
    }
}

// ============================================================================
// Module Type Checking Results
// ============================================================================

/// Result type for module type checking operations.
pub type TypeModResult<T> = Result<T, TypeModError>;

/// Errors that can occur during module type checking.
#[derive(Debug, Clone)]
pub enum TypeModError {
    /// Core type checking error.
    CoreError(TypeCoreError),
    /// Module not found.
    ModuleNotFound(String),
    /// Signature mismatch.
    SignatureMismatch { expected: String, found: String },
    /// Functor application error.
    FunctorApplicationError(String),
    /// Invalid module expression.
    InvalidModuleExpr(Location),
}

impl From<TypeCoreError> for TypeModError {
    fn from(err: TypeCoreError) -> Self {
        TypeModError::CoreError(err)
    }
}

// ============================================================================
// Module Expression Type Checking
// ============================================================================

/// Type check a module expression.
pub fn type_module_expr(
    tctx: &mut TypeCheckContext<'_>,
    env: &Env,
    mod_expr: &ParsedModuleExpr,
) -> TypeModResult<ModuleExpr> {
    let loc = &mod_expr.pmod_loc;

    match &mod_expr.pmod_desc {
        ParsedModuleExprDesc::Pmod_ident(lid) => {
            // Module identifier: M
            // Look up the module path in the environment
            let path = path_from_longident(&lid.txt);

            Ok(ModuleExpr {
                mod_desc: ModuleExprDesc::Tmod_ident(path, lid.clone()),
                mod_loc: loc.clone(),
                mod_type: ModuleType::placeholder(),
                mod_env: EnvRef(0),
                mod_attributes: mod_expr.pmod_attributes.clone(),
            })
        }

        ParsedModuleExprDesc::Pmod_structure(items) => {
            // Module structure: { ... }
            let (structure, new_env) = type_structure(tctx, env, items)?;

            Ok(ModuleExpr {
                mod_desc: ModuleExprDesc::Tmod_structure(structure),
                mod_loc: loc.clone(),
                mod_type: ModuleType::placeholder(),
                mod_env: EnvRef(0),
                mod_attributes: mod_expr.pmod_attributes.clone(),
            })
        }

        ParsedModuleExprDesc::Pmod_functor(name, param_type, body) => {
            // Functor: (X: S) => ME
            let param_id = Ident::create_local(&name.txt);

            // Type check the functor body
            // In a full implementation, we would extend the environment with the parameter
            let typed_body = type_module_expr(tctx, env, body)?;

            // Translate parameter type if present
            let typed_param_type = param_type
                .as_ref()
                .map(|_| Box::new(ModuleType::placeholder()));

            Ok(ModuleExpr {
                mod_desc: ModuleExprDesc::Tmod_functor(
                    param_id,
                    name.clone(),
                    typed_param_type,
                    Box::new(typed_body),
                ),
                mod_loc: loc.clone(),
                mod_type: ModuleType::placeholder(),
                mod_env: EnvRef(0),
                mod_attributes: mod_expr.pmod_attributes.clone(),
            })
        }

        ParsedModuleExprDesc::Pmod_apply(functor, arg) => {
            // Functor application: F(M)
            let typed_functor = type_module_expr(tctx, env, functor)?;
            let typed_arg = type_module_expr(tctx, env, arg)?;

            Ok(ModuleExpr {
                mod_desc: ModuleExprDesc::Tmod_apply(
                    Box::new(typed_functor),
                    Box::new(typed_arg),
                    ModuleCoercion::Tcoerce_none,
                ),
                mod_loc: loc.clone(),
                mod_type: ModuleType::placeholder(),
                mod_env: EnvRef(0),
                mod_attributes: mod_expr.pmod_attributes.clone(),
            })
        }

        ParsedModuleExprDesc::Pmod_constraint(mod_expr_inner, mod_type) => {
            // Constrained module: (ME: MT)
            let typed_mod = type_module_expr(tctx, env, mod_expr_inner)?;

            Ok(ModuleExpr {
                mod_desc: ModuleExprDesc::Tmod_constraint(
                    Box::new(typed_mod),
                    ModuleType::placeholder(),
                    crate::types::typedtree::ModuleTypeConstraint::Tmodtype_implicit,
                    ModuleCoercion::Tcoerce_none,
                ),
                mod_loc: loc.clone(),
                mod_type: ModuleType::placeholder(),
                mod_env: EnvRef(0),
                mod_attributes: mod_expr.pmod_attributes.clone(),
            })
        }

        ParsedModuleExprDesc::Pmod_unpack(expr) => {
            // Unpack: unpack(E)
            let typed_expr = type_expression(tctx.type_ctx, env, expr)?;

            Ok(ModuleExpr {
                mod_desc: ModuleExprDesc::Tmod_unpack(typed_expr, ModuleType::placeholder()),
                mod_loc: loc.clone(),
                mod_type: ModuleType::placeholder(),
                mod_env: EnvRef(0),
                mod_attributes: mod_expr.pmod_attributes.clone(),
            })
        }

        ParsedModuleExprDesc::Pmod_extension(_ext) => {
            // Extension: [%id]
            // Extensions are typically handled by preprocessors
            Ok(ModuleExpr {
                mod_desc: ModuleExprDesc::Tmod_ident(
                    Path::pident(Ident::create_local("extension")),
                    crate::location::Located {
                        txt: crate::parser::longident::Longident::Lident("extension".to_string()),
                        loc: loc.clone(),
                    },
                ),
                mod_loc: loc.clone(),
                mod_type: ModuleType::placeholder(),
                mod_env: EnvRef(0),
                mod_attributes: mod_expr.pmod_attributes.clone(),
            })
        }
    }
}

// ============================================================================
// Structure Type Checking
// ============================================================================

/// Type check a structure (module body).
pub fn type_structure(
    tctx: &mut TypeCheckContext<'_>,
    env: &Env,
    items: &[ParsedStructureItem],
) -> TypeModResult<(Structure, Env)> {
    let mut typed_items = Vec::new();
    let mut current_env = env.clone();

    for item in items {
        let (typed_item, new_env) = type_structure_item(tctx, &current_env, item)?;
        typed_items.push(typed_item);
        current_env = new_env;
    }

    let structure = Structure {
        str_items: typed_items,
        str_type: vec![], // Signature would be computed here
        str_final_env: EnvRef(0),
    };

    Ok((structure, current_env))
}

/// Type check a structure item.
fn type_structure_item(
    tctx: &mut TypeCheckContext<'_>,
    env: &Env,
    item: &ParsedStructureItem,
) -> TypeModResult<(StructureItem, Env)> {
    let loc = &item.pstr_loc;

    match &item.pstr_desc {
        ParsedStructureItemDesc::Pstr_eval(expr, attrs) => {
            // Evaluated expression: E
            let typed_expr = type_expression(tctx.type_ctx, env, expr)?;

            let desc = StructureItemDesc::Tstr_eval(typed_expr, attrs.clone());
            Ok((
                StructureItem {
                    str_desc: desc,
                    str_loc: loc.clone(),
                    str_env: EnvRef(0),
                },
                env.clone(),
            ))
        }

        ParsedStructureItemDesc::Pstr_value(rec_flag, bindings) => {
            // Value binding: let P = E
            let typed_bindings = type_value_bindings(tctx, env, *rec_flag, bindings)?;

            // Extend environment with new bindings
            let new_env = extend_env_with_bindings(env, &typed_bindings);

            let desc = StructureItemDesc::Tstr_value(*rec_flag, typed_bindings);
            Ok((
                StructureItem {
                    str_desc: desc,
                    str_loc: loc.clone(),
                    str_env: EnvRef(0),
                },
                new_env,
            ))
        }

        ParsedStructureItemDesc::Pstr_primitive(_vd) => {
            // Primitive declaration: external f : t = "..."
            // Full implementation would register the primitive
            let desc = StructureItemDesc::Tstr_primitive(crate::types::ValueDescription {
                val_type: tctx.type_ctx.new_var(None),
                val_kind: crate::types::ValueKind::ValReg,
                val_loc: loc.clone(),
                val_attributes: vec![],
            });
            Ok((
                StructureItem {
                    str_desc: desc,
                    str_loc: loc.clone(),
                    str_env: EnvRef(0),
                },
                env.clone(),
            ))
        }

        ParsedStructureItemDesc::Pstr_type(rec_flag, decls) => {
            // Type declaration: type t = ...
            let ctx = tctx.type_ctx;
            let mut new_env = env.clone();
            let mut typed_decls = Vec::new();

            for decl in decls {
                let type_name = &decl.ptype_name.txt;
                let type_id = Ident::create_persistent(type_name);

                // Create the type path for self-reference
                let type_path = Path::pident(type_id.clone());

                // Convert the type kind
                let (type_kind, constructors) = match &decl.ptype_kind {
                    crate::parser::ast::TypeKind::Ptype_abstract => {
                        (crate::types::TypeKind::TypeAbstract, vec![])
                    }
                    crate::parser::ast::TypeKind::Ptype_variant(cstrs) => {
                        let mut typed_cstrs = Vec::new();
                        let mut cstr_descs = Vec::new();
                        let num_consts = cstrs.iter().filter(|c| matches!(c.pcd_args, crate::parser::ast::ConstructorArguments::Pcstr_tuple(ref args) if args.is_empty())).count() as i32;
                        let num_nonconsts = cstrs.len() as i32 - num_consts;

                        let mut const_tag = 0i32;
                        let mut block_tag = 0i32;

                        for cstr in cstrs {
                            let cstr_id = Ident::create_persistent(&cstr.pcd_name.txt);
                            let is_constant = matches!(cstr.pcd_args, crate::parser::ast::ConstructorArguments::Pcstr_tuple(ref args) if args.is_empty());

                            // Convert constructor arguments
                            let (cstr_args, cd_args) = match &cstr.pcd_args {
                                crate::parser::ast::ConstructorArguments::Pcstr_tuple(types) => {
                                    let typed_args: Vec<TypeExprRef> = types.iter().map(|_| {
                                        // For now, use a placeholder type variable
                                        ctx.new_var(None)
                                    }).collect();
                                    (typed_args.clone(), crate::types::ConstructorArguments::CstrTuple(typed_args))
                                }
                                crate::parser::ast::ConstructorArguments::Pcstr_record(_) => {
                                    // TODO: Handle inline record constructors
                                    (vec![], crate::types::ConstructorArguments::CstrTuple(vec![]))
                                }
                            };

                            // Determine the tag
                            let tag = if is_constant {
                                let t = const_tag;
                                const_tag += 1;
                                crate::types::ConstructorTag::CstrConstant(t)
                            } else {
                                let t = block_tag;
                                block_tag += 1;
                                crate::types::ConstructorTag::CstrBlock(t)
                            };

                            // Create the result type (the variant type itself)
                            let res_ty = ctx.new_constr(type_path.clone(), vec![]);

                            // Create typed constructor declaration
                            typed_cstrs.push(crate::types::ConstructorDeclaration {
                                cd_id: cstr_id.clone(),
                                cd_args,
                                cd_res: None,
                                cd_loc: cstr.pcd_loc.clone(),
                                cd_attributes: vec![],
                            });

                            // Create constructor description for the environment
                            let cstr_arity = cstr_args.len() as i32;
                            cstr_descs.push((cstr_id, crate::types::ConstructorDescription {
                                cstr_name: cstr.pcd_name.txt.clone(),
                                cstr_res: res_ty,
                                cstr_existentials: vec![],
                                cstr_args,
                                cstr_arity,
                                cstr_tag: tag,
                                cstr_consts: num_consts,
                                cstr_nonconsts: num_nonconsts,
                                cstr_generalized: false,
                                cstr_private: crate::types::PrivateFlag::Public,
                                cstr_loc: cstr.pcd_loc.clone(),
                                cstr_attributes: vec![],
                                cstr_inlined: None,
                            }));
                        }

                        (crate::types::TypeKind::TypeVariant(typed_cstrs), cstr_descs)
                    }
                    crate::parser::ast::TypeKind::Ptype_record(labels) => {
                        let typed_labels: Vec<crate::types::LabelDeclaration> = labels.iter().map(|lbl| {
                            // Convert parser MutableFlag to types MutableFlag
                            let mutable_flag = match lbl.pld_mutable {
                                crate::parser::ast::MutableFlag::Immutable => crate::types::MutableFlag::Immutable,
                                crate::parser::ast::MutableFlag::Mutable => crate::types::MutableFlag::Mutable,
                            };
                            crate::types::LabelDeclaration {
                                ld_id: Ident::create_persistent(&lbl.pld_name.txt),
                                ld_mutable: mutable_flag,
                                ld_optional: false,
                                ld_type: ctx.new_var(None), // Placeholder
                                ld_loc: lbl.pld_loc.clone(),
                                ld_attributes: vec![],
                            }
                        }).collect();
                        (crate::types::TypeKind::TypeRecord(typed_labels, crate::types::RecordRepresentation::RecordRegular), vec![])
                    }
                    crate::parser::ast::TypeKind::Ptype_open => {
                        (crate::types::TypeKind::TypeOpen, vec![])
                    }
                };

                // Create the typed type declaration
                let typed_decl = crate::types::TypeDeclaration {
                    type_params: vec![],
                    type_arity: 0,
                    type_kind,
                    type_private: crate::types::PrivateFlag::Public,
                    type_manifest: None,
                    type_variance: vec![],
                    type_newtype_level: None,
                    type_loc: decl.ptype_loc.clone(),
                    type_attributes: vec![],
                    type_immediate: false,
                    type_unboxed: crate::types::UnboxedStatus::default(),
                    type_inlined_types: vec![],
                };

                // Add type to environment
                new_env.add_type(type_id, typed_decl.clone());

                // Add constructors to environment
                for (cstr_id, cstr_desc) in constructors {
                    new_env.add_constructor(cstr_id, cstr_desc);
                }

                typed_decls.push(typed_decl);
            }

            let desc = StructureItemDesc::Tstr_type(*rec_flag, typed_decls);
            Ok((
                StructureItem {
                    str_desc: desc,
                    str_loc: loc.clone(),
                    str_env: EnvRef(0),
                },
                new_env,
            ))
        }

        ParsedStructureItemDesc::Pstr_typext(_ext) => {
            // Type extension: type t += ...
            let desc = StructureItemDesc::Tstr_typext(TypeExtension {
                tyext_path: Path::pident(Ident::create_local("placeholder")),
                tyext_txt: crate::location::Located {
                    txt: crate::parser::longident::Longident::Lident("placeholder".to_string()),
                    loc: loc.clone(),
                },
                tyext_params: vec![],
                tyext_constructors: vec![],
                tyext_private: crate::parser::ast::PrivateFlag::Public,
                tyext_loc: loc.clone(),
                tyext_attributes: vec![],
            });
            Ok((
                StructureItem {
                    str_desc: desc,
                    str_loc: loc.clone(),
                    str_env: EnvRef(0),
                },
                env.clone(),
            ))
        }

        ParsedStructureItemDesc::Pstr_exception(_ext_constr) => {
            // Exception declaration: exception C
            let desc =
                StructureItemDesc::Tstr_exception(crate::types::typedtree::ExtensionConstructor {
                    ext_id: Ident::create_local("Exception"),
                    ext_name: crate::location::Located {
                        txt: "Exception".to_string(),
                        loc: loc.clone(),
                    },
                    ext_type: crate::types::ExtensionConstructor {
                        ext_type_path: Path::pident(Ident::create_local("exn")),
                        ext_type_params: vec![],
                        ext_args: crate::types::ConstructorArguments::CstrTuple(vec![]),
                        ext_ret_type: None,
                        ext_private: crate::types::PrivateFlag::Public,
                        ext_loc: loc.clone(),
                        ext_attributes: vec![],
                        ext_is_exception: true,
                    },
                    ext_kind: crate::types::typedtree::ExtensionConstructorKind::Text_decl(
                        vec![],
                        None,
                    ),
                    ext_loc: loc.clone(),
                    ext_attributes: vec![],
                });
            Ok((
                StructureItem {
                    str_desc: desc,
                    str_loc: loc.clone(),
                    str_env: EnvRef(0),
                },
                env.clone(),
            ))
        }

        ParsedStructureItemDesc::Pstr_module(mb) => {
            // Module binding: module M = ME
            let typed_mod = type_module_expr(tctx, env, &mb.pmb_expr)?;
            let mod_id = Ident::create_local(&mb.pmb_name.txt);

            let desc = StructureItemDesc::Tstr_module(ModuleBinding {
                mb_id: mod_id,
                mb_name: mb.pmb_name.clone(),
                mb_expr: typed_mod,
                mb_attributes: mb.pmb_attributes.clone(),
                mb_loc: mb.pmb_loc.clone(),
            });
            Ok((
                StructureItem {
                    str_desc: desc,
                    str_loc: loc.clone(),
                    str_env: EnvRef(0),
                },
                env.clone(),
            ))
        }

        ParsedStructureItemDesc::Pstr_recmodule(mbs) => {
            // Recursive modules: module rec M = ...
            let mut typed_bindings = Vec::new();
            for mb in mbs {
                let typed_mod = type_module_expr(tctx, env, &mb.pmb_expr)?;
                let mod_id = Ident::create_local(&mb.pmb_name.txt);
                typed_bindings.push(ModuleBinding {
                    mb_id: mod_id,
                    mb_name: mb.pmb_name.clone(),
                    mb_expr: typed_mod,
                    mb_attributes: mb.pmb_attributes.clone(),
                    mb_loc: mb.pmb_loc.clone(),
                });
            }

            let desc = StructureItemDesc::Tstr_recmodule(typed_bindings);
            Ok((
                StructureItem {
                    str_desc: desc,
                    str_loc: loc.clone(),
                    str_env: EnvRef(0),
                },
                env.clone(),
            ))
        }

        ParsedStructureItemDesc::Pstr_modtype(mtd) => {
            // Module type declaration: module type S = MT
            let mtd_id = Ident::create_local(&mtd.pmtd_name.txt);

            let desc = StructureItemDesc::Tstr_modtype(ModuleTypeDeclaration {
                mtd_id,
                mtd_name: mtd.pmtd_name.clone(),
                mtd_type: mtd.pmtd_type.as_ref().map(|_| ModuleType::placeholder()),
                mtd_attributes: mtd.pmtd_attributes.clone(),
                mtd_loc: mtd.pmtd_loc.clone(),
            });
            Ok((
                StructureItem {
                    str_desc: desc,
                    str_loc: loc.clone(),
                    str_env: EnvRef(0),
                },
                env.clone(),
            ))
        }

        ParsedStructureItemDesc::Pstr_open(open_desc) => {
            // Open declaration: open M
            let path = path_from_longident(&open_desc.popen_lid.txt);

            let desc = StructureItemDesc::Tstr_open(OpenDeclaration {
                open_expr: ModuleExpr {
                    mod_desc: ModuleExprDesc::Tmod_ident(path, open_desc.popen_lid.clone()),
                    mod_loc: loc.clone(),
                    mod_type: ModuleType::placeholder(),
                    mod_env: EnvRef(0),
                    mod_attributes: vec![],
                },
                open_override: open_desc.popen_override,
                open_env: EnvRef(0),
                open_attributes: open_desc.popen_attributes.clone(),
                open_loc: loc.clone(),
            });
            Ok((
                StructureItem {
                    str_desc: desc,
                    str_loc: loc.clone(),
                    str_env: EnvRef(0),
                },
                env.clone(),
            ))
        }

        ParsedStructureItemDesc::Pstr_include(incl) => {
            // Include declaration: include ME
            let typed_mod = type_module_expr(tctx, env, &incl.pincl_mod)?;

            let desc = StructureItemDesc::Tstr_include(IncludeDeclaration {
                incl_mod: typed_mod,
                incl_type: vec![],
                incl_attributes: incl.pincl_attributes.clone(),
                incl_loc: incl.pincl_loc.clone(),
            });
            Ok((
                StructureItem {
                    str_desc: desc,
                    str_loc: loc.clone(),
                    str_env: EnvRef(0),
                },
                env.clone(),
            ))
        }

        ParsedStructureItemDesc::Pstr_attribute(attr) => {
            // Attribute: @@attr
            let desc = StructureItemDesc::Tstr_attribute(attr.clone());
            Ok((
                StructureItem {
                    str_desc: desc,
                    str_loc: loc.clone(),
                    str_env: EnvRef(0),
                },
                env.clone(),
            ))
        }

        ParsedStructureItemDesc::Pstr_extension(_ext, _attrs) => {
            // Extension: [%%id]
            // Extensions are typically handled by preprocessors
            // Return a placeholder eval item
            let ctx = tctx.type_ctx;
            let unit_ty = ctx.new_tuple(vec![]);
            let desc = StructureItemDesc::Tstr_eval(
                Expression::new(
                    crate::types::typedtree::ExpressionDesc::Texp_constant(
                        crate::types::typedtree::Constant::Int(0),
                    ),
                    loc.clone(),
                    unit_ty,
                    EnvRef(0),
                    vec![],
                ),
                vec![],
            );
            Ok((
                StructureItem {
                    str_desc: desc,
                    str_loc: loc.clone(),
                    str_env: EnvRef(0),
                },
                env.clone(),
            ))
        }
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Type check value bindings in a structure.
fn type_value_bindings(
    tctx: &mut TypeCheckContext<'_>,
    env: &Env,
    rec_flag: RecFlag,
    bindings: &[crate::parser::ast::ValueBinding],
) -> TypeModResult<Vec<ValueBinding>> {
    // type_binding takes all bindings at once and returns (Vec<ValueBinding>, Env)
    let (typed_bindings, _new_env) = type_binding(tctx.type_ctx, env, rec_flag, bindings)?;
    Ok(typed_bindings)
}

/// Extend environment with value bindings.
fn extend_env_with_bindings(env: &Env, bindings: &[ValueBinding]) -> Env {
    let mut new_env = env.clone();

    for binding in bindings {
        // Extract variable names from the pattern and add them to the environment
        // This is a simplified implementation - full version would handle all pattern types
        if let crate::types::typedtree::PatternDesc::Tpat_var(id, _) = &binding.vb_pat.pat_desc {
            let val_desc = crate::types::ValueDescription {
                val_type: binding.vb_pat.pat_type,
                val_kind: crate::types::ValueKind::ValReg,
                val_loc: binding.vb_loc.clone(),
                val_attributes: vec![],
            };
            new_env.add_value(id.clone(), val_desc);
        }
    }

    new_env
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_module_error_conversion() {
        let core_error = TypeCoreError::MultiplyBoundVariable("x".to_string());
        let mod_error: TypeModError = core_error.into();
        match mod_error {
            TypeModError::CoreError(_) => {}
            _ => panic!("Expected CoreError"),
        }
    }
}
