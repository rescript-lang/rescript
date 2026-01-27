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
use crate::types::typecore::{TypeCheckContext, TypeCoreError, type_binding, type_expression, transl_type};
use crate::types::typedtree::{
    EnvRef, Expression, IncludeDeclaration, ModuleBinding, ModuleCoercion, ModuleExpr,
    ModuleExprDesc, ModuleType, ModuleTypeDeclaration, OpenDeclaration, Structure,
    StructureItem, StructureItemDesc, TypeExtension, ValueBinding,
    TypedTypeDeclaration, TypedTypeKind, TypedConstructorDeclaration, TypedConstructorArguments,
    TypedLabelDeclaration, TypedCoreType, TypedCoreTypeDesc,
};
use crate::types::Variance as TypesVariance;
use crate::parser::ast::Variance as ParserVariance;

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
            let mut typed_tree_decls: Vec<TypedTypeDeclaration> = Vec::new();

            for decl in decls {
                let type_name = &decl.ptype_name.txt;
                let type_id = Ident::create_persistent(type_name);

                // Create the type path for self-reference
                let type_path = Path::pident(type_id.clone());

                // Convert type parameters: (CoreType, Variance) -> (TypedCoreType, Variance)
                let mut typ_params: Vec<(TypedCoreType, TypesVariance)> = Vec::new();
                let mut internal_type_params: Vec<TypeExprRef> = Vec::new();
                for (core_type, parser_var) in &decl.ptype_params {
                    let (typed_core_type, type_ref) = transl_type(tctx, env, core_type)?;
                    internal_type_params.push(type_ref);
                    let types_var = match parser_var {
                        ParserVariance::Covariant => TypesVariance::covariant(),
                        ParserVariance::Contravariant => TypesVariance::contravariant(),
                        ParserVariance::Invariant => TypesVariance::invariant(),
                    };
                    typ_params.push((typed_core_type, types_var));
                }
                let type_arity = internal_type_params.len() as i32;

                // Convert type constraints
                let mut typ_cstrs: Vec<(TypedCoreType, TypedCoreType, Location)> = Vec::new();
                for (ct1, ct2, cstr_loc) in &decl.ptype_cstrs {
                    let (typed_ct1, _) = transl_type(tctx, env, ct1)?;
                    let (typed_ct2, _) = transl_type(tctx, env, ct2)?;
                    typ_cstrs.push((typed_ct1, typed_ct2, cstr_loc.clone()));
                }

                // Convert manifest type (type alias)
                let typ_manifest = match &decl.ptype_manifest {
                    Some(manifest) => {
                        let (typed_manifest, _) = transl_type(tctx, env, manifest)?;
                        Some(typed_manifest)
                    }
                    None => None,
                };

                // Convert the type kind - create both internal and typed tree versions
                let (internal_type_kind, typed_tree_kind, constructors) = match &decl.ptype_kind {
                    crate::parser::ast::TypeKind::Ptype_abstract => {
                        (crate::types::TypeKind::TypeAbstract, TypedTypeKind::Ttype_abstract, vec![])
                    }
                    crate::parser::ast::TypeKind::Ptype_variant(cstrs) => {
                        let mut internal_cstrs = Vec::new();
                        let mut typed_tree_cstrs = Vec::new();
                        let mut cstr_descs = Vec::new();
                        let num_consts = cstrs.iter().filter(|c| matches!(c.pcd_args, crate::parser::ast::ConstructorArguments::Pcstr_tuple(ref args) if args.is_empty())).count() as i32;
                        let num_nonconsts = cstrs.len() as i32 - num_consts;

                        let mut const_tag = 0i32;
                        let mut block_tag = 0i32;

                        for cstr in cstrs {
                            let cstr_id = Ident::create_persistent(&cstr.pcd_name.txt);
                            let is_constant = matches!(cstr.pcd_args, crate::parser::ast::ConstructorArguments::Pcstr_tuple(ref args) if args.is_empty());

                            // Convert constructor arguments using transl_type
                            let (cstr_args, internal_cd_args, typed_tree_cd_args) = match &cstr.pcd_args {
                                crate::parser::ast::ConstructorArguments::Pcstr_tuple(types) => {
                                    let mut typed_args: Vec<TypeExprRef> = Vec::new();
                                    let mut typed_tree_args: Vec<TypedCoreType> = Vec::new();
                                    for core_type in types {
                                        let (typed_core_type, type_ref) = transl_type(tctx, env, core_type)?;
                                        typed_args.push(type_ref);
                                        typed_tree_args.push(typed_core_type);
                                    }
                                    (typed_args.clone(), crate::types::ConstructorArguments::CstrTuple(typed_args), TypedConstructorArguments::Cstr_tuple(typed_tree_args))
                                }
                                crate::parser::ast::ConstructorArguments::Pcstr_record(labels) => {
                                    let mut typed_labels: Vec<TypedLabelDeclaration> = Vec::new();
                                    let mut internal_labels: Vec<crate::types::LabelDeclaration> = Vec::new();
                                    for lbl in labels {
                                        let (typed_core_type, type_ref) = transl_type(tctx, env, &lbl.pld_type)?;
                                        let mutable_flag = match lbl.pld_mutable {
                                            crate::parser::ast::MutableFlag::Immutable => crate::types::MutableFlag::Immutable,
                                            crate::parser::ast::MutableFlag::Mutable => crate::types::MutableFlag::Mutable,
                                        };
                                        internal_labels.push(crate::types::LabelDeclaration {
                                            ld_id: Ident::create_persistent(&lbl.pld_name.txt),
                                            ld_mutable: mutable_flag,
                                            ld_optional: lbl.pld_optional,
                                            ld_type: type_ref,
                                            ld_loc: lbl.pld_loc.clone(),
                                            ld_attributes: vec![],
                                        });
                                        typed_labels.push(TypedLabelDeclaration {
                                            ld_id: Ident::create_persistent(&lbl.pld_name.txt),
                                            ld_name: crate::location::Located {
                                                txt: lbl.pld_name.txt.clone(),
                                                loc: lbl.pld_name.loc.clone(),
                                            },
                                            ld_mutable: lbl.pld_mutable.clone(),
                                            ld_optional: lbl.pld_optional,
                                            ld_type: typed_core_type,
                                            ld_loc: lbl.pld_loc.clone(),
                                            ld_attributes: lbl.pld_attributes.clone(),
                                        });
                                    }
                                    (vec![], crate::types::ConstructorArguments::CstrRecord(internal_labels), TypedConstructorArguments::Cstr_record(typed_labels))
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

                            // Create the result type
                            let res_ty = ctx.new_constr(type_path.clone(), vec![]);

                            // Create internal constructor declaration
                            internal_cstrs.push(crate::types::ConstructorDeclaration {
                                cd_id: cstr_id.clone(),
                                cd_args: internal_cd_args,
                                cd_res: None,
                                cd_loc: cstr.pcd_loc.clone(),
                                cd_attributes: vec![],
                            });

                            // Create typed tree constructor declaration
                            typed_tree_cstrs.push(TypedConstructorDeclaration {
                                cd_id: cstr_id.clone(),
                                cd_name: crate::location::Located {
                                    txt: cstr.pcd_name.txt.clone(),
                                    loc: cstr.pcd_name.loc.clone(),
                                },
                                cd_args: typed_tree_cd_args,
                                cd_res: None,
                                cd_loc: cstr.pcd_loc.clone(),
                                cd_attributes: cstr.pcd_attributes.clone(),
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

                        (crate::types::TypeKind::TypeVariant(internal_cstrs), TypedTypeKind::Ttype_variant(typed_tree_cstrs), cstr_descs)
                    }
                    crate::parser::ast::TypeKind::Ptype_record(labels) => {
                        let mut internal_labels: Vec<crate::types::LabelDeclaration> = Vec::new();
                        let mut typed_tree_labels: Vec<TypedLabelDeclaration> = Vec::new();
                        for lbl in labels {
                            let (typed_core_type, type_ref) = transl_type(tctx, env, &lbl.pld_type)?;
                            let mutable_flag = match lbl.pld_mutable {
                                crate::parser::ast::MutableFlag::Immutable => crate::types::MutableFlag::Immutable,
                                crate::parser::ast::MutableFlag::Mutable => crate::types::MutableFlag::Mutable,
                            };
                            internal_labels.push(crate::types::LabelDeclaration {
                                ld_id: Ident::create_persistent(&lbl.pld_name.txt),
                                ld_mutable: mutable_flag,
                                ld_optional: lbl.pld_optional,
                                ld_type: type_ref,
                                ld_loc: lbl.pld_loc.clone(),
                                ld_attributes: vec![],
                            });
                            typed_tree_labels.push(TypedLabelDeclaration {
                                ld_id: Ident::create_persistent(&lbl.pld_name.txt),
                                ld_name: crate::location::Located {
                                    txt: lbl.pld_name.txt.clone(),
                                    loc: lbl.pld_name.loc.clone(),
                                },
                                ld_mutable: lbl.pld_mutable.clone(),
                                ld_optional: lbl.pld_optional,
                                ld_type: typed_core_type,
                                ld_loc: lbl.pld_loc.clone(),
                                ld_attributes: lbl.pld_attributes.clone(),
                            });
                        }
                        (crate::types::TypeKind::TypeRecord(internal_labels, crate::types::RecordRepresentation::RecordRegular), TypedTypeKind::Ttype_record(typed_tree_labels), vec![])
                    }
                    crate::parser::ast::TypeKind::Ptype_open => {
                        (crate::types::TypeKind::TypeOpen, TypedTypeKind::Ttype_open, vec![])
                    }
                };

                // Create internal type declaration for the environment
                let internal_decl = crate::types::TypeDeclaration {
                    type_params: internal_type_params,
                    type_arity,
                    type_kind: internal_type_kind,
                    type_private: crate::types::PrivateFlag::Public,
                    type_manifest: None,
                    type_variance: typ_params.iter().map(|(_, v)| *v).collect(),
                    type_newtype_level: None,
                    type_loc: decl.ptype_loc.clone(),
                    type_attributes: vec![],
                    type_immediate: false,
                    type_unboxed: crate::types::UnboxedStatus::default(),
                    type_inlined_types: vec![],
                };

                // Add type to environment
                new_env.add_type(type_id.clone(), internal_decl.clone());

                // Add constructors to environment
                for (cstr_id, cstr_desc) in constructors {
                    new_env.add_constructor(cstr_id, cstr_desc);
                }

                // Convert private flag
                let typ_private = match decl.ptype_private {
                    crate::parser::ast::PrivateFlag::Public => crate::parser::ast::PrivateFlag::Public,
                    crate::parser::ast::PrivateFlag::Private => crate::parser::ast::PrivateFlag::Private,
                };

                // Create typed tree type declaration
                let typed_tree_decl = TypedTypeDeclaration {
                    typ_id: type_id,
                    typ_name: crate::location::Located {
                        txt: type_name.clone(),
                        loc: decl.ptype_name.loc.clone(),
                    },
                    typ_params,
                    typ_type: internal_decl,
                    typ_cstrs,
                    typ_kind: typed_tree_kind,
                    typ_private,
                    typ_manifest,
                    typ_loc: decl.ptype_loc.clone(),
                    typ_attributes: decl.ptype_attributes.clone(),
                };

                typed_tree_decls.push(typed_tree_decl);
            }

            let desc = StructureItemDesc::Tstr_type(*rec_flag, typed_tree_decls);
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
