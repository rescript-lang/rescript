//! JSX PPX transformation
//!
//! This module transforms JSX syntax into React function calls, matching the OCaml
//! JSX v4 PPX behavior. The transformation is applied after parsing.
//!
//! Transformations:
//! - `<></>` -> `React.jsx(React.jsxFragment, {})`
//! - `<div />` -> `ReactDOM.jsx("div", {})`
//! - `<Component />` -> `React.jsx(Component.make, {})`
//! - Multiple children use `jsxs` instead of `jsx`
//! - Key props use `jsxKeyed`/`jsxsKeyed` instead
//! - `@react.component` transforms functions to accept props record

use crate::location::{Location as FullLocation, Position};
use crate::parse_arena::{LidentIdx, Located, LocIdx, ParseArena};
use super::ast::*;
use super::longident::Longident;

/// Component description type - mirrors OCaml's componentDescription
#[derive(Debug, Clone, Copy, PartialEq)]
enum ComponentDescription {
    LowercasedComponent,
    UppercasedComponent,
    FragmentComponent,
}

/// JSX configuration from @@jsxConfig attribute
#[derive(Debug, Clone)]
pub struct JsxConfig {
    /// JSX version (4 for modern JSX, -1 for preserve/no transform)
    pub version: i32,
    /// Module name for JSX functions (default: "React")
    pub module_name: String,
    /// Nested module path for component naming
    pub nested_modules: Vec<String>,
    /// Whether a component has already been defined in this scope
    pub has_component: bool,
    /// File name (without extension, capitalized) for module naming
    pub file_name: String,
}

impl Default for JsxConfig {
    fn default() -> Self {
        JsxConfig {
            version: -1, // Default: no transformation
            module_name: "React".to_string(),
            nested_modules: vec![],
            has_component: false,
            file_name: String::new(),
        }
    }
}

/// Extract capitalized file name from a file path
fn filename_from_loc(loc: &Location, arena: &ParseArena) -> String {
    let file_name_idx = arena.loc_start(*loc).file_name;
    let file_name = arena.get_string(file_name_idx);
    if file_name.is_empty() {
        return String::new();
    }
    // Get basename and remove extension
    let basename = std::path::Path::new(file_name)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or(file_name);
    // Capitalize first character
    let mut chars = basename.chars();
    match chars.next() {
        None => String::new(),
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
    }
}

/// Extracted argument info from a function
#[derive(Debug, Clone)]
struct ExtractedArg {
    /// The argument label
    label: ArgLabel,
    /// Default value if optional with default
    default: Option<Expression>,
    /// The pattern
    pattern: Pattern,
    /// Alias name for the argument
    alias: String,
    /// Location
    loc: Location,
    /// Type constraint if any
    type_: Option<CoreType>,
}

/// Named type info for props
#[derive(Debug, Clone)]
struct NamedType {
    is_optional: bool,
    label: String,
    attrs: Attributes,
    loc: Location,
    /// The interior type - Ptyp_any if no explicit annotation
    interior_type: CoreType,
    /// Whether the type was explicitly annotated (vs inferred as Ptyp_any)
    has_explicit_type: bool,
}

// ============================================================================
// Public API
// ============================================================================

/// Transform a structure, applying JSX PPX transformations
pub fn transform_structure(structure: Structure, initial_version: i32, arena: &mut ParseArena) -> Structure {
    // Extract file name from first item's location
    let file_name = structure
        .first()
        .map(|item| filename_from_loc(&item.pstr_loc, arena))
        .unwrap_or_default();

    let mut config = JsxConfig {
        version: initial_version,
        file_name,
        ..Default::default()
    };

    let mut result = vec![];
    for item in structure {
        let items = transform_structure_item_multi(item, &mut config, arena);
        result.extend(items);
    }
    result
}

/// Transform a signature, applying JSX PPX transformations
pub fn transform_signature(signature: Signature, initial_version: i32, arena: &mut ParseArena) -> Signature {
    // Extract file name from first item's location
    let file_name = signature
        .first()
        .map(|item| filename_from_loc(&item.psig_loc, arena))
        .unwrap_or_default();

    let mut config = JsxConfig {
        version: initial_version,
        file_name,
        ..Default::default()
    };

    let mut result = vec![];
    for item in signature {
        let items = transform_signature_item_multi(item, &mut config, arena);
        result.extend(items);
    }
    result
}

// ============================================================================
// Structure Item Transformation
// ============================================================================

/// Transform a structure item, potentially returning multiple items
/// (e.g., props type + transformed function)
fn transform_structure_item_multi(item: StructureItem, config: &mut JsxConfig, arena: &mut ParseArena) -> Vec<StructureItem> {
    // Check for @@jsxConfig attribute
    if let StructureItemDesc::Pstr_attribute(attr) = &item.pstr_desc {
        if attr.0.txt == "jsxConfig" {
            update_config_from_attribute(arena, attr, config);
        }
        return vec![item];
    }

    // Only transform if version is 4
    if config.version != 4 {
        return vec![item];
    }

    // Handle Pstr_value with potential @react.component
    if let StructureItemDesc::Pstr_value(rec_flag, bindings) = item.pstr_desc {
        return transform_value_bindings(rec_flag, bindings, &item.pstr_loc, config, arena);
    }

    // Handle Pstr_primitive (external) with potential @react.component
    if let StructureItemDesc::Pstr_primitive(vd) = &item.pstr_desc {
        if has_react_component_attr(&vd.pval_attributes) {
            return transform_react_component_external(vd.clone(), &item.pstr_loc, config, arena);
        }
    }

    // Handle module definitions (for nested module tracking)
    if let StructureItemDesc::Pstr_module(mb) = item.pstr_desc {
        let transformed = transform_module_binding_with_tracking(mb, config, arena);
        return vec![StructureItem {
            pstr_desc: StructureItemDesc::Pstr_module(transformed),
            pstr_loc: item.pstr_loc,
        }];
    }

    // Default: just transform expressions within
    vec![StructureItem {
        pstr_desc: transform_structure_item_desc(item.pstr_desc, config, arena),
        pstr_loc: item.pstr_loc,
    }]
}

fn transform_module_binding_with_tracking(mb: ModuleBinding, config: &mut JsxConfig, arena: &mut ParseArena) -> ModuleBinding {
    // Track nested module name
    let module_name = mb.pmb_name.txt.clone();
    config.nested_modules.push(module_name);
    config.has_component = false;

    let transformed = transform_module_binding(mb, config, arena);

    // Pop the module name
    config.nested_modules.pop();

    transformed
}

/// Transform value bindings, handling @react.component
fn transform_value_bindings(
    rec_flag: RecFlag,
    bindings: Vec<ValueBinding>,
    loc: &Location,
    config: &mut JsxConfig,
    arena: &mut ParseArena,
) -> Vec<StructureItem> {
    let mut props_types = vec![];
    let mut transformed_bindings = vec![];
    let mut new_bindings = vec![];

    for binding in bindings {
        if has_react_component_attr(&binding.pvb_attributes) {
            // Transform @react.component binding
            let (props_type, transformed, new_binding) =
                transform_react_component_binding(binding, rec_flag, loc, config, arena);

            if let Some(pt) = props_type {
                props_types.push(pt);
            }
            transformed_bindings.push(transformed);
            if let Some(nb) = new_binding {
                new_bindings.push(nb);
            }
        } else if has_react_component_with_props_attr(&binding.pvb_attributes) {
            // Transform @react.componentWithProps binding
            let (transformed, new_binding) =
                transform_react_component_with_props_binding(binding, rec_flag, loc, config, arena);

            transformed_bindings.push(transformed);
            if let Some(nb) = new_binding {
                new_bindings.push(nb);
            }
        } else {
            // Just transform expressions within
            transformed_bindings.push(transform_value_binding(binding, config, arena));
        }
    }

    let mut result = props_types;
    result.push(StructureItem {
        pstr_desc: StructureItemDesc::Pstr_value(rec_flag, transformed_bindings),
        pstr_loc: loc.clone(),
    });

    if !new_bindings.is_empty() {
        result.push(StructureItem {
            pstr_desc: StructureItemDesc::Pstr_value(rec_flag, new_bindings),
            pstr_loc: empty_loc(),
        });
    }

    result
}

/// Detect forwardRef wrapper and extract inner function
/// Returns (wrapper_expr, has_forward_ref, inner_expression)
/// wrapper_expr is the original wrapper (e.g., React.forwardRef) expression with its source location
fn spelunk_for_fun_expression<'a>(arena: &ParseArena, expr: &'a Expression) -> (Option<&'a Expression>, bool, &'a Expression) {
    match &expr.pexp_desc {
        // let make = (~prop) => ...
        ExpressionDesc::Pexp_fun { .. } | ExpressionDesc::Pexp_newtype(_, _) => {
            (None, false, expr)
        }
        // let make = {let foo = bar in (~prop) => ...}
        ExpressionDesc::Pexp_let(_, _, inner) => {
            let (_, has_forward_ref, inner_expr) = spelunk_for_fun_expression(arena, inner);
            (None, has_forward_ref, inner_expr)
        }
        // let make = React.forwardRef((~prop) => ...)
        ExpressionDesc::Pexp_apply { funct, args, .. } if args.len() == 1 && matches!(&args[0].0, ArgLabel::Nolabel) => {
            let (_, _, inner_expr) = spelunk_for_fun_expression(arena, &args[0].1);
            let has_forward_ref = is_forward_ref(arena, funct);
            // Return the original funct expression (e.g., React.forwardRef) to preserve its source location
            (if has_forward_ref { Some(funct.as_ref()) } else { None }, has_forward_ref, inner_expr)
        }
        ExpressionDesc::Pexp_sequence(_, inner) => {
            let (_, has_forward_ref, inner_expr) = spelunk_for_fun_expression(arena, inner);
            (None, has_forward_ref, inner_expr)
        }
        ExpressionDesc::Pexp_constraint(inner, _) => {
            spelunk_for_fun_expression(arena, inner)
        }
        _ => (None, false, expr)
    }
}

/// Transform a @react.component binding
fn transform_react_component_binding(
    binding: ValueBinding,
    rec_flag: RecFlag,
    pstr_loc: &Location,
    config: &mut JsxConfig,
    arena: &mut ParseArena,
) -> (Option<StructureItem>, ValueBinding, Option<ValueBinding>) {
    // Check for multiple components in the same module (OCaml: check_multiple_components)
    if config.has_component {
        eprintln!(
            "Only one component definition is allowed for each module. Move to a submodule or other file if necessary. (at {:?})",
            pstr_loc
        );
    }
    config.has_component = true;

    // Get function name and location from pattern
    let fn_name = get_fn_name(&binding.pvb_pat);
    let fn_name_loc = get_fn_name_loc(&binding.pvb_pat);
    let original_binding_loc = binding.pvb_loc.clone();

    // Check for sharedProps: @react.component(:sharedProps<T>)
    let core_type_of_attr = core_type_of_attrs(&binding.pvb_attributes);
    let typ_vars_of_core_type: Vec<CoreType> = core_type_of_attr.as_ref()
        .map(typ_vars_of_core_type)
        .unwrap_or_default();

    // Detect forwardRef and get inner expression
    // wrapper_expr is the original React.forwardRef expression with its source location
    let (wrapper_expr, has_forward_ref, inner_expr) = spelunk_for_fun_expression(arena, &binding.pvb_expr);

    // Detect if function is async
    let is_async = is_async_function(&binding.pvb_expr);

    // Filter out @react.component attribute
    let filtered_attrs: Attributes = binding.pvb_attributes
        .iter()
        .filter(|attr| !is_react_component_attr(attr))
        .cloned()
        .collect();

    // Extract labeled arguments from the function (use inner expression to handle forwardRef)
    let (args, newtypes, _) = recursively_extract_named_args(arena, inner_expr);

    // Build named type list for props
    let named_type_list: Vec<NamedType> = args.iter()
        .filter_map(|arg| arg_to_type(arena, arg))
        .collect();

    // Create props type - either record or abstract with manifest (for sharedProps)
    let props_type = if let Some(manifest) = &core_type_of_attr {
        make_props_abstract_type("props", pstr_loc, manifest, &typ_vars_of_core_type)
    } else {
        make_props_record_type("props", pstr_loc, &named_type_list, has_forward_ref)
    };

    // Build the internal expression pattern (use inner expression to handle forwardRef)
    let (patterns_with_label, expression) = build_props_pattern_and_body(inner_expr, &args, config, arena);

    // Create record pattern: {a, b, _}: props<'a, 'b>
    let record_pattern = if patterns_with_label.is_empty() {
        Pattern {
            ppat_desc: PatternDesc::Ppat_any,
            ppat_loc: empty_loc(),
            ppat_attributes: vec![],
        }
    } else {
        Pattern {
            ppat_desc: PatternDesc::Ppat_record(patterns_with_label, ClosedFlag::Open),
            ppat_loc: empty_loc(),
            ppat_attributes: vec![],
        }
    };

    // Constrain pattern with props type
    // For sharedProps, use Ptyp_any as type params; otherwise use named_type_list
    let props_type_params: Vec<CoreType> = if core_type_of_attr.is_some() {
        // For sharedProps, use empty params or single Ptyp_any if typ_vars exist
        if typ_vars_of_core_type.is_empty() {
            vec![]
        } else {
            vec![CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_any,
                ptyp_loc: empty_loc(),
                ptyp_attributes: vec![],
            }]
        }
    } else {
        named_type_list.iter()
            .filter(|nt| nt.label != "key")
            .filter_map(|nt| {
                // For forwardRef, 'ref' param should be Ptyp_var "ref" when interior_type is Ptyp_any
                // OCaml: if interior_type is Ptyp_any, use ref_type_var loc (actual location)
                // Otherwise, strip Js.Nullable wrapper when has_forward_ref
                if nt.label == "ref" && has_forward_ref {
                    match &nt.interior_type.ptyp_desc {
                        CoreTypeDesc::Ptyp_any => {
                            // OCaml uses actual param location for Ptyp_var when interior is Ptyp_any
                            return Some(CoreType {
                                ptyp_desc: CoreTypeDesc::Ptyp_var("ref".to_string()),
                                ptyp_loc: nt.loc.clone(),
                                ptyp_attributes: vec![],
                            });
                        }
                        _ => {
                            // Strip explicit Js.Nullable.t in case of forwardRef
                            return strip_js_nullable(arena, &nt.interior_type);
                        }
                    }
                }
                // For optional args, OCaml strips the option wrapper
                if nt.is_optional && nt.has_explicit_type {
                    strip_option(arena, &nt.interior_type)
                } else {
                    Some(nt.interior_type.clone())
                }
            })
            .collect()
    };

    let props_str_idx = arena.intern_string("props");
    let props_lid_idx = arena.push_longident(Longident::Lident(props_str_idx));
    let props_constr = CoreType {
        ptyp_desc: CoreTypeDesc::Ptyp_constr(
            Loc { txt: props_lid_idx, loc: empty_loc() },
            props_type_params.clone(),
        ),
        ptyp_loc: empty_loc(),
        ptyp_attributes: vec![],
    };

    let constrained_pattern = Pattern {
        ppat_desc: PatternDesc::Ppat_constraint(Box::new(record_pattern), props_constr),
        ppat_loc: empty_loc(),
        ppat_attributes: vec![],
    };

    // Determine arity based on forwardRef
    let total_arity = if has_forward_ref { 2 } else { 1 };

    // Create the function: (props: props<...>) => body
    // For forwardRef: (props, ref) => body
    let inner_func_expr = if has_forward_ref {
        // Find ref arg and its alias from extracted args
        let ref_arg = args.iter().find(|a| a.alias == "ref" || a.alias == "_ref");
        let ref_alias = ref_arg.map(|a| a.alias.clone()).unwrap_or_else(|| "ref".to_string());
        // OCaml uses the original location from the ref parameter
        let ref_loc = ref_arg.map(|a| a.loc.clone()).unwrap_or_else(empty_loc);

        // Create ref parameter - use actual alias and location from args
        let base_ref_pattern = Pattern {
            ppat_desc: PatternDesc::Ppat_var(Loc { txt: ref_alias.clone(), loc: ref_loc.clone() }),
            ppat_loc: ref_loc.clone(),
            ppat_attributes: vec![],
        };

        // OCaml (line 749-751): only add Js.Nullable.t<'ref> constraint when the pattern is just
        // Ppat_var {txt} (i.e., no existing constraint). If the original pattern already has a
        // type constraint, preserve it.
        let ref_pattern = if let Some(arg) = ref_arg {
            // Check if the original pattern already has a type constraint
            if matches!(arg.pattern.ppat_desc, PatternDesc::Ppat_constraint(_, _)) {
                // Preserve the original pattern with its type annotation
                arg.pattern.clone()
            } else if !ref_alias.starts_with('_') {
                // No existing constraint, and not unused - add Js.Nullable.t<'ref>
                // OCaml calls ref_type Location.none, so inner Ptyp_var uses ghost location
                let ref_type_var = CoreType {
                    ptyp_desc: CoreTypeDesc::Ptyp_var("ref".to_string()),
                    ptyp_loc: empty_loc(),
                    ptyp_attributes: vec![],
                };
                let js_idx = arena.intern_string("Js");
                let nullable_idx = arena.intern_string("Nullable");
                let t_idx = arena.intern_string("t");
                let js_nullable_t_lid = arena.push_longident(Longident::Ldot(
                    Box::new(Longident::Ldot(
                        Box::new(Longident::Lident(js_idx)),
                        nullable_idx,
                    )),
                    t_idx,
                ));
                let js_nullable_t = CoreType {
                    ptyp_desc: CoreTypeDesc::Ptyp_constr(
                        Loc {
                            txt: js_nullable_t_lid,
                            loc: empty_loc(),
                        },
                        vec![ref_type_var],
                    ),
                    ptyp_loc: empty_loc(),
                    ptyp_attributes: vec![],
                };

                // OCaml uses ghost location for the outer Ppat_constraint
                Pattern {
                    ppat_desc: PatternDesc::Ppat_constraint(Box::new(base_ref_pattern), js_nullable_t),
                    ppat_loc: empty_loc(),
                    ppat_attributes: vec![],
                }
            } else {
                base_ref_pattern
            }
        } else if !ref_alias.starts_with('_') {
            // No ref_arg found but alias doesn't start with "_" - add Js.Nullable.t<'ref>
            let ref_type_var = CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_var("ref".to_string()),
                ptyp_loc: empty_loc(),
                ptyp_attributes: vec![],
            };
            let js_idx = arena.intern_string("Js");
            let nullable_idx = arena.intern_string("Nullable");
            let t_idx = arena.intern_string("t");
            let js_nullable_t_lid = arena.push_longident(Longident::Ldot(
                Box::new(Longident::Ldot(
                    Box::new(Longident::Lident(js_idx)),
                    nullable_idx,
                )),
                t_idx,
            ));
            let js_nullable_t = CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_constr(
                    Loc {
                        txt: js_nullable_t_lid,
                        loc: empty_loc(),
                    },
                    vec![ref_type_var],
                ),
                ptyp_loc: empty_loc(),
                ptyp_attributes: vec![],
            };
            Pattern {
                ppat_desc: PatternDesc::Ppat_constraint(Box::new(base_ref_pattern), js_nullable_t),
                ppat_loc: empty_loc(),
                ppat_attributes: vec![],
            }
        } else {
            base_ref_pattern
        };

        // Inner function with ref parameter
        let inner_with_ref = Expression {
            pexp_desc: ExpressionDesc::Pexp_fun {
                arg_label: ArgLabel::Nolabel,
                default: None,
                lhs: ref_pattern,
                rhs: Box::new(expression),
                arity: Arity::Unknown,
                is_async: false,
            },
            pexp_loc: empty_loc(),
            pexp_attributes: vec![],
        };

        // Outer function with props parameter
        // OCaml: Exp.fun_ ~arity:(Some total_arity) ~async:is_async Nolabel None
        // Note: OCaml line 765 does NOT pass ~attrs, so this inner expression has no attributes
        // The outer wrapper (line 632) is the one that gets ~attrs:binding.pvb_expr.pexp_attributes
        Expression {
            pexp_desc: ExpressionDesc::Pexp_fun {
                arg_label: ArgLabel::Nolabel,
                default: None,
                lhs: constrained_pattern,
                rhs: Box::new(inner_with_ref),
                arity: Arity::Full(total_arity),
                is_async,
            },
            // OCaml uses ghost location for the inner Pexp_fun
            pexp_loc: empty_loc(),
            pexp_attributes: vec![],
        }
    } else {
        // Normal case without forwardRef
        // OCaml: Exp.fun_ ~arity:(Some total_arity) ~async:is_async Nolabel None
        // Note: OCaml line 765 does NOT pass ~attrs, so this inner expression has no attributes
        Expression {
            pexp_desc: ExpressionDesc::Pexp_fun {
                arg_label: ArgLabel::Nolabel,
                default: None,
                lhs: constrained_pattern,
                rhs: Box::new(expression),
                arity: Arity::Full(total_arity),
                is_async,
            },
            // OCaml uses ghost location for the inner Pexp_fun
            pexp_loc: empty_loc(),
            pexp_attributes: vec![],
        }
    };

    // Wrap the ENTIRE function with newtypes (type a, type b, etc)
    let mut func_expr = inner_func_expr;
    for newtype in newtypes.into_iter().rev() {
        let loc = func_expr.pexp_loc.clone();
        func_expr = Expression {
            pexp_desc: ExpressionDesc::Pexp_newtype(newtype, Box::new(func_expr)),
            pexp_loc: loc,
            pexp_attributes: vec![],
        };
    }

    // Create the full module name for the component
    let full_module_name = make_module_name(config, &fn_name);

    // Create wrapper expression: let "ModuleName" = props => ...; "ModuleName"
    // For forwardRef, wrapper needs to handle ref too
    let full_expression = if full_module_name.is_empty() {
        func_expr.clone()
    } else {
        // For forwardRef without a file_name, add "ForwardRef$" prefix with nested modules in order
        // For forwardRef with file_name, use the regular full_module_name
        let binding_name = if has_forward_ref && config.file_name.is_empty() {
            let nested = &config.nested_modules;
            if nested.is_empty() {
                "ForwardRef".to_string()
            } else {
                format!("ForwardRef${}", nested.join("$"))
            }
        } else {
            full_module_name.clone()
        };

        // For sharedProps without type variables, the wrapper should NOT have a props type constraint
        // For sharedProps WITH type variables, or non-sharedProps, it should have the constraint
        let has_props_constraint = !named_type_list.is_empty() && (core_type_of_attr.is_none() || !typ_vars_of_core_type.is_empty());

        let inner_binding = ValueBinding {
            pvb_pat: Pattern {
                ppat_desc: PatternDesc::Ppat_var(Loc { txt: binding_name.clone(), loc: empty_loc() }),
                ppat_loc: empty_loc(),
                ppat_attributes: vec![],
            },
            // OCaml line 632: ~attrs:binding.pvb_expr.pexp_attributes
            pvb_expr: make_props_wrapper_expr_with_ref(arena, &fn_name, rec_flag, config, has_props_constraint, has_forward_ref, is_async, binding.pvb_expr.pexp_attributes.clone()),
            pvb_attributes: vec![],
            pvb_loc: empty_loc(),
        };

        let binding_str_idx = arena.push_string(binding_name.clone());
        let binding_lid_idx = arena.push_longident(Longident::Lident(binding_str_idx));
        Expression {
            pexp_desc: ExpressionDesc::Pexp_let(
                RecFlag::Nonrecursive,
                vec![inner_binding],
                Box::new(Expression {
                    pexp_desc: ExpressionDesc::Pexp_ident(Loc {
                        txt: binding_lid_idx,
                        loc: empty_loc(),
                    }),
                    pexp_loc: pstr_loc.clone(),
                    pexp_attributes: vec![],
                }),
            ),
            pexp_loc: empty_loc(),
            pexp_attributes: vec![],
        }
    };

    // For forwardRef, wrap full_expression with React.forwardRef(...)
    // Use the original wrapper_expr to preserve source locations
    let final_full_expression = if has_forward_ref {
        if let Some(wrapper) = wrapper_expr {
            // Use original wrapper expression from source (preserves location)
            Expression {
                pexp_desc: ExpressionDesc::Pexp_apply {
                    funct: Box::new(wrapper.clone()),
                    args: vec![(ArgLabel::Nolabel, full_expression)],
                    partial: false,
                    transformed_jsx: false,
                },
                // OCaml uses ghost location for the outer Pexp_apply
                pexp_loc: empty_loc(),
                pexp_attributes: vec![],
            }
        } else {
            // Fallback: create fresh expression (shouldn't happen)
            let react_idx = arena.intern_string("React");
            let forwardref_idx = arena.intern_string("forwardRef");
            let react_forwardref_lid = arena.push_longident(Longident::Ldot(
                Box::new(Longident::Lident(react_idx)),
                forwardref_idx,
            ));
            Expression {
                pexp_desc: ExpressionDesc::Pexp_apply {
                    funct: Box::new(Expression {
                        pexp_desc: ExpressionDesc::Pexp_ident(Loc {
                            txt: react_forwardref_lid,
                            loc: empty_loc(),
                        }),
                        pexp_loc: empty_loc(),
                        pexp_attributes: vec![],
                    }),
                    args: vec![(ArgLabel::Nolabel, full_expression)],
                    partial: false,
                    transformed_jsx: false,
                },
                pexp_loc: empty_loc(),
                pexp_attributes: vec![],
            }
        }
    } else {
        full_expression
    };

    // Handle recursive vs nonrecursive differently
    // OCaml's jsx_v4.ml lines 788-806
    let (transformed_binding, new_binding) = match rec_flag {
        RecFlag::Recursive => {
            // For recursive: create a nested let structure
            // let rec make = {
            //   let "make$Internal" = func_expr in
            //   let make = full_expression in
            //   make
            // }
            let internal_fn_name = format!("{}$Internal", fn_name);

            // Inner let: let "make$Internal" = func_expr
            let internal_binding = ValueBinding {
                pvb_pat: Pattern {
                    ppat_desc: PatternDesc::Ppat_var(Loc { txt: internal_fn_name, loc: empty_loc() }),
                    ppat_loc: empty_loc(),
                    ppat_attributes: vec![],
                },
                pvb_expr: func_expr,
                pvb_attributes: vec![],
                pvb_loc: empty_loc(),
            };

            // Middle let: let make = full_expression
            let wrapper_binding = ValueBinding {
                pvb_pat: Pattern {
                    ppat_desc: PatternDesc::Ppat_var(Loc { txt: fn_name.clone(), loc: empty_loc() }),
                    ppat_loc: empty_loc(),
                    ppat_attributes: vec![],
                },
                pvb_expr: final_full_expression,
                pvb_attributes: vec![],
                pvb_loc: empty_loc(),
            };

            // Final identifier: make
            let fn_str_idx = arena.push_string(fn_name.clone());
            let fn_lid_idx = arena.push_longident(Longident::Lident(fn_str_idx));
            let final_ident = Expression {
                pexp_desc: ExpressionDesc::Pexp_ident(Loc {
                    txt: fn_lid_idx,
                    loc: empty_loc(),
                }),
                pexp_loc: empty_loc(),
                pexp_attributes: vec![],
            };

            // Build nested let: let "make$Internal" = ... in (let make = ... in make)
            let inner_let = Expression {
                pexp_desc: ExpressionDesc::Pexp_let(
                    RecFlag::Nonrecursive,
                    vec![wrapper_binding],
                    Box::new(final_ident),
                ),
                pexp_loc: empty_loc(),
                pexp_attributes: vec![],
            };

            let outer_let = Expression {
                pexp_desc: ExpressionDesc::Pexp_let(
                    RecFlag::Nonrecursive,
                    vec![internal_binding],
                    Box::new(inner_let),
                ),
                pexp_loc: empty_loc(),
                pexp_attributes: vec![],
            };

            // Create the transformed binding with nested let as expression
            let binding = ValueBinding {
                pvb_pat: Pattern {
                    ppat_desc: PatternDesc::Ppat_var(Loc { txt: fn_name, loc: original_binding_loc.clone() }),
                    ppat_loc: original_binding_loc,
                    ppat_attributes: vec![],
                },
                pvb_expr: outer_let,
                pvb_attributes: filtered_attrs,
                pvb_loc: pstr_loc.clone(),
            };

            (binding, None)
        }
        RecFlag::Nonrecursive => {
            // For nonrecursive: create two separate bindings
            let transformed_binding = ValueBinding {
                pvb_pat: Pattern {
                    ppat_desc: PatternDesc::Ppat_var(Loc { txt: fn_name.clone(), loc: empty_loc() }),
                    ppat_loc: empty_loc(),
                    ppat_attributes: vec![],
                },
                pvb_expr: func_expr,
                pvb_attributes: filtered_attrs,
                pvb_loc: empty_loc(),
            };

            // Create new binding with wrapper if module name is non-empty
            let new_binding = if !full_module_name.is_empty() {
                Some(ValueBinding {
                    pvb_pat: Pattern {
                        ppat_desc: PatternDesc::Ppat_var(Loc { txt: fn_name, loc: fn_name_loc.clone() }),
                        ppat_loc: fn_name_loc,
                        ppat_attributes: vec![],
                    },
                    pvb_expr: final_full_expression,
                    pvb_attributes: vec![],
                    pvb_loc: pstr_loc.clone(),
                })
            } else {
                None
            };

            (transformed_binding, new_binding)
        }
    };

    (Some(props_type), transformed_binding, new_binding)
}

/// Transform a @react.componentWithProps binding
/// This is different from @react.component - it uses an existing props type
/// and doesn't create a new props type declaration
fn transform_react_component_with_props_binding(
    binding: ValueBinding,
    rec_flag: RecFlag,
    pstr_loc: &Location,
    config: &mut JsxConfig,
    arena: &mut ParseArena,
) -> (ValueBinding, Option<ValueBinding>) {
    // Check for multiple components in the same module
    if config.has_component {
        eprintln!(
            "Only one component definition is allowed for each module. Move to a submodule or other file if necessary."
        );
    }
    config.has_component = true;

    // Get function name and location from pattern
    let fn_name = get_fn_name(&binding.pvb_pat);
    let fn_name_loc = get_fn_name_loc(&binding.pvb_pat);
    let internal_fn_name = format!("{}$Internal", fn_name);
    let full_module_name = make_module_name(config, &fn_name);

    // Check if the expression uses React.forwardRef (not allowed with componentWithProps)
    let (_, has_forward_ref, _) = spelunk_for_fun_expression(arena, &binding.pvb_expr);
    if has_forward_ref {
        eprintln!(
            "Components using React.forwardRef cannot use @react.componentWithProps. Use @react.component instead."
        );
    }

    // Check if async
    let is_async = is_async_function(&binding.pvb_expr);

    // Filter out @react.componentWithProps attribute
    let filtered_attrs: Attributes = binding.pvb_attributes
        .iter()
        .filter(|attr| !is_react_component_with_props_attr(attr))
        .cloned()
        .collect();

    // Create props pattern based on the original function's pattern
    let props_pattern = make_with_props_pattern(arena, &binding.pvb_expr);

    // Create the applied expression: fnName(props) or fnName$Internal(props)
    let called_fn_name = match rec_flag {
        RecFlag::Recursive => internal_fn_name.clone(),
        RecFlag::Nonrecursive => fn_name.clone(),
    };

    let called_fn_str_idx = arena.push_string(called_fn_name);
    let called_fn_lid = arena.push_longident(Longident::Lident(called_fn_str_idx));
    let props_str_idx = arena.intern_string("props");
    let props_lid = arena.push_longident(Longident::Lident(props_str_idx));
    let applied_expr = Expression {
        pexp_desc: ExpressionDesc::Pexp_apply {
            funct: Box::new(Expression {
                pexp_desc: ExpressionDesc::Pexp_ident(Loc {
                    txt: called_fn_lid,
                    loc: empty_loc(),
                }),
                pexp_loc: empty_loc(),
                pexp_attributes: vec![],
            }),
            args: vec![(
                ArgLabel::Nolabel,
                Expression {
                    pexp_desc: ExpressionDesc::Pexp_ident(Loc {
                        txt: props_lid,
                        loc: empty_loc(),
                    }),
                    pexp_loc: empty_loc(),
                    pexp_attributes: vec![],
                },
            )],
            partial: false,
            transformed_jsx: false,
        },
        pexp_loc: empty_loc(),
        pexp_attributes: vec![],
    };

    // Handle async wrapping if needed
    let applied_expr = if is_async {
        wrap_with_jsx_promise(arena, applied_expr)
    } else {
        applied_expr
    };

    // Wrap with React.element return type constraint
    let constrained_expr = constrain_jsx_return(arena, applied_expr, config);

    // Create wrapper expression: props => constrained_expr
    let wrapper_expr = Expression {
        pexp_desc: ExpressionDesc::Pexp_fun {
            arg_label: ArgLabel::Nolabel,
            default: None,
            lhs: props_pattern,
            rhs: Box::new(constrained_expr),
            arity: Arity::Full(1),
            is_async: false,
        },
        pexp_loc: empty_loc(),
        pexp_attributes: binding.pvb_expr.pexp_attributes.clone(),
    };

    // Create let binding: let full_module_name = wrapper_expr in full_module_name
    let internal_expr = Expression {
        pexp_desc: ExpressionDesc::Pexp_let(
            RecFlag::Nonrecursive,
            vec![ValueBinding {
                pvb_pat: Pattern {
                    ppat_desc: PatternDesc::Ppat_var(Loc { txt: full_module_name.clone(), loc: empty_loc() }),
                    ppat_loc: empty_loc(),
                    ppat_attributes: vec![],
                },
                pvb_expr: wrapper_expr,
                pvb_attributes: vec![],
                pvb_loc: empty_loc(),
            }],
            Box::new(Expression {
                pexp_desc: ExpressionDesc::Pexp_ident(Loc {
                    txt: {
                        let str_idx = arena.push_string(full_module_name);
                        arena.push_longident(Longident::Lident(str_idx))
                    },
                    loc: empty_loc(),
                }),
                pexp_loc: empty_loc(),
                pexp_attributes: vec![],
            }),
        ),
        pexp_loc: empty_loc(),
        pexp_attributes: vec![],
    };

    // Create the new binding
    // OCaml's make_new_binding uses empty_loc for both pvb_loc and ppat_loc
    let new_binding = ValueBinding {
        pvb_pat: Pattern {
            ppat_desc: PatternDesc::Ppat_var(Loc { txt: fn_name.clone(), loc: empty_loc() }),
            ppat_loc: empty_loc(),
            ppat_attributes: vec![],
        },
        pvb_expr: internal_expr,
        pvb_attributes: filtered_attrs.clone(),
        pvb_loc: empty_loc(),
    };

    // Handle recursive vs non-recursive
    match rec_flag {
        RecFlag::Recursive => {
            // For recursive: modify original binding to be internal, add new binding
            let modified_binding = ValueBinding {
                pvb_pat: Pattern {
                    ppat_desc: PatternDesc::Ppat_var(Loc { txt: internal_fn_name, loc: empty_loc() }),
                    ppat_loc: empty_loc(),
                    ppat_attributes: vec![],
                },
                pvb_expr: transform_expression(binding.pvb_expr, config, arena),
                pvb_attributes: filtered_attrs,
                pvb_loc: binding.pvb_loc,
            };
            (modified_binding, Some(new_binding))
        }
        RecFlag::Nonrecursive => {
            // For non-recursive: wrap the function body with React.element constraint
            let transformed_expr = transform_expression(binding.pvb_expr, config, arena);
            let constrained_expr = wrap_function_body_with_constraint(arena, transformed_expr, config);
            // OCaml line 905-909: clear pexp_attributes (they were moved to wrapper_expr)
            let constrained_expr = Expression {
                pexp_attributes: vec![],
                ..constrained_expr
            };
            let modified_binding = ValueBinding {
                pvb_pat: binding.pvb_pat,
                pvb_expr: constrained_expr,
                pvb_attributes: filtered_attrs,
                pvb_loc: binding.pvb_loc,
            };
            (modified_binding, Some(new_binding))
        }
    }
}

/// Wrap the innermost body of a function expression with constrain_jsx_return
fn wrap_function_body_with_constraint(arena: &mut ParseArena, expr: Expression, config: &JsxConfig) -> Expression {
    match expr.pexp_desc {
        ExpressionDesc::Pexp_fun { arg_label, default, lhs, rhs, arity, is_async } => {
            // Recursively wrap the body
            let wrapped_rhs = wrap_function_body_with_constraint(arena, *rhs, config);
            Expression {
                pexp_desc: ExpressionDesc::Pexp_fun {
                    arg_label,
                    default,
                    lhs,
                    rhs: Box::new(wrapped_rhs),
                    arity,
                    is_async,
                },
                pexp_loc: expr.pexp_loc,
                pexp_attributes: expr.pexp_attributes,
            }
        }
        _ => {
            // This is the innermost body - wrap it with React.element constraint
            constrain_jsx_return(arena, expr, config)
        }
    }
}

/// Create props pattern for componentWithProps based on the original function's pattern
fn make_with_props_pattern(arena: &mut ParseArena, expr: &Expression) -> Pattern {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_fun { lhs, .. } => {
            match &lhs.ppat_desc {
                PatternDesc::Ppat_constraint(_, typ) => {
                    match &typ.ptyp_desc {
                        CoreTypeDesc::Ptyp_constr(loc, args) if arena.is_lident(loc.txt, "props") => {
                            let props_str_idx = arena.intern_string("props");
                            let props_lid = arena.push_longident(Longident::Lident(props_str_idx));
                            if !args.is_empty() {
                                // props<_>
                                Pattern {
                                    ppat_desc: PatternDesc::Ppat_constraint(
                                        Box::new(Pattern {
                                            ppat_desc: PatternDesc::Ppat_var(Loc { txt: "props".to_string(), loc: empty_loc() }),
                                            ppat_loc: empty_loc(),
                                            ppat_attributes: vec![],
                                        }),
                                        CoreType {
                                            ptyp_desc: CoreTypeDesc::Ptyp_constr(
                                                Loc { txt: props_lid, loc: empty_loc() },
                                                vec![CoreType {
                                                    ptyp_desc: CoreTypeDesc::Ptyp_any,
                                                    ptyp_loc: empty_loc(),
                                                    ptyp_attributes: vec![],
                                                }],
                                            ),
                                            ptyp_loc: empty_loc(),
                                            ptyp_attributes: vec![],
                                        },
                                    ),
                                    ppat_loc: empty_loc(),
                                    ppat_attributes: vec![],
                                }
                            } else {
                                // props
                                Pattern {
                                    ppat_desc: PatternDesc::Ppat_constraint(
                                        Box::new(Pattern {
                                            ppat_desc: PatternDesc::Ppat_var(Loc { txt: "props".to_string(), loc: empty_loc() }),
                                            ppat_loc: empty_loc(),
                                            ppat_attributes: vec![],
                                        }),
                                        CoreType {
                                            ptyp_desc: CoreTypeDesc::Ptyp_constr(
                                                Loc { txt: props_lid, loc: empty_loc() },
                                                vec![],
                                            ),
                                            ptyp_loc: empty_loc(),
                                            ptyp_attributes: vec![],
                                        },
                                    ),
                                    ppat_loc: empty_loc(),
                                    ppat_attributes: vec![],
                                }
                            }
                        }
                        _ => simple_props_pattern(),
                    }
                }
                _ => simple_props_pattern(),
            }
        }
        _ => simple_props_pattern(),
    }
}

fn simple_props_pattern() -> Pattern {
    Pattern {
        ppat_desc: PatternDesc::Ppat_var(Loc { txt: "props".to_string(), loc: empty_loc() }),
        ppat_loc: empty_loc(),
        ppat_attributes: vec![],
    }
}

/// Wrap expression with Jsx.promise for async components
fn wrap_with_jsx_promise(arena: &mut ParseArena, expr: Expression) -> Expression {
    let jsx_idx = arena.intern_string("Jsx");
    let promise_idx = arena.intern_string("promise");
    Expression {
        pexp_desc: ExpressionDesc::Pexp_apply {
            funct: Box::new(Expression {
                pexp_desc: ExpressionDesc::Pexp_ident(Loc {
                    txt: arena.push_longident(Longident::Ldot(
                        Box::new(Longident::Lident(jsx_idx)),
                        promise_idx,
                    )),
                    loc: empty_loc(),
                }),
                pexp_loc: empty_loc(),
                pexp_attributes: vec![],
            }),
            args: vec![(ArgLabel::Nolabel, expr)],
            partial: false,
            transformed_jsx: false,
        },
        pexp_loc: empty_loc(),
        pexp_attributes: vec![],
    }
}

/// Check if expression is an async function
fn is_async_function(expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_fun { is_async, .. } => *is_async,
        ExpressionDesc::Pexp_constraint(inner, _) => is_async_function(inner),
        _ => false,
    }
}

///// Create wrapper expression: props => fnName(props)
/// OCaml's make_props_pattern only adds type constraint when there are props
fn make_props_wrapper_expr(arena: &mut ParseArena, fn_name: &str, _rec_flag: RecFlag, _config: &JsxConfig, has_props: bool) -> Expression {
    let props_pattern = Pattern {
        ppat_desc: PatternDesc::Ppat_var(Loc { txt: "props".to_string(), loc: empty_loc() }),
        ppat_loc: empty_loc(),
        ppat_attributes: vec![],
    };

    // Only constrain with props<_> if there are props (matches OCaml's make_props_pattern)
    let final_pattern = if has_props {
        let props_str_idx = arena.intern_string("props");
        let props_type = CoreType {
            ptyp_desc: CoreTypeDesc::Ptyp_constr(
                Loc { txt: arena.push_longident(Longident::Lident(props_str_idx)), loc: empty_loc() },
                vec![CoreType {
                    ptyp_desc: CoreTypeDesc::Ptyp_any,
                    ptyp_loc: empty_loc(),
                    ptyp_attributes: vec![],
                }],
            ),
            ptyp_loc: empty_loc(),
            ptyp_attributes: vec![],
        };
        Pattern {
            ppat_desc: PatternDesc::Ppat_constraint(Box::new(props_pattern), props_type),
            ppat_loc: empty_loc(),
            ppat_attributes: vec![],
        }
    } else {
        props_pattern
    };

    let fn_str_idx = arena.push_string(fn_name.to_string());
    let props_str_idx = arena.intern_string("props");
    let apply_expr = Expression {
        pexp_desc: ExpressionDesc::Pexp_apply {
            funct: Box::new(Expression {
                pexp_desc: ExpressionDesc::Pexp_ident(Loc {
                    txt: arena.push_longident(Longident::Lident(fn_str_idx)),
                    loc: empty_loc(),
                }),
                pexp_loc: empty_loc(),
                pexp_attributes: vec![],
            }),
            args: vec![(
                ArgLabel::Nolabel,
                Expression {
                    pexp_desc: ExpressionDesc::Pexp_ident(Loc {
                        txt: arena.push_longident(Longident::Lident(props_str_idx)),
                        loc: empty_loc(),
                    }),
                    pexp_loc: empty_loc(),
                    pexp_attributes: vec![],
                },
            )],
            partial: false,
            transformed_jsx: false,
        },
        pexp_loc: empty_loc(),
        pexp_attributes: vec![],
    };

    // Note: The wrapper does NOT get React.element constraint (only main body does)
    Expression {
        pexp_desc: ExpressionDesc::Pexp_fun {
            arg_label: ArgLabel::Nolabel,
            default: None,
            lhs: final_pattern,
            rhs: Box::new(apply_expr),
            arity: Arity::Full(1),
            is_async: false,
        },
        pexp_loc: empty_loc(),
        pexp_attributes: vec![],
    }
}

/// Create wrapper expression with forwardRef support: (props, ref) => fnName(props, ref)
/// expr_attrs: the original expression's attributes (e.g., @directive) to put on the outer Pexp_fun
/// For recursive components, calls fn_name$Internal instead of fn_name
fn make_props_wrapper_expr_with_ref(arena: &mut ParseArena, fn_name: &str, rec_flag: RecFlag, _config: &JsxConfig, has_props: bool, has_forward_ref: bool, is_async: bool, expr_attrs: Attributes) -> Expression {
    let props_pattern = Pattern {
        ppat_desc: PatternDesc::Ppat_var(Loc { txt: "props".to_string(), loc: empty_loc() }),
        ppat_loc: empty_loc(),
        ppat_attributes: vec![],
    };

    // Only constrain with props<_> if there are props
    let final_pattern = if has_props {
        let props_str_idx = arena.intern_string("props");
        let props_type = CoreType {
            ptyp_desc: CoreTypeDesc::Ptyp_constr(
                Loc { txt: arena.push_longident(Longident::Lident(props_str_idx)), loc: empty_loc() },
                vec![CoreType {
                    ptyp_desc: CoreTypeDesc::Ptyp_any,
                    ptyp_loc: empty_loc(),
                    ptyp_attributes: vec![],
                }],
            ),
            ptyp_loc: empty_loc(),
            ptyp_attributes: vec![],
        };
        Pattern {
            ppat_desc: PatternDesc::Ppat_constraint(Box::new(props_pattern), props_type),
            ppat_loc: empty_loc(),
            ppat_attributes: vec![],
        }
    } else {
        props_pattern
    };

    // Build arguments for the apply expression
    let props_str_idx2 = arena.intern_string("props");
    let mut apply_args = vec![(
        ArgLabel::Nolabel,
        Expression {
            pexp_desc: ExpressionDesc::Pexp_ident(Loc {
                txt: arena.push_longident(Longident::Lident(props_str_idx2)),
                loc: empty_loc(),
            }),
            pexp_loc: empty_loc(),
            pexp_attributes: vec![],
        },
    )];

    // For forwardRef, add ref argument
    if has_forward_ref {
        let ref_str_idx = arena.intern_string("ref");
        apply_args.push((
            ArgLabel::Nolabel,
            Expression {
                pexp_desc: ExpressionDesc::Pexp_ident(Loc {
                    txt: arena.push_longident(Longident::Lident(ref_str_idx)),
                    loc: empty_loc(),
                }),
                pexp_loc: empty_loc(),
                pexp_attributes: vec![],
            },
        ));
    }

    // For recursive components, call fn_name$Internal; otherwise call fn_name
    let called_fn_name = match rec_flag {
        RecFlag::Recursive => format!("{}$Internal", fn_name),
        RecFlag::Nonrecursive => fn_name.to_string(),
    };
    let fn_str_idx2 = arena.push_string(called_fn_name);
    let apply_expr = Expression {
        pexp_desc: ExpressionDesc::Pexp_apply {
            funct: Box::new(Expression {
                pexp_desc: ExpressionDesc::Pexp_ident(Loc {
                    txt: arena.push_longident(Longident::Lident(fn_str_idx2)),
                    loc: empty_loc(),
                }),
                pexp_loc: empty_loc(),
                pexp_attributes: vec![],
            }),
            args: apply_args,
            partial: false,
            transformed_jsx: false,
        },
        pexp_loc: empty_loc(),
        pexp_attributes: vec![],
    };

    // Wrap with Jsx.promise for async components
    let apply_expr = if is_async {
        wrap_with_jsx_promise(arena, apply_expr)
    } else {
        apply_expr
    };

    let total_arity = if has_forward_ref { 2 } else { 1 };

    // For forwardRef, wrap with an additional ref parameter
    let inner_expr = if has_forward_ref {
        let ref_pattern = Pattern {
            ppat_desc: PatternDesc::Ppat_var(Loc { txt: "ref".to_string(), loc: empty_loc() }),
            ppat_loc: empty_loc(),
            ppat_attributes: vec![],
        };
        Expression {
            pexp_desc: ExpressionDesc::Pexp_fun {
                arg_label: ArgLabel::Nolabel,
                default: None,
                lhs: ref_pattern,
                rhs: Box::new(apply_expr),
                arity: Arity::Unknown,
                is_async: false,
            },
            pexp_loc: empty_loc(),
            pexp_attributes: vec![],
        }
    } else {
        apply_expr
    };

    // The wrapper expression does NOT have is_async (OCaml line 632 uses Exp.fun_ without ~async)
    // The is_async parameter is only used for wrapping with Jsx.promise
    // OCaml line 632: ~attrs:binding.pvb_expr.pexp_attributes
    Expression {
        pexp_desc: ExpressionDesc::Pexp_fun {
            arg_label: ArgLabel::Nolabel,
            default: None,
            lhs: final_pattern,
            rhs: Box::new(inner_expr),
            arity: Arity::Full(total_arity),
            is_async: false,
        },
        pexp_loc: empty_loc(),
        pexp_attributes: expr_attrs,
    }
}

/// Strip Ppat_constraint from a pattern, EXCEPT for Ptyp_package constraints (first-class modules)
/// Matches OCaml's strip_constraint_unpack
fn strip_pattern_constraint(pat: &Pattern) -> Pattern {
    match &pat.ppat_desc {
        PatternDesc::Ppat_constraint(inner, core_type) => {
            // Keep first-class module constraints (Ptyp_package)
            if matches!(core_type.ptyp_desc, CoreTypeDesc::Ptyp_package(..)) {
                pat.clone()
            } else {
                // Recursively strip other constraints
                strip_pattern_constraint(inner)
            }
        }
        _ => pat.clone(),
    }
}

/// Build patterns for props record and the function body
fn build_props_pattern_and_body(
    expr: &Expression,
    args: &[ExtractedArg],
    config: &JsxConfig,
    arena: &mut ParseArena,
) -> (Vec<PatternRecordField>, Expression) {
    let mut patterns = vec![];

    for arg in args {
        let label = get_label(arena, &arg.label);
        if label.is_empty() {
            continue;
        }

        // OCaml strips constraint from pattern when building record field
        let base_pat = strip_pattern_constraint(&arg.pattern);

        // For optional args with default, prefix with __
        let mut pat = if arg.default.is_some() {
            let mut p = base_pat;
            prefix_pattern_var(&mut p, "__");
            p
        } else {
            base_pat
        };

        // Preserve original pattern attributes (OCaml does: pattern_with_safe_label with ppat_attributes = pattern.ppat_attributes)
        pat.ppat_attributes = arg.pattern.ppat_attributes.clone();

        let label_str_idx = arena.push_string(label.clone());
        patterns.push(PatternRecordField {
            lid: Loc {
                txt: arena.push_longident(Longident::Lident(label_str_idx)),
                loc: arg.loc.clone(),
            },
            pat,
            opt: is_optional(&arg.label),
        });
    }

    // Get the body expression (innermost non-fun expression)
    let body = get_function_body(expr);
    let transformed_body = transform_expression(body.clone(), config, arena);

    // Constrain return type to React.element
    let constrained_body = constrain_jsx_return(arena, transformed_body, config);

    // Add default value handling
    let final_body = add_default_value_matches(arena, constrained_body, args);

    (patterns, final_body)
}

/// Add pattern matching for optional props with default values
fn add_default_value_matches(arena: &mut ParseArena, mut expr: Expression, args: &[ExtractedArg]) -> Expression {
    for arg in args.iter().rev() {
        if let Some(default) = &arg.default {
            // Use the label name for the match pattern, not the alias
            // For ~foo as bar, we use "foo" in the match
            let label_name = get_label(arena, &arg.label);
            let alias = &arg.alias;
            let prefixed = format!("__{}", alias);

            // Create: let alias = switch __alias { | Some(labelName) => labelName | None => default }
            let prefixed_str_idx = arena.push_string(prefixed);
            let some_str_idx = arena.intern_string("Some");
            let none_str_idx = arena.intern_string("None");
            let label_name_str_idx = arena.push_string(label_name.clone());
            let match_expr = Expression {
                pexp_desc: ExpressionDesc::Pexp_match(
                    Box::new(Expression {
                        pexp_desc: ExpressionDesc::Pexp_ident(Loc {
                            txt: arena.push_longident(Longident::Lident(prefixed_str_idx)),
                            loc: empty_loc(),
                        }),
                        pexp_loc: empty_loc(),
                        pexp_attributes: vec![],
                    }),
                    vec![
                        Case {
                            pc_bar: None,
                            pc_lhs: Pattern {
                                ppat_desc: PatternDesc::Ppat_construct(
                                    Loc { txt: arena.push_longident(Longident::Lident(some_str_idx)), loc: empty_loc() },
                                    Some(Box::new(Pattern {
                                        ppat_desc: PatternDesc::Ppat_var(Loc {
                                            txt: label_name.clone(),
                                            loc: empty_loc(),
                                        }),
                                        ppat_loc: empty_loc(),
                                        ppat_attributes: vec![],
                                    })),
                                ),
                                ppat_loc: empty_loc(),
                                ppat_attributes: vec![],
                            },
                            pc_guard: None,
                            pc_rhs: Expression {
                                pexp_desc: ExpressionDesc::Pexp_ident(Loc {
                                    txt: arena.push_longident(Longident::Lident(label_name_str_idx)),
                                    loc: empty_loc(),
                                }),
                                pexp_loc: empty_loc(),
                                pexp_attributes: vec![],
                            },
                        },
                        Case {
                            pc_bar: None,
                            pc_lhs: Pattern {
                                ppat_desc: PatternDesc::Ppat_construct(
                                    Loc { txt: arena.push_longident(Longident::Lident(none_str_idx)), loc: empty_loc() },
                                    None,
                                ),
                                ppat_loc: empty_loc(),
                                ppat_attributes: vec![],
                            },
                            pc_guard: None,
                            pc_rhs: default.clone(),
                        },
                    ],
                ),
                pexp_loc: empty_loc(),
                pexp_attributes: vec![],
            };

            // OCaml uses the original arg location for the let binding pattern
            let binding = ValueBinding {
                pvb_pat: Pattern {
                    ppat_desc: PatternDesc::Ppat_var(Loc { txt: alias.clone(), loc: arg.loc.clone() }),
                    ppat_loc: empty_loc(),
                    ppat_attributes: vec![],
                },
                pvb_expr: match_expr,
                pvb_attributes: vec![],
                pvb_loc: empty_loc(),
            };

            expr = Expression {
                pexp_desc: ExpressionDesc::Pexp_let(RecFlag::Nonrecursive, vec![binding], Box::new(expr)),
                pexp_loc: empty_loc(),
                pexp_attributes: vec![],
            };
        }
    }
    expr
}

/// Get the innermost body of a function expression
fn get_function_body(expr: &Expression) -> Expression {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_fun { rhs, .. } => get_function_body(rhs),
        ExpressionDesc::Pexp_newtype(_, body) => get_function_body(body),
        ExpressionDesc::Pexp_constraint(inner, _) => get_function_body(inner),
        _ => expr.clone(),
    }
}

/// Prefix variable names in a pattern with a string
fn prefix_pattern_var(pat: &mut Pattern, prefix: &str) {
    match &mut pat.ppat_desc {
        PatternDesc::Ppat_var(loc) => {
            loc.txt = format!("{}{}", prefix, loc.txt);
        }
        PatternDesc::Ppat_alias(_, loc) => {
            loc.txt = format!("{}{}", prefix, loc.txt);
        }
        PatternDesc::Ppat_constraint(inner, _) => {
            prefix_pattern_var(inner, prefix);
        }
        _ => {}
    }
}

// ============================================================================
// Signature Item Transformation
// ============================================================================

fn transform_signature_item_multi(item: SignatureItem, config: &mut JsxConfig, arena: &mut ParseArena) -> Vec<SignatureItem> {
    // Check for @@jsxConfig attribute
    if let SignatureItemDesc::Psig_attribute(attr) = &item.psig_desc {
        if attr.0.txt == "jsxConfig" {
            update_config_from_attribute(arena, attr, config);
        }
        return vec![item];
    }

    // Only transform if version is 4
    if config.version != 4 {
        return vec![item];
    }

    // Handle Psig_value with potential @react.component
    if let SignatureItemDesc::Psig_value(vd) = &item.psig_desc {
        if has_react_component_attr(&vd.pval_attributes) {
            return transform_react_component_sig(vd.clone(), &item.psig_loc, config, arena);
        }
    }

    vec![SignatureItem {
        psig_desc: transform_signature_item_desc(item.psig_desc, config, arena),
        psig_loc: item.psig_loc,
    }]
}

fn transform_react_component_sig(
    vd: ValueDescription,
    loc: &Location,
    config: &mut JsxConfig,
    arena: &mut ParseArena,
) -> Vec<SignatureItem> {
    // Check for multiple components in the same module (OCaml: check_multiple_components)
    if config.has_component {
        eprintln!(
            "Only one component definition is allowed for each module. Move to a submodule or other file if necessary. (at {:?})",
            loc
        );
    }
    config.has_component = true;

    // Check for sharedProps: @react.component(:sharedProps<T>)
    let core_type_of_attr = core_type_of_attrs(&vd.pval_attributes);
    let typ_vars_of_core_type: Vec<CoreType> = core_type_of_attr.as_ref()
        .map(typ_vars_of_core_type)
        .unwrap_or_default();

    // Extract prop types from the function type
    let prop_types = collect_prop_types(&vd.pval_type);
    let named_type_list: Vec<NamedType> = prop_types.into_iter()
        .filter_map(|(label, attrs, loc, typ)| {
            let label_str = get_label(arena, &label);
            if label_str.is_empty() {
                None
            } else {
                Some(NamedType {
                    is_optional: is_optional(&label),
                    label: label_str,
                    attrs,
                    loc,
                    interior_type: typ,
                    has_explicit_type: true, // In signatures, types are always explicit
                })
            }
        })
        .collect();

    // Create props type - either record or abstract with manifest (for sharedProps)
    // For sharedProps signatures, don't add the live/jsxComponentProps attributes
    let props_type = if let Some(manifest) = &core_type_of_attr {
        // sharedProps signature: no extra attributes
        make_props_abstract_type_sig("props", loc, manifest, &typ_vars_of_core_type)
    } else {
        make_props_record_type_sig("props", loc, &named_type_list)
    };

    // Filter attributes
    let filtered_attrs: Attributes = vd.pval_attributes
        .iter()
        .filter(|attr| !is_react_component_attr(attr))
        .cloned()
        .collect();

    // Create props type ref with appropriate type params
    // For sharedProps without type variables: no type args
    // For sharedProps with type variables: use Ptyp_any for each type var
    // For non-sharedProps: use the interior types
    let props_type_params: Vec<CoreType> = if core_type_of_attr.is_some() {
        if typ_vars_of_core_type.is_empty() {
            // sharedProps without type vars: no type arguments
            vec![]
        } else {
            // sharedProps with type vars: use Ptyp_any for each type var
            typ_vars_of_core_type.iter()
                .map(|_| CoreType {
                    ptyp_desc: CoreTypeDesc::Ptyp_any,
                    ptyp_loc: empty_loc(),
                    ptyp_attributes: vec![],
                })
                .collect()
        }
    } else {
        named_type_list.iter()
            .filter(|nt| nt.label != "key")
            .map(|nt| nt.interior_type.clone())
            .collect()
    };

    // Create new value description with component type
    // OCaml: Typ.constr (Location.mkloc (Lident "props") psig_loc) params
    // Typ.constr without ~loc uses Location.none for ptyp_loc
    let props_str_idx = arena.intern_string("props");
    let props_type_ref = CoreType {
        ptyp_desc: CoreTypeDesc::Ptyp_constr(
            Loc { txt: arena.push_longident(Longident::Lident(props_str_idx)), loc: loc.clone() }, // longident uses psig_loc
            props_type_params,
        ),
        ptyp_loc: empty_loc(), // OCaml: Typ.constr without ~loc defaults to Location.none
        ptyp_attributes: vec![],
    };

    // OCaml: {pval_type with ptyp_desc = new_external_type}
    // This preserves pval_type's original ptyp_loc
    let react_str_idx = arena.intern_string("React");
    let component_str_idx = arena.intern_string("component");
    let component_type = CoreType {
        ptyp_desc: CoreTypeDesc::Ptyp_constr(
            Loc {
                txt: arena.push_longident(Longident::Ldot(Box::new(Longident::Lident(react_str_idx)), component_str_idx)),
                loc: loc.clone(), // OCaml: {loc = psig_loc; txt = ...}
            },
            vec![props_type_ref],
        ),
        ptyp_loc: vd.pval_type.ptyp_loc.clone(), // OCaml: {pval_type with ...} preserves ptyp_loc
        ptyp_attributes: vec![],
    };

    let new_vd = ValueDescription {
        pval_name: vd.pval_name,
        pval_type: component_type,
        pval_prim: vd.pval_prim,
        pval_attributes: filtered_attrs,
        pval_loc: vd.pval_loc,
    };

    vec![
        props_type,
        SignatureItem {
            psig_desc: SignatureItemDesc::Psig_value(new_vd),
            psig_loc: loc.clone(),
        },
    ]
}

/// Transform @react.component on external declaration
fn transform_react_component_external(
    vd: ValueDescription,
    loc: &Location,
    config: &mut JsxConfig,
    arena: &mut ParseArena,
) -> Vec<StructureItem> {
    // Check for multiple components in the same module (OCaml: check_multiple_components)
    if config.has_component {
        eprintln!(
            "Only one component definition is allowed for each module. Move to a submodule or other file if necessary. (at {:?})",
            loc
        );
    }
    config.has_component = true;

    // Check for sharedProps: @react.component(:sharedProps<T>)
    let core_type_of_attr = core_type_of_attrs(&vd.pval_attributes);
    let typ_vars_of_core_type: Vec<CoreType> = core_type_of_attr.as_ref()
        .map(typ_vars_of_core_type)
        .unwrap_or_default();

    // Extract prop types from the function type
    let prop_types = collect_prop_types(&vd.pval_type);
    let named_type_list: Vec<NamedType> = prop_types.into_iter()
        .filter_map(|(label, attrs, loc, typ)| {
            let label_str = get_label(arena, &label);
            if label_str.is_empty() {
                None
            } else {
                Some(NamedType {
                    is_optional: is_optional(&label),
                    label: label_str,
                    attrs,
                    loc,
                    interior_type: typ,
                    has_explicit_type: true,
                })
            }
        })
        .collect();

    // Create props type - either record or abstract with manifest (for sharedProps)
    // For sharedProps externals, don't add the live/jsxComponentProps attributes
    let props_type = if let Some(manifest) = &core_type_of_attr {
        // sharedProps external: no extra attributes
        make_props_abstract_type("props", loc, manifest, &typ_vars_of_core_type)
    } else {
        // regular external: add live attribute
        make_props_record_type_with_live("props", loc, &named_type_list)
    };

    // Filter attributes (remove @react.component)
    let filtered_attrs: Attributes = vd.pval_attributes
        .iter()
        .filter(|attr| !is_react_component_attr(attr))
        .cloned()
        .collect();

    // Create props type ref with appropriate type params
    // For sharedProps without type variables: no type args
    // For sharedProps with type variables: use Ptyp_any for each type var
    // For non-sharedProps: use the interior types
    let props_type_params: Vec<CoreType> = if core_type_of_attr.is_some() {
        if typ_vars_of_core_type.is_empty() {
            // sharedProps without type vars: no type arguments
            vec![]
        } else {
            // sharedProps with type vars: use Ptyp_any for each type var
            typ_vars_of_core_type.iter()
                .map(|_| CoreType {
                    ptyp_desc: CoreTypeDesc::Ptyp_any,
                    ptyp_loc: empty_loc(),
                    ptyp_attributes: vec![],
                })
                .collect()
        }
    } else {
        named_type_list.iter()
            .filter(|nt| nt.label != "key")
            .map(|nt| nt.interior_type.clone())
            .collect()
    };

    // OCaml uses the external's full location for the props type and its longident
    let props_str_idx = arena.intern_string("props");
    let props_type_ref = CoreType {
        ptyp_desc: CoreTypeDesc::Ptyp_constr(
            Loc { txt: arena.push_longident(Longident::Lident(props_str_idx)), loc: loc.clone() },
            props_type_params,
        ),
        ptyp_loc: loc.clone(),
        ptyp_attributes: vec![],
    };

    // OCaml uses the original return type's location for the outer core_type,
    // and the external's full location for the longident
    let module_str_idx = arena.push_string(config.module_name.clone());
    let component_str_idx = arena.intern_string("component");
    let component_type = CoreType {
        ptyp_desc: CoreTypeDesc::Ptyp_constr(
            Loc {
                txt: arena.push_longident(Longident::Ldot(
                    Box::new(Longident::Lident(module_str_idx)),
                    component_str_idx,
                )),
                loc: loc.clone(),
            },
            vec![props_type_ref],
        ),
        ptyp_loc: vd.pval_type.ptyp_loc.clone(),
        ptyp_attributes: vec![],
    };

    let new_vd = ValueDescription {
        pval_name: vd.pval_name,
        pval_type: component_type,
        pval_prim: vd.pval_prim,
        pval_attributes: filtered_attrs,
        pval_loc: vd.pval_loc,
    };

    vec![
        props_type,
        StructureItem {
            pstr_desc: StructureItemDesc::Pstr_primitive(new_vd),
            pstr_loc: loc.clone(),
        },
    ]
}

/// Create props record type with @live attribute (for externals)
fn make_props_record_type_with_live(name: &str, loc: &Location, named_types: &[NamedType]) -> StructureItem {
    // Check for duplicate labels (OCaml: check_duplicated_label)
    check_duplicated_labels(named_types);

    let label_decls: Vec<LabelDeclaration> = named_types.iter()
        .map(|nt| {
            // OCaml uses ghost location for the Ptyp_var inside label declarations
            let type_var = CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_var(safe_type_from_value(&nt.label)),
                ptyp_loc: empty_loc(),
                ptyp_attributes: vec![],
            };

            // Key prop is always optional in type declaration (OCaml: if label = "key" then ~optional:true)
            let is_optional = if nt.label == "key" { true } else { nt.is_optional };

            LabelDeclaration {
                pld_name: Loc { txt: nt.label.clone(), loc: nt.loc.clone() },
                pld_mutable: MutableFlag::Immutable,
                pld_type: type_var,
                pld_loc: nt.loc.clone(),
                pld_attributes: nt.attrs.clone(),
                pld_optional: is_optional,
            }
        })
        .collect();

    // Type parameters - these keep the real location (OCaml uses real locs for ptype_params)
    let type_params: Vec<(CoreType, Variance)> = named_types.iter()
        .filter(|nt| nt.label != "key")
        .map(|nt| {
            let var = CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_var(safe_type_from_value(&nt.label)),
                ptyp_loc: nt.loc.clone(),
                ptyp_attributes: vec![],
            };
            (var, Variance::Invariant)
        })
        .collect();

    // Create @res.jsxComponentProps and @live attributes
    let jsx_props_attr = (
        Loc { txt: "res.jsxComponentProps".to_string(), loc: empty_loc() },
        Payload::PStr(vec![]),
    );
    let live_attr = (
        Loc { txt: "live".to_string(), loc: empty_loc() },
        Payload::PStr(vec![]),
    );

    // OCaml uses ghost location for the generated props type structure_item
    StructureItem {
        pstr_desc: StructureItemDesc::Pstr_type(
            RecFlag::Nonrecursive,
            vec![TypeDeclaration {
                ptype_name: Loc { txt: name.to_string(), loc: loc.clone() },
                ptype_params: type_params,
                ptype_cstrs: vec![],
                ptype_kind: TypeKind::Ptype_record(label_decls),
                ptype_private: PrivateFlag::Public,
                ptype_manifest: None,
                ptype_attributes: vec![jsx_props_attr, live_attr],
                ptype_loc: loc.clone(),
            }],
        ),
        pstr_loc: empty_loc(),
    }
}

/// Collect prop types from a function type
fn collect_prop_types(typ: &CoreType) -> Vec<(ArgLabel, Attributes, Location, CoreType)> {
    let mut types = vec![];
    collect_prop_types_rec(typ, &mut types);
    types
}

fn collect_prop_types_rec(typ: &CoreType, types: &mut Vec<(ArgLabel, Attributes, Location, CoreType)>) {
    if let CoreTypeDesc::Ptyp_arrow { arg, ret, .. } = &typ.ptyp_desc {
        let label = &arg.lbl;
        if is_labelled(label) || is_optional(label) {
            // OCaml uses different locations based on whether return is another arrow:
            // - If ret is arrow: use typ.ptyp_loc (the enclosing arrow's location)
            // - If ret is not arrow (last arg): use ret.ptyp_loc (return type's location)
            let loc = if matches!(ret.ptyp_desc, CoreTypeDesc::Ptyp_arrow { .. }) {
                typ.ptyp_loc.clone()
            } else {
                ret.ptyp_loc.clone()
            };
            types.push((label.clone(), arg.attrs.clone(), loc, arg.typ.clone()));
        }
        collect_prop_types_rec(ret, types);
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

fn has_react_component_attr(attrs: &Attributes) -> bool {
    attrs.iter().any(is_react_component_attr)
}

fn is_react_component_attr(attr: &Attribute) -> bool {
    matches!(attr.0.txt.as_str(), "react.component" | "jsx.component")
}

fn has_react_component_with_props_attr(attrs: &Attributes) -> bool {
    attrs.iter().any(is_react_component_with_props_attr)
}

fn is_react_component_with_props_attr(attr: &Attribute) -> bool {
    matches!(attr.0.txt.as_str(), "react.componentWithProps" | "jsx.componentWithProps")
}

/// Extract core type from @react.component(:sharedProps<T>) attribute
/// Returns Some(CoreType) if the attribute has a type payload
fn core_type_of_attrs(attrs: &Attributes) -> Option<CoreType> {
    attrs.iter().find_map(|(loc, payload)| {
        match (loc.txt.as_str(), payload) {
            ("react.component" | "jsx.component", Payload::PTyp(core_type)) => Some(core_type.as_ref().clone()),
            _ => None,
        }
    })
}

/// Extract type variables from a core type (for sharedProps<'a, 'b>)
fn typ_vars_of_core_type(typ: &CoreType) -> Vec<CoreType> {
    match &typ.ptyp_desc {
        CoreTypeDesc::Ptyp_constr(_, args) => {
            args.iter()
                .filter(|t| matches!(t.ptyp_desc, CoreTypeDesc::Ptyp_var(_)))
                .cloned()
                .collect()
        }
        _ => vec![],
    }
}

/// Check if an expression is React.forwardRef
fn is_forward_ref(arena: &ParseArena, expr: &Expression) -> bool {
    if let ExpressionDesc::Pexp_ident(loc) = &expr.pexp_desc {
        if let Longident::Ldot(base, field_idx) = arena.get_longident(loc.txt) {
            if let Longident::Lident(m_idx) = base.as_ref() {
                let m = arena.get_string(*m_idx);
                let field = arena.get_string(*field_idx);
                return m == "React" && field == "forwardRef";
            }
        }
    }
    false
}

fn get_fn_name(pat: &Pattern) -> String {
    match &pat.ppat_desc {
        PatternDesc::Ppat_var(loc) => loc.txt.clone(),
        PatternDesc::Ppat_constraint(inner, _) => get_fn_name(inner),
        _ => "make".to_string(),
    }
}

/// Get the location of the function name from a pattern
/// For Ppat_constraint, returns the OUTER constraint's location (not the inner pattern's)
/// This matches OCaml's jsx_v4.ml: `let binding_pat_loc = binding.pvb_pat.ppat_loc`
fn get_fn_name_loc(pat: &Pattern) -> Location {
    match &pat.ppat_desc {
        PatternDesc::Ppat_var(loc) => loc.loc.clone(),
        // For constrained patterns, use the constraint's location (the full span including type annotation)
        // OCaml uses binding.pvb_pat.ppat_loc which is the outer Ppat_constraint's location
        PatternDesc::Ppat_constraint(_, _) => pat.ppat_loc.clone(),
        _ => pat.ppat_loc.clone(),
    }
}

fn make_module_name(config: &JsxConfig, fn_name: &str) -> String {
    // Build component name: file_name + nested_modules (in order, not reversed)
    // If fn_name is not "make", append it to the nested modules
    let file_name = &config.file_name;
    let nested_modules = &config.nested_modules;

    let parts: Vec<String> = match (file_name.is_empty(), fn_name == "make") {
        (true, true) => {
            // No file_name, fn_name is "make": just use nested_modules
            nested_modules.clone()
        }
        (true, false) => {
            // No file_name, fn_name is not "make": nested_modules + fn_name
            let mut v = nested_modules.clone();
            v.push(fn_name.to_string());
            v
        }
        (false, true) => {
            // Has file_name, fn_name is "make": file_name + nested_modules
            let mut v = vec![file_name.clone()];
            v.extend(nested_modules.iter().cloned());
            v
        }
        (false, false) => {
            // Has file_name, fn_name is not "make": file_name + nested_modules + fn_name
            let mut v = vec![file_name.clone()];
            v.extend(nested_modules.iter().cloned());
            v.push(fn_name.to_string());
            v
        }
    };

    parts.join("$")
}

/// Strip `option<T>` wrapper from a type, returning just `T`
fn strip_option(arena: &ParseArena, core_type: &CoreType) -> Option<CoreType> {
    match &core_type.ptyp_desc {
        CoreTypeDesc::Ptyp_constr(lid, args) if args.len() == 1 => {
            let last_idx = arena.get_longident(lid.txt).last_idx();
            let last_str = arena.get_string(last_idx);
            if last_str == "option" {
                args.first().cloned()
            } else {
                Some(core_type.clone())
            }
        }
        _ => Some(core_type.clone()),
    }
}

/// Strip `Js.Nullable.t<T>` wrapper from a type, returning just `T`
fn strip_js_nullable(arena: &ParseArena, core_type: &CoreType) -> Option<CoreType> {
    match &core_type.ptyp_desc {
        CoreTypeDesc::Ptyp_constr(lid, args) if args.len() == 1 => {
            // Check for Js.Nullable.t
            if let Longident::Ldot(outer, inner_idx) = arena.get_longident(lid.txt) {
                let inner = arena.get_string(*inner_idx);
                if inner == "t" {
                    if let Longident::Ldot(js, nullable_idx) = outer.as_ref() {
                        let nullable = arena.get_string(*nullable_idx);
                        if nullable == "Nullable" {
                            if let Longident::Lident(js_name_idx) = js.as_ref() {
                                let js_name = arena.get_string(*js_name_idx);
                                if js_name == "Js" {
                                    return args.first().cloned();
                                }
                            }
                        }
                    }
                }
            }
            Some(core_type.clone())
        }
        _ => Some(core_type.clone()),
    }
}

/// Create React.element type constraint
fn jsx_element_type(arena: &mut ParseArena, config: &JsxConfig, loc: &Location) -> CoreType {
    let module_str_idx = arena.push_string(config.module_name.clone());
    let element_str_idx = arena.intern_string("element");
    CoreType {
        ptyp_desc: CoreTypeDesc::Ptyp_constr(
            Loc {
                txt: arena.push_longident(Longident::Ldot(
                    Box::new(Longident::Lident(module_str_idx)),
                    element_str_idx,
                )),
                loc: loc.clone(),
            },
            vec![],
        ),
        ptyp_loc: loc.clone(),
        ptyp_attributes: vec![],
    }
}

/// Wrap expression with React.element constraint (recursively through function bodies)
fn constrain_jsx_return(arena: &mut ParseArena, expr: Expression, config: &JsxConfig) -> Expression {
    let loc = &expr.pexp_loc;
    let element_type = jsx_element_type(arena, config, loc);

    match &expr.pexp_desc {
        ExpressionDesc::Pexp_fun { arg_label, default, lhs, rhs, arity, is_async } => {
            Expression {
                pexp_desc: ExpressionDesc::Pexp_fun {
                    arg_label: arg_label.clone(),
                    default: default.clone(),
                    lhs: lhs.clone(),
                    rhs: Box::new(constrain_jsx_return(arena, *rhs.clone(), config)),
                    arity: *arity,
                    is_async: *is_async,
                },
                pexp_loc: expr.pexp_loc.clone(),
                pexp_attributes: expr.pexp_attributes.clone(),
            }
        }
        ExpressionDesc::Pexp_newtype(param, inner) => {
            Expression {
                pexp_desc: ExpressionDesc::Pexp_newtype(
                    param.clone(),
                    Box::new(constrain_jsx_return(arena, *inner.clone(), config)),
                ),
                pexp_loc: expr.pexp_loc.clone(),
                pexp_attributes: expr.pexp_attributes.clone(),
            }
        }
        ExpressionDesc::Pexp_constraint(inner, _) => {
            // Already constrained, wrap the inner with jsx element constraint
            let constrained = constrain_jsx_return(arena, *inner.clone(), config);
            Expression {
                pexp_desc: ExpressionDesc::Pexp_constraint(Box::new(constrained), element_type),
                pexp_loc: expr.pexp_loc.clone(),
                pexp_attributes: expr.pexp_attributes.clone(),
            }
        }
        ExpressionDesc::Pexp_let(rec_flag, bindings, body) => {
            Expression {
                pexp_desc: ExpressionDesc::Pexp_let(
                    *rec_flag,
                    bindings.clone(),
                    Box::new(constrain_jsx_return(arena, *body.clone(), config)),
                ),
                pexp_loc: expr.pexp_loc.clone(),
                pexp_attributes: expr.pexp_attributes.clone(),
            }
        }
        ExpressionDesc::Pexp_sequence(first, second) => {
            Expression {
                pexp_desc: ExpressionDesc::Pexp_sequence(
                    first.clone(),
                    Box::new(constrain_jsx_return(arena, *second.clone(), config)),
                ),
                pexp_loc: expr.pexp_loc.clone(),
                pexp_attributes: expr.pexp_attributes.clone(),
            }
        }
        _ => {
            // Default: wrap with constraint
            // OCaml uses ghost location for the synthesized Pexp_constraint wrapper
            Expression {
                pexp_desc: ExpressionDesc::Pexp_constraint(Box::new(expr.clone()), element_type),
                pexp_loc: empty_loc(),
                pexp_attributes: vec![],
            }
        }
    }
}

fn safe_type_from_value(value: &str) -> String {
    if value.is_empty() || !value.starts_with('_') {
        value.to_string()
    } else {
        format!("T{}", value)
    }
}

/// Capitalize the first character of a string (like OCaml's String.capitalize_ascii)
fn capitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
    }
}

/// Build module access path: capitalize(module_) + "." + value -> parsed as Longident
/// Matches OCaml's module_access_name
fn module_access_name(arena: &mut ParseArena, config: &JsxConfig, value: &str) -> LidentIdx {
    let capitalized = capitalize_first(&config.module_name);
    let cap_str_idx = arena.push_string(capitalized);
    let val_str_idx = arena.intern_string(value);
    arena.push_longident(Longident::Ldot(Box::new(Longident::Lident(cap_str_idx)), val_str_idx))
}

/// Try to find the key prop in a list of JSX props
/// Returns the (label, expression) if found
fn try_find_key_prop(arena: &mut ParseArena, props: &[JsxProp]) -> Option<(ArgLabel, Expression)> {
    for prop in props {
        match prop {
            JsxProp::Punning { optional, name } if name.txt == "key" => {
                let key_str_idx = arena.intern_string("key");
                let label_loc = Located::new(key_str_idx, name.loc.clone());
                let arg_label = if *optional {
                    ArgLabel::Optional(label_loc)
                } else {
                    ArgLabel::Labelled(label_loc)
                };
                // OCaml: Exp.ident without ~loc defaults to Location.none (ghost)
                let expr = Expression {
                    pexp_desc: ExpressionDesc::Pexp_ident(Loc {
                        txt: arena.push_longident(Longident::Lident(key_str_idx)),
                        loc: name.loc.clone(),
                    }),
                    pexp_loc: empty_loc(),
                    pexp_attributes: vec![],
                };
                return Some((arg_label, expr));
            }
            JsxProp::Value { name, optional, value } if name.txt == "key" => {
                let key_str_idx = arena.intern_string(&name.txt);
                let label_loc = Located::new(key_str_idx, name.loc.clone());
                let arg_label = if *optional {
                    ArgLabel::Optional(label_loc)
                } else {
                    ArgLabel::Labelled(label_loc)
                };
                return Some((arg_label, value.clone()));
            }
            _ => {}
        }
    }
    None
}

/// Create a unit expression () at the given location
fn unit_expr(arena: &mut ParseArena, loc: &Location) -> Expression {
    let unit_str_idx = arena.intern_string("()");
    Expression {
        pexp_desc: ExpressionDesc::Pexp_construct(
            Loc { txt: arena.push_longident(Longident::Lident(unit_str_idx)), loc: loc.clone() },
            None,
        ),
        pexp_loc: loc.clone(),
        pexp_attributes: vec![],
    }
}

fn is_optional(label: &ArgLabel) -> bool {
    matches!(label, ArgLabel::Optional(_))
}

fn is_labelled(label: &ArgLabel) -> bool {
    matches!(label, ArgLabel::Labelled(_))
}

fn get_label(arena: &ParseArena, label: &ArgLabel) -> String {
    match label {
        ArgLabel::Labelled(s) | ArgLabel::Optional(s) => arena.get_string(s.txt).to_string(),
        ArgLabel::Nolabel => String::new(),
    }
}

fn empty_loc() -> Location {
    // Use Location::none() to match OCaml's Location.none (serialized as (loc 1 -1 1 -1))
    Location::none()
}

/// Recursively extract named arguments from a function expression
fn recursively_extract_named_args(arena: &ParseArena, expr: &Expression) -> (Vec<ExtractedArg>, Vec<StringLoc>, Option<CoreType>) {
    let mut args = vec![];
    let mut newtypes = vec![];
    extract_args_rec(arena, expr, &mut args, &mut newtypes);
    (args, newtypes, None)
}

fn extract_args_rec(arena: &ParseArena, expr: &Expression, args: &mut Vec<ExtractedArg>, newtypes: &mut Vec<StringLoc>) {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_fun { arg_label, default, lhs, rhs, .. } => {
            // Check for key argument - this is an error (OCaml: raise_error)
            // Key cannot be accessed inside of a component
            let label_name = get_label(arena, arg_label);
            if label_name == "key" {
                eprintln!(
                    "Key cannot be accessed inside of a component. Don't worry - you can always key a component from its parent! (at {:?})",
                    expr.pexp_loc
                );
                return;
            }

            // Skip unit argument or Ppat_any, but continue extracting from body
            let is_unit_pattern = if let PatternDesc::Ppat_construct(loc, _) = &lhs.ppat_desc {
                arena.is_lident(loc.txt, "()")
            } else {
                false
            };
            if is_unit_pattern || matches!(&lhs.ppat_desc, PatternDesc::Ppat_any)
            {
                if matches!(arg_label, ArgLabel::Nolabel) {
                    // Continue extracting from body even for skipped args (e.g., forwardRef (_, _ref) => ...)
                    extract_args_rec(arena, rhs, args, newtypes);
                    return;
                }
            }

            if is_labelled(arg_label) || is_optional(arg_label) {
                let alias = get_alias_from_pattern(arena, lhs, arg_label);
                let type_ = get_type_from_pattern(lhs);

                args.push(ExtractedArg {
                    label: arg_label.clone(),
                    default: default.as_ref().map(|d| *d.clone()),
                    pattern: lhs.clone(),
                    alias,
                    loc: lhs.ppat_loc.clone(),
                    type_,
                });

                extract_args_rec(arena, rhs, args, newtypes);
            } else if matches!(arg_label, ArgLabel::Nolabel) {
                // For forwardRef, capture unlabeled 'ref' or '_ref' argument
                let alias = get_alias_from_pattern(arena, lhs, arg_label);
                if alias == "ref" || alias == "_ref" {
                    let type_ = get_type_from_pattern(lhs);
                    args.push(ExtractedArg {
                        label: arg_label.clone(),
                        default: None,
                        pattern: lhs.clone(),
                        alias,
                        loc: lhs.ppat_loc.clone(),
                        type_,
                    });
                }
                // Continue extracting from body even for ref
                extract_args_rec(arena, rhs, args, newtypes);
            }
        }
        ExpressionDesc::Pexp_newtype(name, body) => {
            newtypes.push(name.clone());
            extract_args_rec(arena, body, args, newtypes);
        }
        ExpressionDesc::Pexp_constraint(inner, _) => {
            extract_args_rec(arena, inner, args, newtypes);
        }
        _ => {}
    }
}

fn get_alias_from_pattern(arena: &ParseArena, pat: &Pattern, label: &ArgLabel) -> String {
    match &pat.ppat_desc {
        PatternDesc::Ppat_var(loc) => loc.txt.clone(),
        PatternDesc::Ppat_alias(_, loc) => loc.txt.clone(),
        PatternDesc::Ppat_constraint(inner, _) => get_alias_from_pattern(arena, inner, label),
        PatternDesc::Ppat_any => "_".to_string(),
        _ => get_label(arena, label),
    }
}

fn get_type_from_pattern(pat: &Pattern) -> Option<CoreType> {
    match &pat.ppat_desc {
        PatternDesc::Ppat_constraint(_, typ) => {
            // Don't use package types
            if matches!(typ.ptyp_desc, CoreTypeDesc::Ptyp_package(_)) {
                None
            } else {
                Some(typ.clone())
            }
        }
        _ => None,
    }
}

fn arg_to_type(arena: &ParseArena, arg: &ExtractedArg) -> Option<NamedType> {
    let label = get_label(arena, &arg.label);
    // For unlabeled arguments (like 'ref' in forwardRef), use the alias as label
    // Note: "_ref" (underscore-prefixed) means unused ref, should NOT be added to props
    let label = if label.is_empty() {
        if arg.alias == "ref" {
            // Only "ref" (without underscore) gets added to props
            "ref".to_string()
        } else {
            // "_ref" or other unlabeled args are excluded from props
            return None;
        }
    } else {
        label
    };

    let has_explicit_type = arg.type_.is_some();
    let interior_type = arg.type_.clone().unwrap_or_else(|| CoreType {
        ptyp_desc: CoreTypeDesc::Ptyp_any,
        ptyp_loc: arg.loc.clone(),
        ptyp_attributes: vec![],
    });

    Some(NamedType {
        is_optional: is_optional(&arg.label),
        label,
        attrs: arg.pattern.ppat_attributes.clone(),
        loc: arg.loc.clone(),
        interior_type,
        has_explicit_type,
    })
}

/// Create props record type declaration
/// Check for duplicate labels in props (OCaml: check_duplicated_label)
fn check_duplicated_labels(named_types: &[NamedType]) {
    let mut seen = std::collections::HashSet::new();
    for nt in named_types.iter().rev() {
        if !seen.insert(&nt.label) {
            eprintln!(
                "The prop `{}` is defined several times in this component. (at {:?})",
                nt.label, nt.loc
            );
        }
    }
}

fn make_props_record_type(name: &str, loc: &Location, named_types: &[NamedType], has_forward_ref: bool) -> StructureItem {
    // Check for duplicate labels (OCaml: check_duplicated_label)
    check_duplicated_labels(named_types);

    let label_decls: Vec<LabelDeclaration> = named_types.iter()
        .map(|nt| {
            // OCaml uses ghost location for the Ptyp_var inside label declarations
            let type_var = CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_var(safe_type_from_value(&nt.label)),
                ptyp_loc: empty_loc(),
                ptyp_attributes: vec![],
            };

            // Key prop is always optional in type declaration (OCaml: if label = "key" then ~optional:true)
            // For forwardRef, ref is also always optional
            let is_optional = if nt.label == "key" || (nt.label == "ref" && has_forward_ref) { true } else { nt.is_optional };

            LabelDeclaration {
                pld_name: Loc { txt: nt.label.clone(), loc: nt.loc.clone() },
                pld_mutable: MutableFlag::Immutable,
                pld_type: type_var,
                pld_loc: nt.loc.clone(),
                pld_optional: is_optional,
                pld_attributes: nt.attrs.clone(),
            }
        })
        .collect();

    let type_params: Vec<(CoreType, Variance)> = named_types.iter()
        .filter(|nt| nt.label != "key")
        .map(|nt| {
            (
                CoreType {
                    ptyp_desc: CoreTypeDesc::Ptyp_var(safe_type_from_value(&nt.label)),
                    ptyp_loc: nt.loc.clone(),
                    ptyp_attributes: vec![],
                },
                Variance::Invariant,
            )
        })
        .collect();

    let type_decl = TypeDeclaration {
        ptype_name: Loc { txt: name.to_string(), loc: loc.clone() },
        ptype_params: type_params,
        ptype_cstrs: vec![],
        ptype_kind: TypeKind::Ptype_record(label_decls),
        ptype_private: PrivateFlag::Public,
        ptype_manifest: None,
        ptype_attributes: vec![
            (Loc { txt: "res.jsxComponentProps".to_string(), loc: empty_loc() }, Payload::PStr(vec![])),
        ],
        ptype_loc: loc.clone(),
    };

    StructureItem {
        pstr_desc: StructureItemDesc::Pstr_type(RecFlag::Nonrecursive, vec![type_decl]),
        // OCaml's Str.type_ without ~loc uses Location.none
        pstr_loc: empty_loc(),
    }
}

/// Create props record type declaration for signatures
fn make_props_record_type_sig(name: &str, loc: &Location, named_types: &[NamedType]) -> SignatureItem {
    // Check for duplicate labels (OCaml: check_duplicated_label)
    check_duplicated_labels(named_types);

    let label_decls: Vec<LabelDeclaration> = named_types.iter()
        .map(|nt| {
            // OCaml uses ghost location for the Ptyp_var inside label declarations
            let type_var = CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_var(safe_type_from_value(&nt.label)),
                ptyp_loc: empty_loc(),
                ptyp_attributes: vec![],
            };

            // Key prop is always optional in type declaration (OCaml: if label = "key" then ~optional:true)
            let is_optional = if nt.label == "key" { true } else { nt.is_optional };

            LabelDeclaration {
                pld_name: Loc { txt: nt.label.clone(), loc: nt.loc.clone() },
                pld_mutable: MutableFlag::Immutable,
                pld_type: type_var,
                pld_loc: nt.loc.clone(),
                pld_optional: is_optional,
                pld_attributes: nt.attrs.clone(),
            }
        })
        .collect();

    let type_params: Vec<(CoreType, Variance)> = named_types.iter()
        .filter(|nt| nt.label != "key")
        .map(|nt| {
            (
                CoreType {
                    ptyp_desc: CoreTypeDesc::Ptyp_var(safe_type_from_value(&nt.label)),
                    ptyp_loc: nt.loc.clone(),
                    ptyp_attributes: vec![],
                },
                Variance::Invariant,
            )
        })
        .collect();

    let type_decl = TypeDeclaration {
        ptype_name: Loc { txt: name.to_string(), loc: loc.clone() },
        ptype_params: type_params,
        ptype_cstrs: vec![],
        ptype_kind: TypeKind::Ptype_record(label_decls),
        ptype_private: PrivateFlag::Public,
        ptype_manifest: None,
        ptype_attributes: vec![
            (Loc { txt: "res.jsxComponentProps".to_string(), loc: empty_loc() }, Payload::PStr(vec![])),
        ],
        ptype_loc: loc.clone(),
    };

    SignatureItem {
        psig_desc: SignatureItemDesc::Psig_type(RecFlag::Nonrecursive, vec![type_decl]),
        // OCaml: Sig.type_ without ~loc uses Location.none
        psig_loc: empty_loc(),
    }
}

/// Create abstract props type with manifest (for sharedProps)
/// type props = sharedProps<T>
fn make_props_abstract_type(name: &str, loc: &Location, manifest: &CoreType, typ_vars: &[CoreType]) -> StructureItem {
    let type_params: Vec<(CoreType, Variance)> = typ_vars.iter()
        .map(|v| (v.clone(), Variance::Invariant))
        .collect();

    let type_decl = TypeDeclaration {
        ptype_name: Loc { txt: name.to_string(), loc: loc.clone() },
        ptype_params: type_params,
        ptype_cstrs: vec![],
        ptype_kind: TypeKind::Ptype_abstract,
        ptype_private: PrivateFlag::Public,
        ptype_manifest: Some(manifest.clone()),
        ptype_attributes: vec![],
        ptype_loc: loc.clone(),
    };

    StructureItem {
        pstr_desc: StructureItemDesc::Pstr_type(RecFlag::Nonrecursive, vec![type_decl]),
        // OCaml's Str.type_ without ~loc uses Location.none
        pstr_loc: empty_loc(),
    }
}

/// Create abstract props type with manifest for signatures (for sharedProps)
fn make_props_abstract_type_sig(name: &str, loc: &Location, manifest: &CoreType, typ_vars: &[CoreType]) -> SignatureItem {
    let type_params: Vec<(CoreType, Variance)> = typ_vars.iter()
        .map(|v| (v.clone(), Variance::Invariant))
        .collect();

    let type_decl = TypeDeclaration {
        ptype_name: Loc { txt: name.to_string(), loc: loc.clone() },
        ptype_params: type_params,
        ptype_cstrs: vec![],
        ptype_kind: TypeKind::Ptype_abstract,
        ptype_private: PrivateFlag::Public,
        ptype_manifest: Some(manifest.clone()),
        ptype_attributes: vec![],
        ptype_loc: loc.clone(),
    };

    SignatureItem {
        psig_desc: SignatureItemDesc::Psig_type(RecFlag::Nonrecursive, vec![type_decl]),
        // OCaml: Sig.type_ without ~loc uses Location.none
        psig_loc: empty_loc(),
    }
}

/// Create abstract props type with manifest and @live attribute (for external sharedProps)
fn make_props_abstract_type_with_live(name: &str, loc: &Location, manifest: &CoreType, typ_vars: &[CoreType]) -> StructureItem {
    let type_params: Vec<(CoreType, Variance)> = typ_vars.iter()
        .map(|v| (v.clone(), Variance::Invariant))
        .collect();

    let jsx_props_attr = (
        Loc { txt: "res.jsxComponentProps".to_string(), loc: empty_loc() },
        Payload::PStr(vec![]),
    );
    let live_attr = (
        Loc { txt: "live".to_string(), loc: empty_loc() },
        Payload::PStr(vec![]),
    );

    let type_decl = TypeDeclaration {
        ptype_name: Loc { txt: name.to_string(), loc: loc.clone() },
        ptype_params: type_params,
        ptype_cstrs: vec![],
        ptype_kind: TypeKind::Ptype_abstract,
        ptype_private: PrivateFlag::Public,
        ptype_manifest: Some(manifest.clone()),
        ptype_attributes: vec![jsx_props_attr, live_attr],
        ptype_loc: loc.clone(),
    };

    StructureItem {
        pstr_desc: StructureItemDesc::Pstr_type(RecFlag::Nonrecursive, vec![type_decl]),
        // OCaml's Str.type_ without ~loc uses Location.none
        pstr_loc: empty_loc(),
    }
}

/// Create abstract props type with manifest and @live for signatures
fn make_props_abstract_type_sig_with_live(name: &str, loc: &Location, manifest: &CoreType, typ_vars: &[CoreType]) -> SignatureItem {
    let type_params: Vec<(CoreType, Variance)> = typ_vars.iter()
        .map(|v| (v.clone(), Variance::Invariant))
        .collect();

    let jsx_props_attr = (
        Loc { txt: "res.jsxComponentProps".to_string(), loc: empty_loc() },
        Payload::PStr(vec![]),
    );
    let live_attr = (
        Loc { txt: "live".to_string(), loc: empty_loc() },
        Payload::PStr(vec![]),
    );

    let type_decl = TypeDeclaration {
        ptype_name: Loc { txt: name.to_string(), loc: loc.clone() },
        ptype_params: type_params,
        ptype_cstrs: vec![],
        ptype_kind: TypeKind::Ptype_abstract,
        ptype_private: PrivateFlag::Public,
        ptype_manifest: Some(manifest.clone()),
        ptype_attributes: vec![jsx_props_attr, live_attr],
        ptype_loc: loc.clone(),
    };

    SignatureItem {
        psig_desc: SignatureItemDesc::Psig_type(RecFlag::Nonrecursive, vec![type_decl]),
        // OCaml: Sig.type_ without ~loc uses Location.none
        psig_loc: empty_loc(),
    }
}

// ============================================================================
// Config Update
// ============================================================================

fn update_config_from_attribute(arena: &ParseArena, attr: &Attribute, config: &mut JsxConfig) {
    // Parse @@jsxConfig({version: 4, module_: "React"})
    // Matches OCaml's update_config behavior
    if let Payload::PStr(items) = &attr.1 {
        for item in items {
            if let StructureItemDesc::Pstr_eval(expr, _) = &item.pstr_desc {
                if let ExpressionDesc::Pexp_record(fields, _) = &expr.pexp_desc {
                    let mut module_raw: Option<String> = None;
                    let mut version_raw: Option<i32> = None;

                    for field in fields {
                        let last_idx = arena.get_longident(field.lid.txt).last_idx();
                        let last_str = arena.get_string(last_idx);
                        match last_str {
                            "version" => {
                                if let ExpressionDesc::Pexp_constant(Constant::Integer(s, _)) = &field.expr.pexp_desc {
                                    if let Ok(v) = s.parse::<i32>() {
                                        version_raw = Some(v);
                                    }
                                }
                            }
                            "module_" => {
                                if let ExpressionDesc::Pexp_constant(Constant::String(s, _)) = &field.expr.pexp_desc {
                                    module_raw = Some(s.clone());
                                }
                            }
                            _ => {}
                        }
                    }

                    // OCaml behavior: if module is not "react" (case-insensitive), it's "generic"
                    // and version is always set to 4. Otherwise use explicit version.
                    let is_generic = match &module_raw {
                        Some(m) if m.to_lowercase() == "react" => false,
                        Some(_) => true,
                        None => false,
                    };

                    match (is_generic, version_raw) {
                        (true, _) => config.version = 4,
                        (false, Some(v)) => config.version = v,
                        (false, None) => {}
                    }

                    if let Some(m) = module_raw {
                        config.module_name = m;
                    }
                }
            }
        }
    }
}

// ============================================================================
// Structure/Signature Item Desc Transformation (for non-component items)
// ============================================================================

fn transform_structure_item_desc(desc: StructureItemDesc, config: &JsxConfig, arena: &mut ParseArena) -> StructureItemDesc {
    match desc {
        StructureItemDesc::Pstr_value(rec_flag, bindings) => {
            StructureItemDesc::Pstr_value(
                rec_flag,
                bindings.into_iter().map(|b| transform_value_binding(b, config, arena)).collect(),
            )
        }
        StructureItemDesc::Pstr_eval(expr, attrs) => {
            StructureItemDesc::Pstr_eval(transform_expression(expr, config, arena), attrs)
        }
        StructureItemDesc::Pstr_module(mb) => {
            let mut config_clone = config.clone();
            StructureItemDesc::Pstr_module(transform_module_binding(mb, &mut config_clone, arena))
        }
        StructureItemDesc::Pstr_recmodule(mbs) => {
            let mut result = vec![];
            for mb in mbs {
                let mut config_clone = config.clone();
                result.push(transform_module_binding(mb, &mut config_clone, arena));
            }
            StructureItemDesc::Pstr_recmodule(result)
        }
        StructureItemDesc::Pstr_modtype(mt) => {
            let mut config_clone = config.clone();
            StructureItemDesc::Pstr_modtype(transform_module_type_declaration(mt, &mut config_clone, arena))
        }
        other => other,
    }
}

fn transform_module_type_declaration(mt: ModuleTypeDeclaration, config: &mut JsxConfig, arena: &mut ParseArena) -> ModuleTypeDeclaration {
    ModuleTypeDeclaration {
        pmtd_name: mt.pmtd_name,
        pmtd_type: mt.pmtd_type.map(|mty| transform_module_type(mty, config, arena)),
        pmtd_attributes: mt.pmtd_attributes,
        pmtd_loc: mt.pmtd_loc,
    }
}

fn transform_module_type(mty: ModuleType, config: &mut JsxConfig, arena: &mut ParseArena) -> ModuleType {
    ModuleType {
        pmty_desc: match mty.pmty_desc {
            ModuleTypeDesc::Pmty_signature(sig_items) => {
                // Save and reset has_component (OCaml: config.has_component <- false)
                let old_has_component = config.has_component;
                config.has_component = false;
                // Transform signature items
                let mut result = vec![];
                for item in sig_items {
                    let items = transform_signature_item_multi(item, config, arena);
                    result.extend(items);
                }
                // Restore has_component
                config.has_component = old_has_component;
                ModuleTypeDesc::Pmty_signature(result)
            }
            ModuleTypeDesc::Pmty_functor(name, param, body) => {
                ModuleTypeDesc::Pmty_functor(
                    name,
                    param.map(|p| Box::new(transform_module_type(*p, config, arena))),
                    Box::new(transform_module_type(*body, config, arena)),
                )
            }
            ModuleTypeDesc::Pmty_with(mty, constraints) => {
                ModuleTypeDesc::Pmty_with(Box::new(transform_module_type(*mty, config, arena)), constraints)
            }
            other => other,
        },
        pmty_loc: mty.pmty_loc,
        pmty_attributes: mty.pmty_attributes,
    }
}

fn transform_signature_item_desc(desc: SignatureItemDesc, config: &mut JsxConfig, arena: &mut ParseArena) -> SignatureItemDesc {
    match desc {
        SignatureItemDesc::Psig_module(md) => {
            // Transform module declaration - recurse into the module type
            SignatureItemDesc::Psig_module(ModuleDeclaration {
                pmd_name: md.pmd_name,
                pmd_type: transform_module_type(md.pmd_type, config, arena),
                pmd_attributes: md.pmd_attributes,
                pmd_loc: md.pmd_loc,
            })
        }
        SignatureItemDesc::Psig_recmodule(mds) => {
            // Transform recursive module declarations
            SignatureItemDesc::Psig_recmodule(
                mds.into_iter()
                    .map(|md| ModuleDeclaration {
                        pmd_name: md.pmd_name,
                        pmd_type: transform_module_type(md.pmd_type, config, arena),
                        pmd_attributes: md.pmd_attributes,
                        pmd_loc: md.pmd_loc,
                    })
                    .collect()
            )
        }
        SignatureItemDesc::Psig_modtype(mtd) => {
            // Transform module type declaration
            SignatureItemDesc::Psig_modtype(transform_module_type_declaration(mtd, config, arena))
        }
        other => other,
    }
}

fn transform_module_binding(mb: ModuleBinding, config: &mut JsxConfig, arena: &mut ParseArena) -> ModuleBinding {
    ModuleBinding {
        pmb_name: mb.pmb_name,
        pmb_expr: transform_module_expr(mb.pmb_expr, config, arena),
        pmb_attributes: mb.pmb_attributes,
        pmb_loc: mb.pmb_loc,
    }
}

/// Transform a structure with an existing config (preserves file_name and nested_modules)
fn transform_structure_with_config(structure: Structure, config: &mut JsxConfig, arena: &mut ParseArena) -> Structure {
    // Save and reset has_component (OCaml: config.has_component <- false)
    let old_has_component = config.has_component;
    config.has_component = false;

    let mut result = vec![];
    for item in structure {
        let items = transform_structure_item_multi(item, config, arena);
        result.extend(items);
    }

    // Restore has_component
    config.has_component = old_has_component;
    result
}

fn transform_module_expr(mexpr: ModuleExpr, config: &mut JsxConfig, arena: &mut ParseArena) -> ModuleExpr {
    ModuleExpr {
        pmod_desc: match mexpr.pmod_desc {
            ModuleExprDesc::Pmod_structure(items) => {
                // Transform structure with current config (preserving file_name and nested_modules)
                ModuleExprDesc::Pmod_structure(transform_structure_with_config(items, config, arena))
            }
            ModuleExprDesc::Pmod_functor(name, mtype, body) => {
                ModuleExprDesc::Pmod_functor(name, mtype, Box::new(transform_module_expr(*body, config, arena)))
            }
            ModuleExprDesc::Pmod_apply(m1, m2) => {
                ModuleExprDesc::Pmod_apply(
                    Box::new(transform_module_expr(*m1, config, arena)),
                    Box::new(transform_module_expr(*m2, config, arena)),
                )
            }
            ModuleExprDesc::Pmod_constraint(m, mt) => {
                ModuleExprDesc::Pmod_constraint(Box::new(transform_module_expr(*m, config, arena)), mt)
            }
            other => other,
        },
        pmod_loc: mexpr.pmod_loc,
        pmod_attributes: mexpr.pmod_attributes,
    }
}

fn transform_value_binding(binding: ValueBinding, config: &JsxConfig, arena: &mut ParseArena) -> ValueBinding {
    ValueBinding {
        pvb_pat: binding.pvb_pat,
        pvb_expr: transform_expression(binding.pvb_expr, config, arena),
        pvb_attributes: binding.pvb_attributes,
        pvb_loc: binding.pvb_loc,
    }
}

// ============================================================================
// Expression Transformation
// ============================================================================

fn transform_expression(expr: Expression, config: &JsxConfig, arena: &mut ParseArena) -> Expression {
    let new_desc = match expr.pexp_desc {
        ExpressionDesc::Pexp_jsx_element(jsx) => {
            transform_jsx_element(jsx, config, &expr.pexp_loc, &expr.pexp_attributes, arena)
        }
        ExpressionDesc::Pexp_let(rec_flag, bindings, body) => {
            ExpressionDesc::Pexp_let(
                rec_flag,
                bindings.into_iter().map(|b| transform_value_binding(b, config, arena)).collect(),
                Box::new(transform_expression(*body, config, arena)),
            )
        }
        ExpressionDesc::Pexp_fun { arg_label, default, lhs, rhs, is_async, arity } => {
            ExpressionDesc::Pexp_fun {
                arg_label,
                default: default.map(|d| Box::new(transform_expression(*d, config, arena))),
                lhs,
                rhs: Box::new(transform_expression(*rhs, config, arena)),
                is_async,
                arity,
            }
        }
        ExpressionDesc::Pexp_apply { funct, args, partial, transformed_jsx } => {
            ExpressionDesc::Pexp_apply {
                funct: Box::new(transform_expression(*funct, config, arena)),
                args: args.into_iter().map(|(lbl, e)| (lbl, transform_expression(e, config, arena))).collect(),
                partial,
                transformed_jsx,
            }
        }
        ExpressionDesc::Pexp_match(scrutinee, cases) => {
            ExpressionDesc::Pexp_match(
                Box::new(transform_expression(*scrutinee, config, arena)),
                cases.into_iter().map(|c| transform_case(c, config, arena)).collect(),
            )
        }
        ExpressionDesc::Pexp_try(body, cases) => {
            ExpressionDesc::Pexp_try(
                Box::new(transform_expression(*body, config, arena)),
                cases.into_iter().map(|c| transform_case(c, config, arena)).collect(),
            )
        }
        ExpressionDesc::Pexp_tuple(exprs) => {
            ExpressionDesc::Pexp_tuple(
                exprs.into_iter().map(|e| transform_expression(e, config, arena)).collect()
            )
        }
        ExpressionDesc::Pexp_construct(lid, arg) => {
            ExpressionDesc::Pexp_construct(
                lid,
                arg.map(|e| Box::new(transform_expression(*e, config, arena))),
            )
        }
        ExpressionDesc::Pexp_variant(label, arg) => {
            ExpressionDesc::Pexp_variant(
                label,
                arg.map(|e| Box::new(transform_expression(*e, config, arena))),
            )
        }
        ExpressionDesc::Pexp_record(fields, base) => {
            ExpressionDesc::Pexp_record(
                fields.into_iter().map(|f| ExpressionRecordField {
                    lid: f.lid,
                    expr: transform_expression(f.expr, config, arena),
                    opt: f.opt,
                }).collect(),
                base.map(|e| Box::new(transform_expression(*e, config, arena))),
            )
        }
        ExpressionDesc::Pexp_field(obj, field) => {
            ExpressionDesc::Pexp_field(Box::new(transform_expression(*obj, config, arena)), field)
        }
        ExpressionDesc::Pexp_setfield(obj, field, value) => {
            ExpressionDesc::Pexp_setfield(
                Box::new(transform_expression(*obj, config, arena)),
                field,
                Box::new(transform_expression(*value, config, arena)),
            )
        }
        ExpressionDesc::Pexp_array(elems) => {
            ExpressionDesc::Pexp_array(
                elems.into_iter().map(|e| transform_expression(e, config, arena)).collect()
            )
        }
        ExpressionDesc::Pexp_ifthenelse(cond, then_branch, else_branch) => {
            ExpressionDesc::Pexp_ifthenelse(
                Box::new(transform_expression(*cond, config, arena)),
                Box::new(transform_expression(*then_branch, config, arena)),
                else_branch.map(|e| Box::new(transform_expression(*e, config, arena))),
            )
        }
        ExpressionDesc::Pexp_sequence(e1, e2) => {
            ExpressionDesc::Pexp_sequence(
                Box::new(transform_expression(*e1, config, arena)),
                Box::new(transform_expression(*e2, config, arena)),
            )
        }
        ExpressionDesc::Pexp_while(cond, body) => {
            ExpressionDesc::Pexp_while(
                Box::new(transform_expression(*cond, config, arena)),
                Box::new(transform_expression(*body, config, arena)),
            )
        }
        ExpressionDesc::Pexp_for(pat, start, end_expr, dir, body) => {
            ExpressionDesc::Pexp_for(
                pat,
                Box::new(transform_expression(*start, config, arena)),
                Box::new(transform_expression(*end_expr, config, arena)),
                dir,
                Box::new(transform_expression(*body, config, arena)),
            )
        }
        ExpressionDesc::Pexp_constraint(e, t) => {
            ExpressionDesc::Pexp_constraint(Box::new(transform_expression(*e, config, arena)), t)
        }
        ExpressionDesc::Pexp_coerce(e, t1, t2) => {
            ExpressionDesc::Pexp_coerce(Box::new(transform_expression(*e, config, arena)), t1, t2)
        }
        ExpressionDesc::Pexp_letmodule(name, mexpr, body) => {
            let mut config_clone = config.clone();
            ExpressionDesc::Pexp_letmodule(
                name,
                transform_module_expr(mexpr, &mut config_clone, arena),
                Box::new(transform_expression(*body, config, arena)),
            )
        }
        ExpressionDesc::Pexp_letexception(ext, body) => {
            ExpressionDesc::Pexp_letexception(ext, Box::new(transform_expression(*body, config, arena)))
        }
        ExpressionDesc::Pexp_assert(e) => {
            ExpressionDesc::Pexp_assert(Box::new(transform_expression(*e, config, arena)))
        }
        ExpressionDesc::Pexp_newtype(name, body) => {
            ExpressionDesc::Pexp_newtype(name, Box::new(transform_expression(*body, config, arena)))
        }
        ExpressionDesc::Pexp_pack(mexpr) => {
            let mut config_clone = config.clone();
            ExpressionDesc::Pexp_pack(transform_module_expr(mexpr, &mut config_clone, arena))
        }
        ExpressionDesc::Pexp_open(flag, lid, body) => {
            ExpressionDesc::Pexp_open(flag, lid, Box::new(transform_expression(*body, config, arena)))
        }
        ExpressionDesc::Pexp_await(e) => {
            ExpressionDesc::Pexp_await(Box::new(transform_expression(*e, config, arena)))
        }
        other => other,
    };

    Expression {
        pexp_desc: new_desc,
        pexp_loc: expr.pexp_loc,
        pexp_attributes: expr.pexp_attributes,
    }
}

fn transform_case(case: Case, config: &JsxConfig, arena: &mut ParseArena) -> Case {
    Case {
        pc_bar: case.pc_bar,
        pc_lhs: case.pc_lhs,
        pc_guard: case.pc_guard.map(|e| transform_expression(e, config, arena)),
        pc_rhs: transform_expression(case.pc_rhs, config, arena),
    }
}

// ============================================================================
// JSX Element Transformation
// ============================================================================

fn transform_jsx_element(
    jsx: JsxElement,
    config: &JsxConfig,
    loc: &Location,
    attrs: &Attributes,
    arena: &mut ParseArena,
) -> ExpressionDesc {
    match jsx {
        JsxElement::Fragment(fragment) => {
            transform_jsx_fragment(fragment, config, loc, attrs, arena)
        }
        JsxElement::Unary(unary) => {
            transform_jsx_unary(unary, config, loc, attrs, arena)
        }
        JsxElement::Container(container) => {
            transform_jsx_container(container, config, loc, attrs, arena)
        }
    }
}

/// Main JSX transformation function - matches OCaml's mk_react_jsx
fn mk_react_jsx(
    config: &JsxConfig,
    loc: &Location,
    _attrs: &Attributes,
    component_description: ComponentDescription,
    element_tag: Expression,
    props: Vec<JsxProp>,
    children: Vec<Expression>,
    arena: &mut ParseArena,
) -> ExpressionDesc {
    let more_than_one_children = children.len() > 1;

    // Append children prop
    let props_with_children = append_children_prop(config, component_description, props, children.clone(), arena);

    // Create the props record (filtering out key)
    let props_record = mk_record_from_props(loc, &props_with_children, config, arena);

    // Determine jsx function and key handling
    let key_prop = try_find_key_prop(arena, &props_with_children);

    let (jsx_expr, key_and_unit) = {
        let jsx_part = match &key_prop {
            Some(_) => {
                if more_than_one_children { "jsxsKeyed" } else { "jsxKeyed" }
            }
            None => {
                if more_than_one_children { "jsxs" } else { "jsx" }
            }
        };

        let jsx_path = match component_description {
            ComponentDescription::FragmentComponent | ComponentDescription::UppercasedComponent => {
                module_access_name(arena, config, jsx_part)
            }
            ComponentDescription::LowercasedComponent => {
                let element_binding = if config.module_name.to_lowercase() == "react" {
                    let react_dom_idx = arena.intern_string("ReactDOM");
                    Longident::Lident(react_dom_idx)
                } else {
                    let cap_idx = arena.push_string(capitalize_first(&config.module_name));
                    let elements_idx = arena.intern_string("Elements");
                    Longident::Ldot(
                        Box::new(Longident::Lident(cap_idx)),
                        elements_idx,
                    )
                };
                let jsx_part_idx = arena.intern_string(jsx_part);
                arena.push_longident(Longident::Ldot(Box::new(element_binding), jsx_part_idx))
            }
        };

        let jsx_expr = Expression {
            pexp_desc: ExpressionDesc::Pexp_ident(Loc {
                txt: jsx_path,
                loc: empty_loc(),
            }),
            pexp_loc: empty_loc(),
            pexp_attributes: vec![],
        };

        let key_and_unit: Vec<(ArgLabel, Expression)> = match key_prop {
            Some((label, expr)) => {
                vec![
                    (label, expr),
                    (ArgLabel::Nolabel, unit_expr(arena, &empty_loc())),
                ]
            }
            None => vec![],
        };

        (jsx_expr, key_and_unit)
    };

    let mut args = vec![
        (ArgLabel::Nolabel, element_tag),
        (ArgLabel::Nolabel, props_record),
    ];
    args.extend(key_and_unit);

    // Note: jsx_expr uses empty_loc() for ghost locations - don't overwrite it
    // The attributes go on the outer Pexp_apply, not on the jsx function ident
    ExpressionDesc::Pexp_apply {
        funct: Box::new(jsx_expr),
        args,
        partial: false,
        transformed_jsx: true,
    }
}

/// Append children prop to props list - matches OCaml's append_children_prop
fn append_children_prop(
    config: &JsxConfig,
    component_description: ComponentDescription,
    mut props: Vec<JsxProp>,
    children: Vec<Expression>,
    arena: &mut ParseArena,
) -> Vec<JsxProp> {
    if children.is_empty() {
        return props;
    }

    if children.len() == 1 {
        let child = children.into_iter().next().unwrap();
        let transformed_child = transform_expression(child.clone(), config, arena);

        // For lowercase components, wrap in ReactDOM.someElement
        let expr = match component_description {
            ComponentDescription::FragmentComponent | ComponentDescription::UppercasedComponent => {
                transformed_child
            }
            ComponentDescription::LowercasedComponent => {
                let element_binding = if config.module_name.to_lowercase() == "react" {
                    let react_dom_idx = arena.intern_string("ReactDOM");
                    Longident::Lident(react_dom_idx)
                } else {
                    let cap_idx = arena.push_string(capitalize_first(&config.module_name));
                    let elements_idx = arena.intern_string("Elements");
                    Longident::Ldot(
                        Box::new(Longident::Lident(cap_idx)),
                        elements_idx,
                    )
                };
                let some_element_idx = arena.intern_string("someElement");
                let some_element_path = arena.push_longident(Longident::Ldot(Box::new(element_binding), some_element_idx));

                // OCaml: Exp.apply without ~loc uses Location.none (default_loc = ref Location.none)
                Expression {
                    pexp_desc: ExpressionDesc::Pexp_apply {
                        funct: Box::new(Expression {
                            pexp_desc: ExpressionDesc::Pexp_ident(Loc {
                                txt: some_element_path,
                                loc: empty_loc(),
                            }),
                            pexp_loc: empty_loc(),
                            pexp_attributes: vec![],
                        }),
                        args: vec![(ArgLabel::Nolabel, transformed_child)],
                        partial: false,
                        transformed_jsx: false,
                    },
                    pexp_loc: empty_loc(), // OCaml: Exp.apply without ~loc defaults to Location.none
                    pexp_attributes: vec![],
                }
            }
        };

        // Children is optional for lowercase components
        let is_optional = matches!(component_description, ComponentDescription::LowercasedComponent);

        props.push(JsxProp::Value {
            name: Loc { txt: "children".to_string(), loc: child.pexp_loc },
            optional: is_optional,
            value: expr,
        });
    } else {
        // Multiple children: wrap in Module.array([...])
        // OCaml uses real location spanning all children for outer expressions but ghost for inner synthesized ones
        let loc = if let (Some(first), Some(last)) = (children.first(), children.last()) {
            // Create composite location spanning from first child start to last child end
            let start_idx = arena.loc_start_idx(first.pexp_loc);
            let end_idx = arena.loc_end_idx(last.pexp_loc);
            arena.mk_loc(start_idx, end_idx)
        } else {
            empty_loc()
        };

        // React.array function - uses ghost location for ident, real loc for outer expr
        let array_fn = Expression {
            pexp_desc: ExpressionDesc::Pexp_ident(Loc {
                txt: module_access_name(arena, config, "array"),
                loc: loc.clone(), // OCaml uses the children loc for the longident
            }),
            pexp_loc: empty_loc(), // Ghost location for the ident expression itself
            pexp_attributes: vec![],
        };

        // Array literal containing children - ghost location
        let children_array = Expression {
            pexp_desc: ExpressionDesc::Pexp_array(
                children.into_iter().map(|c| transform_expression(c, config, arena)).collect()
            ),
            pexp_loc: empty_loc(), // Ghost location for the array
            pexp_attributes: vec![],
        };

        let children_expr = Expression {
            pexp_desc: ExpressionDesc::Pexp_apply {
                funct: Box::new(array_fn),
                args: vec![(ArgLabel::Nolabel, children_array)],
                partial: false,
                transformed_jsx: false,
            },
            pexp_loc: loc.clone(),
            pexp_attributes: vec![],
        };

        props.push(JsxProp::Value {
            name: Loc { txt: "children".to_string(), loc },
            optional: false,
            value: children_expr,
        });
    }

    props
}

/// Get location from a JSX prop
fn loc_from_prop(prop: &JsxProp, arena: &mut ParseArena) -> Location {
    match prop {
        JsxProp::Punning { name, .. } => name.loc,
        JsxProp::Value { name, value, .. } => {
            // Location spans from name start to value end
            let start_idx = arena.loc_start_idx(name.loc);
            let end_idx = arena.loc_end_idx(value.pexp_loc);
            arena.mk_loc(start_idx, end_idx)
        }
        JsxProp::Spreading { loc, .. } => *loc,
    }
}

/// Create props record from JSX props - matches OCaml's mk_record_from_props
fn mk_record_from_props(
    jsx_expr_loc: &Location,
    props: &[JsxProp],
    config: &JsxConfig,
    arena: &mut ParseArena,
) -> Expression {
    // Calculate location from ALL props (including key) - OCaml does this before filtering
    let loc = if props.is_empty() {
        *jsx_expr_loc
    } else {
        let first = loc_from_prop(&props[0], arena);
        let last = loc_from_prop(props.last().unwrap(), arena);
        // Create composite location from first to last
        let start_idx = arena.loc_start_idx(first);
        let end_idx = arena.loc_end_idx(last);
        arena.mk_loc(start_idx, end_idx)
    };

    // Filter out key prop AFTER calculating location
    let props: Vec<&JsxProp> = props.iter()
        .filter(|p| match p {
            JsxProp::Punning { name, .. } | JsxProp::Value { name, .. } => name.txt != "key",
            JsxProp::Spreading { .. } => true,
        })
        .collect();

    // Handle spread props - must be first if present
    let (regular_props, spread_base) = {
        let mut props_iter = props.into_iter().peekable();
        let spread_base = if let Some(JsxProp::Spreading { expr, .. }) = props_iter.peek() {
            let spread = transform_expression(expr.clone(), config, arena);
            props_iter.next(); // consume the spread
            Some(Box::new(spread))
        } else {
            None
        };

        let regular: Vec<_> = props_iter.collect();
        (regular, spread_base)
    };

    // Build record fields
    let record_fields: Vec<ExpressionRecordField> = regular_props.into_iter()
        .filter_map(|prop| {
            match prop {
                JsxProp::Punning { optional, name } => {
                    let name_str_idx = arena.push_string(name.txt.clone());
                    let name_str_idx2 = arena.push_string(name.txt.clone());
                    Some(ExpressionRecordField {
                        lid: Loc {
                            txt: arena.push_longident(Longident::Lident(name_str_idx)),
                            loc: name.loc.clone(),
                        },
                        expr: Expression {
                            pexp_desc: ExpressionDesc::Pexp_ident(Loc {
                                txt: arena.push_longident(Longident::Lident(name_str_idx2)),
                                loc: name.loc.clone(),
                            }),
                            pexp_loc: name.loc.clone(),
                            pexp_attributes: vec![],
                        },
                        opt: *optional,
                    })
                }
                JsxProp::Value { name, optional, value } => {
                    let name_str_idx = arena.push_string(name.txt.clone());
                    Some(ExpressionRecordField {
                        lid: Loc {
                            txt: arena.push_longident(Longident::Lident(name_str_idx)),
                            loc: name.loc.clone(),
                        },
                        expr: transform_expression(value.clone(), config, arena),
                        opt: *optional,
                    })
                }
                JsxProp::Spreading { loc, .. } => {
                    // Spread after first position is an error
                    // For now, just skip (OCaml raises an error here)
                    eprintln!("JSX: use {{...p}} {{x: v}} not {{x: v}} {{...p}}. Multiple spreads not allowed at {:?}", loc);
                    None
                }
            }
        })
        .collect();

    // Handle empty record with spread
    match (&record_fields[..], &spread_base) {
        ([], Some(spread)) => {
            Expression {
                pexp_desc: spread.pexp_desc.clone(),
                pexp_loc: loc,
                pexp_attributes: vec![],
            }
        }
        _ => {
            Expression {
                pexp_desc: ExpressionDesc::Pexp_record(record_fields, spread_base),
                pexp_loc: loc,
                pexp_attributes: vec![],
            }
        }
    }
}

fn transform_jsx_fragment(
    fragment: JsxFragment,
    config: &JsxConfig,
    loc: &Location,
    attrs: &Attributes,
    arena: &mut ParseArena,
) -> ExpressionDesc {
    let children = fragment.children;

    // Fragment expression: React.jsxFragment (or Module.jsxFragment)
    let fragment_expr = Expression {
        pexp_desc: ExpressionDesc::Pexp_ident(Loc {
            txt: module_access_name(arena, config, "jsxFragment"),
            loc: loc.clone(),
        }),
        pexp_loc: loc.clone(),
        pexp_attributes: vec![],
    };

    // Use mk_react_jsx to handle the full transformation
    mk_react_jsx(
        config,
        loc,
        attrs,
        ComponentDescription::FragmentComponent,
        fragment_expr,
        vec![], // No props for fragments
        children,
        arena,
    )
}

fn transform_jsx_unary(
    unary: JsxUnaryElement,
    config: &JsxConfig,
    loc: &Location,
    attrs: &Attributes,
    arena: &mut ParseArena,
) -> ExpressionDesc {
    let tag_loc = &unary.tag_name.loc;
    match &unary.tag_name.txt {
        JsxTagName::Lower(name) => {
            // Lowercase element like <div />
            let component_name_expr = Expression {
                pexp_desc: ExpressionDesc::Pexp_constant(Constant::String(name.clone(), None)),
                pexp_loc: tag_loc.clone(),
                pexp_attributes: vec![],
            };
            mk_react_jsx(
                config,
                loc,
                attrs,
                ComponentDescription::LowercasedComponent,
                component_name_expr,
                unary.props,
                vec![],
                arena,
            )
        }
        JsxTagName::Upper(path) => {
            // Uppercase element like <MyModule /> -> MyModule.make
            let make_str_idx = arena.intern_string("make");
            let make_id = Expression {
                pexp_desc: ExpressionDesc::Pexp_ident(Loc {
                    txt: arena.push_longident(Longident::Ldot(Box::new(arena.get_longident(*path).clone()), make_str_idx)),
                    loc: tag_loc.clone(),
                }),
                pexp_loc: tag_loc.clone(),
                pexp_attributes: vec![],
            };
            mk_react_jsx(
                config,
                loc,
                attrs,
                ComponentDescription::UppercasedComponent,
                make_id,
                unary.props,
                vec![],
                arena,
            )
        }
        JsxTagName::QualifiedLower { path, name } => {
            // Qualified lowercase like <MyModule.lowercase /> -> MyModule.lowercase
            let name_str_idx = arena.push_string(name.clone());
            let make_id = Expression {
                pexp_desc: ExpressionDesc::Pexp_ident(Loc {
                    txt: arena.push_longident(Longident::Ldot(Box::new(arena.get_longident(*path).clone()), name_str_idx)),
                    loc: tag_loc.clone(),
                }),
                pexp_loc: tag_loc.clone(),
                pexp_attributes: vec![],
            };
            mk_react_jsx(
                config,
                loc,
                attrs,
                ComponentDescription::UppercasedComponent,
                make_id,
                unary.props,
                vec![],
                arena,
            )
        }
        JsxTagName::Invalid(name) => {
            eprintln!("JSX: element name is neither upper- or lowercase, got \"{}\"", name);
            ExpressionDesc::Pexp_jsx_element(JsxElement::Unary(unary))
        }
    }
}

fn transform_jsx_container(
    container: JsxContainerElement,
    config: &JsxConfig,
    loc: &Location,
    attrs: &Attributes,
    arena: &mut ParseArena,
) -> ExpressionDesc {
    let tag_loc = &container.tag_name_start.loc;
    match &container.tag_name_start.txt {
        JsxTagName::Lower(name) => {
            // Lowercase element like <div>...</div>
            let component_name_expr = Expression {
                pexp_desc: ExpressionDesc::Pexp_constant(Constant::String(name.clone(), None)),
                pexp_loc: tag_loc.clone(),
                pexp_attributes: vec![],
            };
            mk_react_jsx(
                config,
                loc,
                attrs,
                ComponentDescription::LowercasedComponent,
                component_name_expr,
                container.props,
                container.children,
                arena,
            )
        }
        JsxTagName::Upper(path) => {
            // Uppercase element like <MyModule>...</MyModule> -> MyModule.make
            let make_str_idx = arena.intern_string("make");
            let make_id = Expression {
                pexp_desc: ExpressionDesc::Pexp_ident(Loc {
                    txt: arena.push_longident(Longident::Ldot(Box::new(arena.get_longident(*path).clone()), make_str_idx)),
                    loc: tag_loc.clone(),
                }),
                pexp_loc: tag_loc.clone(),
                pexp_attributes: vec![],
            };
            mk_react_jsx(
                config,
                loc,
                attrs,
                ComponentDescription::UppercasedComponent,
                make_id,
                container.props,
                container.children,
                arena,
            )
        }
        JsxTagName::QualifiedLower { path, name } => {
            // Qualified lowercase like <MyModule.lowercase>...</MyModule.lowercase>
            let name_str_idx = arena.push_string(name.clone());
            let make_id = Expression {
                pexp_desc: ExpressionDesc::Pexp_ident(Loc {
                    txt: arena.push_longident(Longident::Ldot(Box::new(arena.get_longident(*path).clone()), name_str_idx)),
                    loc: tag_loc.clone(),
                }),
                pexp_loc: tag_loc.clone(),
                pexp_attributes: vec![],
            };
            mk_react_jsx(
                config,
                loc,
                attrs,
                ComponentDescription::UppercasedComponent,
                make_id,
                container.props,
                container.children,
                arena,
            )
        }
        JsxTagName::Invalid(name) => {
            eprintln!("JSX: element name is neither upper- or lowercase, got \"{}\"", name);
            ExpressionDesc::Pexp_jsx_element(JsxElement::Container(container))
        }
    }
}

