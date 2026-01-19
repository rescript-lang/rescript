//! Lambda -> JS IR compilation.
//!
//! This module provides a straightforward lowering from Lambda IR into the
//! JavaScript IR used by the Rust compiler backend. The implementation favors
//! clarity over aggressive optimization; it is intended as a correct baseline
//! that can be extended with more advanced codegen passes.

use crate::context::IdGenerator;
use crate::ident::Ident;
use crate::js_ir::{
    BinOp, Block, CallInfo, Delim, Expression, ExpressionDesc, ForDirection, ModuleId, Program,
    Statement, StatementDesc, VariableDeclaration,
};
use crate::lambda::compat::{Comparison, FieldDbgInfo, LetKind};
use crate::lambda::constant::{Constant, StringDelim};
use crate::lambda::primitive::{Mutable, Primitive};
use crate::lambda::{Apply, Lambda, LambdaSwitch};
use crate::js_ir::op::{IdentInfo, Kind, LengthObject, MutableFlag, Number};

/// Compilation result for an expression: prelude statements plus a value expression.
#[derive(Debug, Clone)]
struct Compiled {
    prelude: Block,
    expr: Expression,
}

/// Lambda compiler to JS IR.
pub struct LambdaCompiler {
    id_gen: IdGenerator,
}

impl Default for LambdaCompiler {
    fn default() -> Self {
        Self::new()
    }
}

impl LambdaCompiler {
    /// Create a new compiler.
    pub fn new() -> Self {
        Self {
            id_gen: IdGenerator::new(),
        }
    }

    /// Compile a Lambda expression into a JS IR program.
    pub fn compile_program(&mut self, lam: &Lambda) -> Program {
        let compiled = self.compile_expr(lam);
        let mut block = compiled.prelude;
        block.push(Statement::exp(compiled.expr));
        Program::new(block, vec![])
    }

    fn compile_expr(&mut self, lam: &Lambda) -> Compiled {
        match lam {
            Lambda::Lvar(id) => Compiled {
                prelude: vec![],
                expr: Expression::var(id.clone()),
            },
            Lambda::LglobalModule(id, dynamic_import) => {
                let module = ModuleId {
                    id: id.clone(),
                    kind: Kind::Ml,
                    dynamic_import: *dynamic_import,
                };
                Compiled {
                    prelude: vec![],
                    expr: Expression::qualified(module, None),
                }
            }
            Lambda::Lconst(cst) => Compiled {
                prelude: vec![],
                expr: Self::compile_constant(cst),
            },
            Lambda::Lapply(Apply {
                ap_func,
                ap_args,
                ap_info: _,
                ap_transformed_jsx: _,
            }) => {
                let mut prelude = vec![];
                let func = self.compile_expr(ap_func);
                prelude.extend(func.prelude);
                let mut args = Vec::new();
                for arg in ap_args {
                    let compiled = self.compile_expr(arg);
                    prelude.extend(compiled.prelude);
                    args.push(compiled.expr);
                }
                let call = Expression::call(func.expr, args, CallInfo::default());
                if prelude.is_empty() {
                    Compiled {
                        prelude,
                        expr: call,
                    }
                } else {
                    let tmp = self.fresh_ident("call");
                    prelude.push(Statement::variable(VariableDeclaration {
                        ident: tmp.clone(),
                        value: Some(call),
                        property: LetKind::Strict,
                        ident_info: IdentInfo::default(),
                    }));
                    Compiled {
                        prelude,
                        expr: Expression::var(tmp),
                    }
                }
            }
            Lambda::Lfunction(func) => {
                let params = func.params.clone();
                let body_compiled = self.compile_expr(&func.body);
                let mut body_block = body_compiled.prelude;
                body_block.push(Statement::return_(body_compiled.expr));
                let expr = Expression::new(ExpressionDesc::Fun {
                    is_method: false,
                    params,
                    body: body_block,
                    env: Default::default(),
                    return_unit: func.attr.return_unit,
                    async_: func.attr.async_,
                    directive: func.attr.directive.clone(),
                });
                Compiled {
                    prelude: vec![],
                    expr,
                }
            }
            Lambda::Llet(kind, id, value, body) => {
                let mut prelude = vec![];
                let value_compiled = self.compile_expr(value);
                prelude.extend(value_compiled.prelude);
                prelude.push(Statement::variable(VariableDeclaration {
                    ident: id.clone(),
                    value: Some(value_compiled.expr),
                    property: *kind,
                    ident_info: IdentInfo::default(),
                }));
                let body_compiled = self.compile_expr(body);
                prelude.extend(body_compiled.prelude);
                Compiled {
                    prelude,
                    expr: body_compiled.expr,
                }
            }
            Lambda::Lletrec(bindings, body) => {
                let mut prelude = vec![];
                for (id, _) in bindings {
                    prelude.push(Statement::variable(VariableDeclaration {
                        ident: id.clone(),
                        value: None,
                        property: LetKind::Strict,
                        ident_info: IdentInfo::default(),
                    }));
                }
                for (id, expr) in bindings {
                    let compiled = self.compile_expr(expr);
                    prelude.extend(compiled.prelude);
                    let assign = Expression::bin(
                        BinOp::Eq,
                        Expression::var(id.clone()),
                        compiled.expr,
                    );
                    prelude.push(Statement::exp(assign));
                }
                let body_compiled = self.compile_expr(body);
                prelude.extend(body_compiled.prelude);
                Compiled {
                    prelude,
                    expr: body_compiled.expr,
                }
            }
            Lambda::Lprim(info) => self.compile_primitive(&info.primitive, &info.args),
            Lambda::Lswitch(scrut, switch) => self.compile_switch(scrut, switch),
            Lambda::LstringSwitch(scrut, cases, default) => {
                self.compile_string_switch(scrut, cases, default.as_deref())
            }
            Lambda::LstaticRaise(_id, args) => {
                let expr = if let Some(arg) = args.first() {
                    self.compile_expr(arg).expr
                } else {
                    Expression::undefined(false)
                };
                Compiled {
                    prelude: vec![Statement::throw(expr)],
                    expr: Expression::undefined(false),
                }
            }
            Lambda::LstaticCatch(body, _handler, _handler_body) => self.compile_expr(body),
            Lambda::LtryWith(body, exn, handler) => {
                let body_block = self.compile_block(body);
                let handler_block = self.compile_block(handler);
                let try_stmt = Statement::new(StatementDesc::Try(
                    body_block,
                    Some((exn.clone(), handler_block)),
                    None,
                ));
                Compiled {
                    prelude: vec![try_stmt],
                    expr: Expression::undefined(false),
                }
            }
            Lambda::LifThenElse(cond, then_, else_) => {
                let cond_compiled = self.compile_expr(cond);
                let then_compiled = self.compile_expr(then_);
                let else_compiled = self.compile_expr(else_);
                let mut prelude = cond_compiled.prelude;
                if then_compiled.prelude.is_empty() && else_compiled.prelude.is_empty() {
                    Compiled {
                        prelude,
                        expr: Expression::cond(
                            cond_compiled.expr,
                            then_compiled.expr,
                            else_compiled.expr,
                        ),
                    }
                } else {
                    let tmp = self.fresh_ident("ifres");
                    prelude.push(Statement::variable(VariableDeclaration {
                        ident: tmp.clone(),
                        value: None,
                        property: LetKind::Strict,
                        ident_info: IdentInfo::default(),
                    }));
                    let mut then_block = then_compiled.prelude;
                    then_block.push(Statement::exp(Expression::bin(
                        BinOp::Eq,
                        Expression::var(tmp.clone()),
                        then_compiled.expr,
                    )));
                    let mut else_block = else_compiled.prelude;
                    else_block.push(Statement::exp(Expression::bin(
                        BinOp::Eq,
                        Expression::var(tmp.clone()),
                        else_compiled.expr,
                    )));
                    prelude.push(Statement::if_(cond_compiled.expr, then_block, else_block));
                    Compiled {
                        prelude,
                        expr: Expression::var(tmp),
                    }
                }
            }
            Lambda::Lsequence(a, b) => {
                let mut prelude = self.compile_block(a);
                let compiled = self.compile_expr(b);
                prelude.extend(compiled.prelude);
                Compiled {
                    prelude,
                    expr: compiled.expr,
                }
            }
            Lambda::Lwhile(cond, body) => {
                let cond_expr = self.compile_expr(cond).expr;
                let body_block = self.compile_block(body);
                Compiled {
                    prelude: vec![Statement::while_(cond_expr, body_block)],
                    expr: Expression::undefined(false),
                }
            }
            Lambda::Lfor(var, start, end, dir, body) => {
                let start_expr = self.compile_expr(start).expr;
                let end_expr = self.compile_expr(end).expr;
                let dir = match dir {
                    crate::lambda::DirectionFlag::Upto => ForDirection::Upto,
                    crate::lambda::DirectionFlag::Downto => ForDirection::Downto,
                };
                let body_block = self.compile_block(body);
                let stmt = Statement::new(StatementDesc::ForRange(
                    Some(start_expr),
                    end_expr,
                    var.clone(),
                    dir,
                    body_block,
                ));
                Compiled {
                    prelude: vec![stmt],
                    expr: Expression::undefined(false),
                }
            }
            Lambda::Lassign(id, value) => {
                let compiled = self.compile_expr(value);
                let prelude = compiled.prelude;
                let assign = Expression::bin(BinOp::Eq, Expression::var(id.clone()), compiled.expr);
                Compiled {
                    prelude,
                    expr: assign,
                }
            }
        }
    }

    fn compile_block(&mut self, lam: &Lambda) -> Block {
        let compiled = self.compile_expr(lam);
        let mut block = compiled.prelude;
        block.push(Statement::exp(compiled.expr));
        block
    }

    fn compile_switch(&mut self, scrut: &Lambda, switch: &LambdaSwitch) -> Compiled {
        let scrut_compiled = self.compile_expr(scrut);
        let mut prelude = scrut_compiled.prelude;
        let tmp = self.fresh_ident("switch");
        prelude.push(Statement::variable(VariableDeclaration {
            ident: tmp.clone(),
            value: None,
            property: LetKind::Strict,
            ident_info: IdentInfo::default(),
        }));

        let mut cases = Vec::new();
        for (tag, body) in &switch.sw_consts {
            let mut body_block = self.compile_block(body);
            body_block.push(Statement::break_());
            cases.push((
                *tag,
                crate::js_ir::CaseClause {
                    body: body_block,
                    should_break: false,
                    comment: None,
                },
            ));
        }

        let default_block = switch.sw_failaction.as_ref().map(|fallback| {
            let mut block = self.compile_block(fallback);
            block.push(Statement::break_());
            block
        });

        prelude.push(Statement::new(StatementDesc::IntSwitch(
            scrut_compiled.expr,
            cases,
            default_block,
        )));

        Compiled {
            prelude,
            expr: Expression::var(tmp),
        }
    }

    fn compile_string_switch(
        &mut self,
        scrut: &Lambda,
        cases: &[(String, Lambda)],
        default: Option<&Lambda>,
    ) -> Compiled {
        let scrut_compiled = self.compile_expr(scrut);
        let mut prelude = scrut_compiled.prelude;
        let tmp = self.fresh_ident("switch");
        prelude.push(Statement::variable(VariableDeclaration {
            ident: tmp.clone(),
            value: None,
            property: LetKind::Strict,
            ident_info: IdentInfo::default(),
        }));

        let mut clauses = Vec::new();
        for (tag, body) in cases {
            let mut body_block = self.compile_block(body);
            body_block.push(Statement::break_());
            clauses.push(crate::js_ir::StringClause {
                tag_type: tag.clone(),
                clause: crate::js_ir::CaseClause {
                    body: body_block,
                    should_break: false,
                    comment: None,
                },
            });
        }

        let default_block = default.map(|fallback| {
            let mut block = self.compile_block(fallback);
            block.push(Statement::break_());
            block
        });

        prelude.push(Statement::new(StatementDesc::StringSwitch(
            scrut_compiled.expr,
            clauses,
            default_block,
        )));

        Compiled {
            prelude,
            expr: Expression::var(tmp),
        }
    }

    fn compile_primitive(&mut self, prim: &Primitive, args: &[Lambda]) -> Compiled {
        use Primitive::*;
        let mut prelude = vec![];
        let mut compiled_args = Vec::new();
        for arg in args {
            let compiled = self.compile_expr(arg);
            prelude.extend(compiled.prelude);
            compiled_args.push(compiled.expr);
        }

        let expr = match prim {
            Pstringlength => Expression::new(ExpressionDesc::Length(
                Box::new(compiled_args[0].clone()),
                LengthObject::String,
            )),
            Parraylength => Expression::new(ExpressionDesc::Length(
                Box::new(compiled_args[0].clone()),
                LengthObject::Array,
            )),
            Paddint | Paddfloat => Expression::bin(BinOp::Plus, compiled_args[0].clone(), compiled_args[1].clone()),
            Psubint | Psubfloat => Expression::bin(BinOp::Minus, compiled_args[0].clone(), compiled_args[1].clone()),
            Pmulint | Pmulfloat => Expression::bin(BinOp::Mul, compiled_args[0].clone(), compiled_args[1].clone()),
            Pdivint | Pdivfloat => Expression::bin(BinOp::Div, compiled_args[0].clone(), compiled_args[1].clone()),
            Pmodint | Pmodfloat => Expression::bin(BinOp::Mod, compiled_args[0].clone(), compiled_args[1].clone()),
            Pintcomp(cmp) | Pfloatcomp(cmp) | Pbigintcomp(cmp) | Pstringcomp(cmp) | Pjscomp(cmp) => {
                let op = comparison_to_binop(*cmp);
                Expression::bin(op, compiled_args[0].clone(), compiled_args[1].clone())
            }
            Psequand => Expression::bin(BinOp::And, compiled_args[0].clone(), compiled_args[1].clone()),
            Psequor => Expression::bin(BinOp::Or, compiled_args[0].clone(), compiled_args[1].clone()),
            Pnot => Expression::new(ExpressionDesc::JsNot(Box::new(compiled_args[0].clone()))),
            Pnegint | Pnegfloat => Expression::bin(
                BinOp::Minus,
                Expression::number(Number::int(0)),
                compiled_args[0].clone(),
            ),
            Pmakearray => Expression::array(compiled_args, MutableFlag::Mutable),
            Pstringadd => Expression::new(ExpressionDesc::StringAppend(
                Box::new(compiled_args[0].clone()),
                Box::new(compiled_args[1].clone()),
            )),
            Pfield(idx, info) => self.compile_field_access(*idx, info, &compiled_args[0]),
            Psetfield(idx, info) => {
                let target = self.compile_set_field(*idx, info, &compiled_args[0], &compiled_args[1]);
                Expression::new(ExpressionDesc::Seq(Box::new(target), Box::new(Expression::undefined(false))))
            }
            Pmakeblock(tag, tag_info, mutable_flag) => {
                let js_mutable = match mutable_flag {
                    Mutable::Mutable => MutableFlag::Mutable,
                    Mutable::Immutable => MutableFlag::Immutable,
                };
                Expression::new(ExpressionDesc::CamlBlock(
                    compiled_args,
                    js_mutable,
                    Box::new(Expression::number(Number::int(*tag))),
                    tag_info.clone(),
                ))
            }
            PrawJsCode(info) => Expression::new(ExpressionDesc::RawJsCode(crate::js_ir::JsRawInfo {
                code: info.code.clone(),
                is_stmt: info.is_stmt,
            })),
            Pdebugger => {
                prelude.push(Statement::debugger());
                Expression::undefined(false)
            }
            Praise => {
                let expr = compiled_args
                    .first()
                    .cloned()
                    .unwrap_or_else(|| Expression::undefined(false));
                prelude.push(Statement::throw(expr));
                Expression::undefined(false)
            }
            Ptypeof => Expression::new(ExpressionDesc::Typeof(Box::new(compiled_args[0].clone()))),
            Pawait => Expression::new(ExpressionDesc::Await(Box::new(compiled_args[0].clone()))),
            Pimport => Expression::call(
                Expression::var(Ident::create_persistent("import")),
                compiled_args,
                CallInfo::default(),
            ),
            PjsCall { prim_name, .. } => {
                Expression::call(Expression::var(Ident::create_persistent(prim_name)), compiled_args, CallInfo::default())
            }
            _ => Expression::undefined(false),
        };

        Compiled { prelude, expr }
    }

    fn compile_constant(cst: &Constant) -> Expression {
        match cst {
            Constant::JsNull => Expression::null(),
            Constant::JsUndefined { is_unit } => Expression::undefined(*is_unit),
            Constant::JsTrue => Expression::bool(true),
            Constant::JsFalse => Expression::bool(false),
            Constant::Int { i, comment } => {
                let _ = comment;
                Expression::number(Number::int(*i))
            }
            Constant::Char(c) => Expression::number(Number::Int { i: *c, c: Some(*c) }),
            Constant::String { s, delim } => {
                let delim = match delim {
                    Some(StringDelim::Backtick) => Delim::BackQuotes,
                    Some(StringDelim::NoQuotes) | None => Delim::None,
                };
                Expression::new(ExpressionDesc::Str {
                    delim,
                    txt: s.clone(),
                })
            }
            Constant::Float(f) => Expression::number(Number::float(f.clone())),
            Constant::BigInt { negative, value } => {
                Expression::number(Number::bigint(!negative, value.clone()))
            }
            Constant::Pointer(name) => Expression::string(name.clone()),
            Constant::Block {
                tag,
                tag_info,
                elements,
            } => {
                let elems = elements.iter().map(Self::compile_constant).collect();
                Expression::new(ExpressionDesc::CamlBlock(
                    elems,
                    MutableFlag::Immutable,
                    Box::new(Expression::number(Number::int(*tag))),
                    tag_info.clone(),
                ))
            }
            Constant::Some(inner) => {
                let inner = Self::compile_constant(inner);
                Expression::new(ExpressionDesc::OptionalBlock(Box::new(inner), true))
            }
            Constant::ModuleAlias => Expression::undefined(false),
        }
    }

    fn compile_field_access(&self, idx: i32, info: &FieldDbgInfo, target: &Expression) -> Expression {
        match info {
            FieldDbgInfo::Module { name }
            | FieldDbgInfo::Record { name, .. }
            | FieldDbgInfo::RecordInline { name }
            | FieldDbgInfo::RecordExtension { name } => Expression::new(ExpressionDesc::StaticIndex(
                Box::new(target.clone()),
                name.clone(),
                None,
            )),
            _ => Expression::new(ExpressionDesc::ArrayIndex(
                Box::new(target.clone()),
                Box::new(Expression::number(Number::int(idx))),
            )),
        }
    }

    fn compile_set_field(
        &self,
        _idx: i32,
        info: &crate::lambda::compat::SetFieldDbgInfo,
        target: &Expression,
        value: &Expression,
    ) -> Expression {
        let field = match info {
            crate::lambda::compat::SetFieldDbgInfo::RecordSet(name)
            | crate::lambda::compat::SetFieldDbgInfo::RecordInlineSet(name)
            | crate::lambda::compat::SetFieldDbgInfo::RecordExtensionSet(name) => {
                Expression::new(ExpressionDesc::StaticIndex(
                    Box::new(target.clone()),
                    name.clone(),
                    None,
                ))
            }
        };
        Expression::bin(BinOp::Eq, field, value.clone())
    }

    fn fresh_ident(&self, prefix: &str) -> Ident {
        self.id_gen.create(prefix)
    }
}

fn comparison_to_binop(cmp: Comparison) -> BinOp {
    match cmp {
        Comparison::Eq => BinOp::EqEqEq,
        Comparison::Neq => BinOp::NotEqEq,
        Comparison::Lt => BinOp::Lt,
        Comparison::Le => BinOp::Le,
        Comparison::Gt => BinOp::Gt,
        Comparison::Ge => BinOp::Ge,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lambda::constant::Constant;
    use crate::lambda::Lambda;

    #[test]
    fn test_compile_constant_int() {
        let mut compiler = LambdaCompiler::new();
        let lam = Lambda::const_(Constant::int(7));
        let program = compiler.compile_program(&lam);
        assert!(!program.block.is_empty());
    }
}
