module Comment = Res_comment
module Doc = Res_doc

type t = {
  leading: Hashtbl.t<Location.t, list<Comment.t>>,
  inside: Hashtbl.t<Location.t, list<Comment.t>>,
  trailing: Hashtbl.t<Location.t, list<Comment.t>>,
}

let make = () => {
  leading: Hashtbl.create(100),
  inside: Hashtbl.create(100),
  trailing: Hashtbl.create(100),
}

let copy = tbl => {
  leading: Hashtbl.copy(tbl.leading),
  inside: Hashtbl.copy(tbl.inside),
  trailing: Hashtbl.copy(tbl.trailing),
}

let empty = make()

@live
let log = t => {
  open Location
  let leadingStuff = Hashtbl.fold((k: Location.t, v: list<Comment.t>, acc) => {
    let loc = Doc.concat(list{
      Doc.lbracket,
      Doc.text(string_of_int(k.loc_start.pos_lnum)),
      Doc.text(":"),
      Doc.text(string_of_int(k.loc_start.pos_cnum - k.loc_start.pos_bol)),
      Doc.text("-"),
      Doc.text(string_of_int(k.loc_end.pos_lnum)),
      Doc.text(":"),
      Doc.text(string_of_int(k.loc_end.pos_cnum - k.loc_end.pos_bol)),
      Doc.rbracket,
    })
    let doc = Doc.breakableGroup(
      ~forceBreak=true,
      Doc.concat(list{
        loc,
        Doc.indent(
          Doc.concat(list{
            Doc.line,
            Doc.join(~sep=Doc.comma, List.map(c => Doc.text(Comment.txt(c)), v)),
          }),
        ),
        Doc.line,
      }),
    )
    list{doc, ...acc}
  }, t.leading, list{})

  let trailingStuff = Hashtbl.fold((k: Location.t, v: list<Comment.t>, acc) => {
    let loc = Doc.concat(list{
      Doc.lbracket,
      Doc.text(string_of_int(k.loc_start.pos_lnum)),
      Doc.text(":"),
      Doc.text(string_of_int(k.loc_start.pos_cnum - k.loc_start.pos_bol)),
      Doc.text("-"),
      Doc.text(string_of_int(k.loc_end.pos_lnum)),
      Doc.text(":"),
      Doc.text(string_of_int(k.loc_end.pos_cnum - k.loc_end.pos_bol)),
      Doc.rbracket,
    })
    let doc = Doc.breakableGroup(
      ~forceBreak=true,
      Doc.concat(list{
        loc,
        Doc.indent(
          Doc.concat(list{
            Doc.line,
            Doc.join(
              ~sep=Doc.concat(list{Doc.comma, Doc.line}),
              List.map(c => Doc.text(Comment.txt(c)), v),
            ),
          }),
        ),
        Doc.line,
      }),
    )
    list{doc, ...acc}
  }, t.trailing, list{})

  Doc.breakableGroup(
    ~forceBreak=true,
    Doc.concat(list{
      Doc.text("leading comments:"),
      Doc.line,
      Doc.indent(Doc.concat(leadingStuff)),
      Doc.line,
      Doc.line,
      Doc.text("trailing comments:"),
      Doc.indent(Doc.concat(trailingStuff)),
      Doc.line,
      Doc.line,
    }),
  )
  |> Doc.toString(~width=80)
  |> print_endline
}

let attach = (tbl, loc, comments) =>
  switch comments {
  | list{} => ()
  | comments => Hashtbl.replace(tbl, loc, comments)
  }

let partitionByLoc = (comments, loc) => {
  let rec loop = ((leading, inside, trailing), comments) => {
    open Location
    switch comments {
    | list{comment, ...rest} =>
      let cmtLoc = Comment.loc(comment)
      if cmtLoc.loc_end.pos_cnum <= loc.loc_start.pos_cnum {
        loop((list{comment, ...leading}, inside, trailing), rest)
      } else if cmtLoc.loc_start.pos_cnum >= loc.loc_end.pos_cnum {
        loop((leading, inside, list{comment, ...trailing}), rest)
      } else {
        loop((leading, list{comment, ...inside}, trailing), rest)
      }
    | list{} => (List.rev(leading), List.rev(inside), List.rev(trailing))
    }
  }

  loop((list{}, list{}, list{}), comments)
}

let partitionLeadingTrailing = (comments, loc) => {
  let rec loop = ((leading, trailing), comments) => {
    open Location
    switch comments {
    | list{comment, ...rest} =>
      let cmtLoc = Comment.loc(comment)
      if cmtLoc.loc_end.pos_cnum <= loc.loc_start.pos_cnum {
        loop((list{comment, ...leading}, trailing), rest)
      } else {
        loop((leading, list{comment, ...trailing}), rest)
      }
    | list{} => (List.rev(leading), List.rev(trailing))
    }
  }

  loop((list{}, list{}), comments)
}

let partitionByOnSameLine = (loc, comments) => {
  let rec loop = ((onSameLine, onOtherLine), comments) => {
    open Location
    switch comments {
    | list{} => (List.rev(onSameLine), List.rev(onOtherLine))
    | list{comment, ...rest} =>
      let cmtLoc = Comment.loc(comment)
      if cmtLoc.loc_start.pos_lnum === loc.loc_end.pos_lnum {
        loop((list{comment, ...onSameLine}, onOtherLine), rest)
      } else {
        loop((onSameLine, list{comment, ...onOtherLine}), rest)
      }
    }
  }

  loop((list{}, list{}), comments)
}

let partitionAdjacentTrailing = (loc1, comments) => {
  open Location
  open Lexing
  let rec loop = (~prevEndPos, afterLoc1, comments) =>
    switch comments {
    | list{} => (List.rev(afterLoc1), list{})
    | list{comment, ...rest} as comments =>
      let cmtPrevEndPos = Comment.prevTokEndPos(comment)
      if prevEndPos.Lexing.pos_cnum === cmtPrevEndPos.pos_cnum {
        let commentEnd = Comment.loc(comment).loc_end
        loop(~prevEndPos=commentEnd, list{comment, ...afterLoc1}, rest)
      } else {
        (List.rev(afterLoc1), comments)
      }
    }

  loop(~prevEndPos=loc1.loc_end, list{}, comments)
}

let rec collectListPatterns = (acc, pattern) => {
  open Parsetree
  switch pattern.ppat_desc {
  | Ppat_construct({txt: Longident.Lident("::")}, Some({ppat_desc: Ppat_tuple(list{pat, rest})})) =>
    collectListPatterns(list{pat, ...acc}, rest)
  | Ppat_construct({txt: Longident.Lident("[]")}, None) => List.rev(acc)
  | _ => List.rev(list{pattern, ...acc})
  }
}

let rec collectListExprs = (acc, expr) => {
  open Parsetree
  switch expr.pexp_desc {
  | Pexp_construct(
      {txt: Longident.Lident("::")},
      Some({pexp_desc: Pexp_tuple(list{expr, rest})}),
    ) =>
    collectListExprs(list{expr, ...acc}, rest)
  | Pexp_construct({txt: Longident.Lident("[]")}, _) => List.rev(acc)
  | _ => List.rev(list{expr, ...acc})
  }
}

/* TODO: use ParsetreeViewer */
let arrowType = ct => {
  open Parsetree
  let rec process = (attrsBefore, acc, typ) =>
    switch typ {
    | {ptyp_desc: Ptyp_arrow(Nolabel as lbl, typ1, typ2), ptyp_attributes: list{}} =>
      let arg = (list{}, lbl, typ1)
      process(attrsBefore, list{arg, ...acc}, typ2)
    | {
        ptyp_desc: Ptyp_arrow(Nolabel as lbl, typ1, typ2),
        ptyp_attributes: list{({txt: "bs"}, _)} as attrs,
      } =>
      let arg = (attrs, lbl, typ1)
      process(attrsBefore, list{arg, ...acc}, typ2)
    | {ptyp_desc: Ptyp_arrow(Nolabel, _typ1, _typ2), ptyp_attributes: _attrs} as returnType =>
      let args = List.rev(acc)
      (attrsBefore, args, returnType)
    | {
        ptyp_desc: Ptyp_arrow((Labelled(_) | Optional(_)) as lbl, typ1, typ2),
        ptyp_attributes: attrs,
      } =>
      let arg = (attrs, lbl, typ1)
      process(attrsBefore, list{arg, ...acc}, typ2)
    | typ => (attrsBefore, List.rev(acc), typ)
    }

  switch ct {
  | {ptyp_desc: Ptyp_arrow(Nolabel, _typ1, _typ2), ptyp_attributes: attrs} as typ =>
    process(attrs, list{}, {...typ, ptyp_attributes: list{}})
  | typ => process(list{}, list{}, typ)
  }
}

/* TODO: avoiding the dependency on ParsetreeViewer here, is this a good idea? */
let modExprApply = modExpr => {
  let rec loop = (acc, modExpr) =>
    switch modExpr {
    | {Parsetree.pmod_desc: Pmod_apply(next, arg)} => loop(list{arg, ...acc}, next)
    | _ => list{modExpr, ...acc}
    }

  loop(list{}, modExpr)
}

/* TODO: avoiding the dependency on ParsetreeViewer here, is this a good idea? */
let modExprFunctor = modExpr => {
  let rec loop = (acc, modExpr) =>
    switch modExpr {
    | {Parsetree.pmod_desc: Pmod_functor(lbl, modType, returnModExpr), pmod_attributes: attrs} =>
      let param = (attrs, lbl, modType)
      loop(list{param, ...acc}, returnModExpr)
    | returnModExpr => (List.rev(acc), returnModExpr)
    }

  loop(list{}, modExpr)
}

let functorType = modtype => {
  let rec process = (acc, modtype) =>
    switch modtype {
    | {Parsetree.pmty_desc: Pmty_functor(lbl, argType, returnType), pmty_attributes: attrs} =>
      let arg = (attrs, lbl, argType)
      process(list{arg, ...acc}, returnType)
    | modType => (List.rev(acc), modType)
    }

  process(list{}, modtype)
}

let funExpr = expr => {
  open Parsetree
  /* Turns (type t, type u, type z) into "type t u z" */
  let rec collectNewTypes = (acc, returnExpr) =>
    switch returnExpr {
    | {pexp_desc: Pexp_newtype(stringLoc, returnExpr), pexp_attributes: list{}} =>
      collectNewTypes(list{stringLoc, ...acc}, returnExpr)
    | returnExpr =>
      let loc = switch (acc, List.rev(acc)) {
      | (list{_startLoc, ..._}, list{endLoc, ..._}) => {...endLoc.loc, loc_end: endLoc.loc.loc_end}
      | _ => Location.none
      }

      let txt = List.fold_right((curr, acc) => acc ++ (" " ++ curr.Location.txt), acc, "type")
      (Location.mkloc(txt, loc), returnExpr)
    }

  /* For simplicity reason Pexp_newtype gets converted to a Nolabel parameter,
   * otherwise this function would need to return a variant:
   * | NormalParamater(...)
   * | NewType(...)
   * This complicates printing with an extra variant/boxing/allocation for a code-path
   * that is not often used. Lets just keep it simple for now */
  let rec collect = (attrsBefore, acc, expr) =>
    switch expr {
    | {pexp_desc: Pexp_fun(lbl, defaultExpr, pattern, returnExpr), pexp_attributes: list{}} =>
      let parameter = (list{}, lbl, defaultExpr, pattern)
      collect(attrsBefore, list{parameter, ...acc}, returnExpr)
    | {pexp_desc: Pexp_newtype(stringLoc, rest), pexp_attributes: attrs} =>
      let (var, returnExpr) = collectNewTypes(list{stringLoc}, rest)
      let parameter = (attrs, Asttypes.Nolabel, None, Ast_helper.Pat.var(~loc=stringLoc.loc, var))
      collect(attrsBefore, list{parameter, ...acc}, returnExpr)
    | {
        pexp_desc: Pexp_fun(lbl, defaultExpr, pattern, returnExpr),
        pexp_attributes: list{({txt: "bs"}, _)} as attrs,
      } =>
      let parameter = (attrs, lbl, defaultExpr, pattern)
      collect(attrsBefore, list{parameter, ...acc}, returnExpr)
    | {
        pexp_desc: Pexp_fun((Labelled(_) | Optional(_)) as lbl, defaultExpr, pattern, returnExpr),
        pexp_attributes: attrs,
      } =>
      let parameter = (attrs, lbl, defaultExpr, pattern)
      collect(attrsBefore, list{parameter, ...acc}, returnExpr)
    | expr => (attrsBefore, List.rev(acc), expr)
    }

  switch expr {
  | {
      pexp_desc: Pexp_fun(Nolabel, _defaultExpr, _pattern, _returnExpr),
      pexp_attributes: attrs,
    } as expr =>
    collect(attrs, list{}, {...expr, pexp_attributes: list{}})
  | expr => collect(list{}, list{}, expr)
  }
}

let rec isBlockExpr = expr => {
  open Parsetree
  switch expr.pexp_desc {
  | Pexp_letmodule(_)
  | Pexp_letexception(_)
  | Pexp_let(_)
  | Pexp_open(_)
  | Pexp_sequence(_) => true
  | Pexp_apply(callExpr, _) if isBlockExpr(callExpr) => true
  | Pexp_constraint(expr, _) if isBlockExpr(expr) => true
  | Pexp_field(expr, _) if isBlockExpr(expr) => true
  | Pexp_setfield(expr, _, _) if isBlockExpr(expr) => true
  | _ => false
  }
}

let isIfThenElseExpr = expr => {
  open Parsetree
  switch expr.pexp_desc {
  | Pexp_ifthenelse(_) => true
  | _ => false
  }
}

let rec walkStructure = (s, t, comments) =>
  switch s {
  | _ if comments == list{} => ()
  | list{} => attach(t.inside, Location.none, comments)
  | s => walkList(~getLoc=n => n.Parsetree.pstr_loc, ~walkNode=walkStructureItem, s, t, comments)
  }

and walkStructureItem = (si, t, comments) =>
  switch si.Parsetree.pstr_desc {
  | _ if comments == list{} => ()
  | Pstr_primitive(valueDescription) => walkValueDescription(valueDescription, t, comments)
  | Pstr_open(openDescription) => walkOpenDescription(openDescription, t, comments)
  | Pstr_value(_, valueBindings) => walkValueBindings(valueBindings, t, comments)
  | Pstr_type(_, typeDeclarations) => walkTypeDeclarations(typeDeclarations, t, comments)
  | Pstr_eval(expr, _) => walkExpr(expr, t, comments)
  | Pstr_module(moduleBinding) => walkModuleBinding(moduleBinding, t, comments)
  | Pstr_recmodule(moduleBindings) =>
    walkList(
      ~getLoc=mb => mb.Parsetree.pmb_loc,
      ~walkNode=walkModuleBinding,
      moduleBindings,
      t,
      comments,
    )
  | Pstr_modtype(modTypDecl) => walkModuleTypeDeclaration(modTypDecl, t, comments)
  | Pstr_attribute(attribute) => walkAttribute(attribute, t, comments)
  | Pstr_extension(extension, _) => walkExtension(extension, t, comments)
  | Pstr_include(includeDeclaration) => walkIncludeDeclaration(includeDeclaration, t, comments)
  | Pstr_exception(extensionConstructor) => walkExtConstr(extensionConstructor, t, comments)
  | Pstr_typext(typeExtension) => walkTypeExtension(typeExtension, t, comments)
  | Pstr_class_type(_) | Pstr_class(_) => ()
  }

and walkValueDescription = (vd, t, comments) => {
  let (leading, trailing) = partitionLeadingTrailing(comments, vd.pval_name.loc)
  attach(t.leading, vd.pval_name.loc, leading)
  let (afterName, rest) = partitionAdjacentTrailing(vd.pval_name.loc, trailing)
  attach(t.trailing, vd.pval_name.loc, afterName)
  let (before, inside, after) = partitionByLoc(rest, vd.pval_type.ptyp_loc)

  attach(t.leading, vd.pval_type.ptyp_loc, before)
  walkTypExpr(vd.pval_type, t, inside)
  attach(t.trailing, vd.pval_type.ptyp_loc, after)
}

and walkTypeExtension = (te, t, comments) => {
  let (leading, trailing) = partitionLeadingTrailing(comments, te.ptyext_path.loc)
  attach(t.leading, te.ptyext_path.loc, leading)
  let (afterPath, rest) = partitionAdjacentTrailing(te.ptyext_path.loc, trailing)
  attach(t.trailing, te.ptyext_path.loc, afterPath)

  /* type params */
  let rest = switch te.ptyext_params {
  | list{} => rest
  | typeParams =>
    visitListButContinueWithRemainingComments(
      ~getLoc=((typexpr, _variance)) => typexpr.Parsetree.ptyp_loc,
      ~walkNode=walkTypeParam,
      ~newlineDelimited=false,
      typeParams,
      t,
      rest,
    )
  }

  walkList(
    ~getLoc=n => n.Parsetree.pext_loc,
    ~walkNode=walkExtConstr,
    te.ptyext_constructors,
    t,
    rest,
  )
}

and walkIncludeDeclaration = (inclDecl, t, comments) => {
  let (before, inside, after) = partitionByLoc(comments, inclDecl.pincl_mod.pmod_loc)
  attach(t.leading, inclDecl.pincl_mod.pmod_loc, before)
  walkModExpr(inclDecl.pincl_mod, t, inside)
  attach(t.trailing, inclDecl.pincl_mod.pmod_loc, after)
}

and walkModuleTypeDeclaration = (mtd, t, comments) => {
  let (leading, trailing) = partitionLeadingTrailing(comments, mtd.pmtd_name.loc)
  attach(t.leading, mtd.pmtd_name.loc, leading)
  switch mtd.pmtd_type {
  | None => attach(t.trailing, mtd.pmtd_name.loc, trailing)
  | Some(modType) =>
    let (afterName, rest) = partitionAdjacentTrailing(mtd.pmtd_name.loc, trailing)
    attach(t.trailing, mtd.pmtd_name.loc, afterName)
    let (before, inside, after) = partitionByLoc(rest, modType.pmty_loc)
    attach(t.leading, modType.pmty_loc, before)
    walkModType(modType, t, inside)
    attach(t.trailing, modType.pmty_loc, after)
  }
}

and walkModuleBinding = (mb, t, comments) => {
  let (leading, trailing) = partitionLeadingTrailing(comments, mb.pmb_name.loc)
  attach(t.leading, mb.pmb_name.loc, leading)
  let (afterName, rest) = partitionAdjacentTrailing(mb.pmb_name.loc, trailing)
  attach(t.trailing, mb.pmb_name.loc, afterName)
  let (leading, inside, trailing) = partitionByLoc(rest, mb.pmb_expr.pmod_loc)
  switch mb.pmb_expr.pmod_desc {
  | Pmod_constraint(_) => walkModExpr(mb.pmb_expr, t, List.concat(list{leading, inside}))
  | _ =>
    attach(t.leading, mb.pmb_expr.pmod_loc, leading)
    walkModExpr(mb.pmb_expr, t, inside)
  }
  attach(t.trailing, mb.pmb_expr.pmod_loc, trailing)
}

and walkSignature = (signature, t, comments) =>
  switch signature {
  | _ if comments == list{} => ()
  | list{} => attach(t.inside, Location.none, comments)
  | _s =>
    walkList(~getLoc=n => n.Parsetree.psig_loc, ~walkNode=walkSignatureItem, signature, t, comments)
  }

and walkSignatureItem = (si, t, comments) =>
  switch si.psig_desc {
  | _ if comments == list{} => ()
  | Psig_value(valueDescription) => walkValueDescription(valueDescription, t, comments)
  | Psig_type(_, typeDeclarations) => walkTypeDeclarations(typeDeclarations, t, comments)
  | Psig_typext(typeExtension) => walkTypeExtension(typeExtension, t, comments)
  | Psig_exception(extensionConstructor) => walkExtConstr(extensionConstructor, t, comments)
  | Psig_module(moduleDeclaration) => walkModuleDeclaration(moduleDeclaration, t, comments)
  | Psig_recmodule(moduleDeclarations) =>
    walkList(
      ~getLoc=n => n.Parsetree.pmd_loc,
      ~walkNode=walkModuleDeclaration,
      moduleDeclarations,
      t,
      comments,
    )
  | Psig_modtype(moduleTypeDeclaration) =>
    walkModuleTypeDeclaration(moduleTypeDeclaration, t, comments)
  | Psig_open(openDescription) => walkOpenDescription(openDescription, t, comments)
  | Psig_include(includeDescription) => walkIncludeDescription(includeDescription, t, comments)
  | Psig_attribute(attribute) => walkAttribute(attribute, t, comments)
  | Psig_extension(extension, _) => walkExtension(extension, t, comments)
  | Psig_class(_) | Psig_class_type(_) => ()
  }

and walkIncludeDescription = (id, t, comments) => {
  let (before, inside, after) = partitionByLoc(comments, id.pincl_mod.pmty_loc)
  attach(t.leading, id.pincl_mod.pmty_loc, before)
  walkModType(id.pincl_mod, t, inside)
  attach(t.trailing, id.pincl_mod.pmty_loc, after)
}

and walkModuleDeclaration = (md, t, comments) => {
  let (leading, trailing) = partitionLeadingTrailing(comments, md.pmd_name.loc)
  attach(t.leading, md.pmd_name.loc, leading)
  let (afterName, rest) = partitionAdjacentTrailing(md.pmd_name.loc, trailing)
  attach(t.trailing, md.pmd_name.loc, afterName)
  let (leading, inside, trailing) = partitionByLoc(rest, md.pmd_type.pmty_loc)
  attach(t.leading, md.pmd_type.pmty_loc, leading)
  walkModType(md.pmd_type, t, inside)
  attach(t.trailing, md.pmd_type.pmty_loc, trailing)
}

and walkList: 'node. (
  ~prevLoc: Location.t=?,
  ~getLoc: 'node => Location.t,
  ~walkNode: ('node, t, list<Comment.t>) => unit,
  list<'node>,
  t,
  list<Comment.t>,
) => unit = (~prevLoc=?, ~getLoc, ~walkNode, l, t, comments) => {
  open Location
  switch l {
  | _ if comments == list{} => ()
  | list{} =>
    switch prevLoc {
    | Some(loc) => attach(t.trailing, loc, comments)
    | None => ()
    }
  | list{node, ...rest} =>
    let currLoc = getLoc(node)
    let (leading, inside, trailing) = partitionByLoc(comments, currLoc)
    switch prevLoc {
    | None =>
      /* first node, all leading comments attach here */
      attach(t.leading, currLoc, leading)
    | Some(prevLoc) =>
      /* Same line */
      if prevLoc.loc_end.pos_lnum === currLoc.loc_start.pos_lnum {
        let (afterPrev, beforeCurr) = partitionAdjacentTrailing(prevLoc, leading)
        let () = attach(t.trailing, prevLoc, afterPrev)
        attach(t.leading, currLoc, beforeCurr)
      } else {
        let (onSameLineAsPrev, afterPrev) = partitionByOnSameLine(prevLoc, leading)
        let () = attach(t.trailing, prevLoc, onSameLineAsPrev)
        let (leading, _inside, _trailing) = partitionByLoc(afterPrev, currLoc)
        attach(t.leading, currLoc, leading)
      }
    }
    walkNode(node, t, inside)
    walkList(~prevLoc=currLoc, ~getLoc, ~walkNode, rest, t, trailing)
  }
}

/* The parsetree doesn't always contain location info about the opening or
 * closing token of a "list-of-things". This routine visits the whole list,
 * but returns any remaining comments that likely fall after the whole list. */
and visitListButContinueWithRemainingComments: 'node. (
  ~prevLoc: Location.t=?,
  ~newlineDelimited: bool,
  ~getLoc: 'node => Location.t,
  ~walkNode: ('node, t, list<Comment.t>) => unit,
  list<'node>,
  t,
  list<Comment.t>,
) => list<Comment.t> = (~prevLoc=?, ~newlineDelimited, ~getLoc, ~walkNode, l, t, comments) => {
  open Location
  switch l {
  | _ if comments == list{} => list{}
  | list{} =>
    switch prevLoc {
    | Some(loc) =>
      let (afterPrev, rest) = if newlineDelimited {
        partitionByOnSameLine(loc, comments)
      } else {
        partitionAdjacentTrailing(loc, comments)
      }

      attach(t.trailing, loc, afterPrev)
      rest
    | None => comments
    }
  | list{node, ...rest} =>
    let currLoc = getLoc(node)
    let (leading, inside, trailing) = partitionByLoc(comments, currLoc)
    let () = switch prevLoc {
    | None =>
      /* first node, all leading comments attach here */
      attach(t.leading, currLoc, leading)
      ()
    | Some(prevLoc) =>
      /* Same line */
      if prevLoc.loc_end.pos_lnum === currLoc.loc_start.pos_lnum {
        let (afterPrev, beforeCurr) = partitionAdjacentTrailing(prevLoc, leading)
        let () = attach(t.trailing, prevLoc, afterPrev)
        let () = attach(t.leading, currLoc, beforeCurr)
      } else {
        let (onSameLineAsPrev, afterPrev) = partitionByOnSameLine(prevLoc, leading)
        let () = attach(t.trailing, prevLoc, onSameLineAsPrev)
        let (leading, _inside, _trailing) = partitionByLoc(afterPrev, currLoc)
        let () = attach(t.leading, currLoc, leading)
      }
    }

    walkNode(node, t, inside)
    visitListButContinueWithRemainingComments(
      ~prevLoc=currLoc,
      ~getLoc,
      ~walkNode,
      ~newlineDelimited,
      rest,
      t,
      trailing,
    )
  }
}

and walkValueBindings = (vbs, t, comments) =>
  walkList(~getLoc=n => n.Parsetree.pvb_loc, ~walkNode=walkValueBinding, vbs, t, comments)

and walkOpenDescription = (openDescription, t, comments) => {
  let loc = openDescription.popen_lid.loc
  let (leading, trailing) = partitionLeadingTrailing(comments, loc)
  attach(t.leading, loc, leading)
  attach(t.trailing, loc, trailing)
}

and walkTypeDeclarations = (typeDeclarations, t, comments) =>
  walkList(
    ~getLoc=n => n.Parsetree.ptype_loc,
    ~walkNode=walkTypeDeclaration,
    typeDeclarations,
    t,
    comments,
  )

and walkTypeParam = ((typexpr, _variance), t, comments) => walkTypExpr(typexpr, t, comments)

and walkTypeDeclaration = (td, t, comments) => {
  let (beforeName, rest) = partitionLeadingTrailing(comments, td.ptype_name.loc)
  attach(t.leading, td.ptype_name.loc, beforeName)

  let (afterName, rest) = partitionAdjacentTrailing(td.ptype_name.loc, rest)
  attach(t.trailing, td.ptype_name.loc, afterName)

  /* type params */
  let rest = switch td.ptype_params {
  | list{} => rest
  | typeParams =>
    visitListButContinueWithRemainingComments(
      ~getLoc=((typexpr, _variance)) => typexpr.Parsetree.ptyp_loc,
      ~walkNode=walkTypeParam,
      ~newlineDelimited=false,
      typeParams,
      t,
      rest,
    )
  }

  /* manifest:  = typexpr */
  let rest = switch td.ptype_manifest {
  | Some(typexpr) =>
    let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(rest, typexpr.ptyp_loc)
    attach(t.leading, typexpr.ptyp_loc, beforeTyp)
    walkTypExpr(typexpr, t, insideTyp)
    let (afterTyp, rest) = partitionAdjacentTrailing(typexpr.Parsetree.ptyp_loc, afterTyp)
    attach(t.trailing, typexpr.ptyp_loc, afterTyp)
    rest
  | None => rest
  }

  let rest = switch td.ptype_kind {
  | Ptype_abstract | Ptype_open => rest
  | Ptype_record(labelDeclarations) =>
    let () = walkList(
      ~getLoc=ld => ld.Parsetree.pld_loc,
      ~walkNode=walkLabelDeclaration,
      labelDeclarations,
      t,
      rest,
    )

    list{}
  | Ptype_variant(constructorDeclarations) =>
    walkConstructorDeclarations(constructorDeclarations, t, rest)
  }

  attach(t.trailing, td.ptype_loc, rest)
}

and walkLabelDeclarations = (lds, t, comments) =>
  visitListButContinueWithRemainingComments(
    ~getLoc=ld => ld.Parsetree.pld_loc,
    ~walkNode=walkLabelDeclaration,
    ~newlineDelimited=false,
    lds,
    t,
    comments,
  )

and walkLabelDeclaration = (ld, t, comments) => {
  let (beforeName, rest) = partitionLeadingTrailing(comments, ld.pld_name.loc)
  attach(t.leading, ld.pld_name.loc, beforeName)
  let (afterName, rest) = partitionAdjacentTrailing(ld.pld_name.loc, rest)
  attach(t.trailing, ld.pld_name.loc, afterName)
  let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(rest, ld.pld_type.ptyp_loc)
  attach(t.leading, ld.pld_type.ptyp_loc, beforeTyp)
  walkTypExpr(ld.pld_type, t, insideTyp)
  attach(t.trailing, ld.pld_type.ptyp_loc, afterTyp)
}

and walkConstructorDeclarations = (cds, t, comments) =>
  visitListButContinueWithRemainingComments(
    ~getLoc=cd => cd.Parsetree.pcd_loc,
    ~walkNode=walkConstructorDeclaration,
    ~newlineDelimited=false,
    cds,
    t,
    comments,
  )

and walkConstructorDeclaration = (cd, t, comments) => {
  let (beforeName, rest) = partitionLeadingTrailing(comments, cd.pcd_name.loc)
  attach(t.leading, cd.pcd_name.loc, beforeName)
  let (afterName, rest) = partitionAdjacentTrailing(cd.pcd_name.loc, rest)
  attach(t.trailing, cd.pcd_name.loc, afterName)
  let rest = walkConstructorArguments(cd.pcd_args, t, rest)

  let rest = switch cd.pcd_res {
  | Some(typexpr) =>
    let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(rest, typexpr.ptyp_loc)
    attach(t.leading, typexpr.ptyp_loc, beforeTyp)
    walkTypExpr(typexpr, t, insideTyp)
    let (afterTyp, rest) = partitionAdjacentTrailing(typexpr.Parsetree.ptyp_loc, afterTyp)
    attach(t.trailing, typexpr.ptyp_loc, afterTyp)
    rest
  | None => rest
  }

  attach(t.trailing, cd.pcd_loc, rest)
}

and walkConstructorArguments = (args, t, comments) =>
  switch args {
  | Pcstr_tuple(typexprs) =>
    visitListButContinueWithRemainingComments(
      ~getLoc=n => n.Parsetree.ptyp_loc,
      ~walkNode=walkTypExpr,
      ~newlineDelimited=false,
      typexprs,
      t,
      comments,
    )
  | Pcstr_record(labelDeclarations) => walkLabelDeclarations(labelDeclarations, t, comments)
  }

and walkValueBinding = (vb, t, comments) => {
  open Location

  let vb = {
    open Parsetree
    switch (vb.pvb_pat, vb.pvb_expr) {
    | (
        {ppat_desc: Ppat_constraint(pat, {ptyp_desc: Ptyp_poly(list{}, t)})},
        {pexp_desc: Pexp_constraint(expr, _typ)},
      ) => {
        ...vb,
        pvb_pat: Ast_helper.Pat.constraint_(
          ~loc={...pat.ppat_loc, loc_end: t.Parsetree.ptyp_loc.loc_end},
          pat,
          t,
        ),
        pvb_expr: expr,
      }
    | (
        {ppat_desc: Ppat_constraint(pat, {ptyp_desc: Ptyp_poly(list{_, ..._}, t)})},
        {pexp_desc: Pexp_fun(_)},
      ) => {
        ...vb,
        pvb_pat: {
          ...vb.pvb_pat,
          ppat_loc: {...pat.ppat_loc, loc_end: t.ptyp_loc.loc_end},
        },
      }

    | (
        {
          ppat_desc: Ppat_constraint(pat, {ptyp_desc: Ptyp_poly(list{_, ..._}, t)} as typ),
        } as constrainedPattern,
        {pexp_desc: Pexp_newtype(_, {pexp_desc: Pexp_constraint(expr, _)})},
      ) => /*
       * The location of the Ptyp_poly on the pattern is the whole thing.
       * let x:
       *   type t. (int, int) => int =
       *   (a, b) => {
       *     // comment
       *     a + b
       *   }
       */
      {
        ...vb,
        pvb_pat: {
          ...constrainedPattern,
          ppat_desc: Ppat_constraint(pat, typ),
          ppat_loc: {...constrainedPattern.ppat_loc, loc_end: t.ptyp_loc.loc_end},
        },
        pvb_expr: expr,
      }
    | _ => vb
    }
  }

  let patternLoc = vb.Parsetree.pvb_pat.ppat_loc
  let exprLoc = vb.Parsetree.pvb_expr.pexp_loc
  let expr = vb.pvb_expr

  let (leading, inside, trailing) = partitionByLoc(comments, patternLoc)

  /* everything before start of pattern can only be leading on the pattern:
   *   let |* before *| a = 1 */
  attach(t.leading, patternLoc, leading)
  walkPattern(vb.Parsetree.pvb_pat, t, inside)
  let (afterPat, surroundingExpr) = partitionAdjacentTrailing(patternLoc, trailing)

  attach(t.trailing, patternLoc, afterPat)
  let (beforeExpr, insideExpr, afterExpr) = partitionByLoc(surroundingExpr, exprLoc)
  if isBlockExpr(expr) {
    walkExpr(expr, t, List.concat(list{beforeExpr, insideExpr, afterExpr}))
  } else {
    attach(t.leading, exprLoc, beforeExpr)
    walkExpr(expr, t, insideExpr)
    attach(t.trailing, exprLoc, afterExpr)
  }
}

and walkExpr = (expr, t, comments) => {
  open Location
  switch expr.Parsetree.pexp_desc {
  | _ if comments == list{} => ()
  | Pexp_constant(_) =>
    let (leading, trailing) = partitionLeadingTrailing(comments, expr.pexp_loc)
    attach(t.leading, expr.pexp_loc, leading)
    attach(t.trailing, expr.pexp_loc, trailing)
  | Pexp_ident(longident) =>
    let (leading, trailing) = partitionLeadingTrailing(comments, longident.loc)
    attach(t.leading, longident.loc, leading)
    attach(t.trailing, longident.loc, trailing)
  | Pexp_let(
      _recFlag,
      valueBindings,
      {pexp_desc: Pexp_construct({txt: Longident.Lident("()")}, None)},
    ) =>
    walkValueBindings(valueBindings, t, comments)
  | Pexp_let(_recFlag, valueBindings, expr2) =>
    let comments = visitListButContinueWithRemainingComments(~getLoc=n =>
      if n.Parsetree.pvb_pat.ppat_loc.loc_ghost {
        n.pvb_expr.pexp_loc
      } else {
        n.Parsetree.pvb_loc
      }
    , ~walkNode=walkValueBinding, ~newlineDelimited=true, valueBindings, t, comments)

    if isBlockExpr(expr2) {
      walkExpr(expr2, t, comments)
    } else {
      let (leading, inside, trailing) = partitionByLoc(comments, expr2.pexp_loc)
      attach(t.leading, expr2.pexp_loc, leading)
      walkExpr(expr2, t, inside)
      attach(t.trailing, expr2.pexp_loc, trailing)
    }
  | Pexp_sequence(expr1, expr2) =>
    let (leading, inside, trailing) = partitionByLoc(comments, expr1.pexp_loc)
    let comments = if isBlockExpr(expr1) {
      let (afterExpr, comments) = partitionByOnSameLine(expr1.pexp_loc, trailing)
      walkExpr(expr1, t, List.concat(list{leading, inside, afterExpr}))
      comments
    } else {
      attach(t.leading, expr1.pexp_loc, leading)
      walkExpr(expr1, t, inside)
      let (afterExpr, comments) = partitionByOnSameLine(expr1.pexp_loc, trailing)
      attach(t.trailing, expr1.pexp_loc, afterExpr)
      comments
    }
    if isBlockExpr(expr2) {
      walkExpr(expr2, t, comments)
    } else {
      let (leading, inside, trailing) = partitionByLoc(comments, expr2.pexp_loc)
      attach(t.leading, expr2.pexp_loc, leading)
      walkExpr(expr2, t, inside)
      attach(t.trailing, expr2.pexp_loc, trailing)
    }
  | Pexp_open(_override, longident, expr2) =>
    let (leading, comments) = partitionLeadingTrailing(comments, expr.pexp_loc)
    attach(t.leading, {...expr.pexp_loc, loc_end: longident.loc.loc_end}, leading)
    let (leading, trailing) = partitionLeadingTrailing(comments, longident.loc)
    attach(t.leading, longident.loc, leading)
    let (afterLongident, rest) = partitionByOnSameLine(longident.loc, trailing)
    attach(t.trailing, longident.loc, afterLongident)
    if isBlockExpr(expr2) {
      walkExpr(expr2, t, rest)
    } else {
      let (leading, inside, trailing) = partitionByLoc(rest, expr2.pexp_loc)
      attach(t.leading, expr2.pexp_loc, leading)
      walkExpr(expr2, t, inside)
      attach(t.trailing, expr2.pexp_loc, trailing)
    }
  | Pexp_extension(
      {txt: "bs.obj" | "obj"},
      PStr(list{{pstr_desc: Pstr_eval({pexp_desc: Pexp_record(rows, _)}, list{})}}),
    ) =>
    walkList(~getLoc=((longident, expr): (Asttypes.loc<Longident.t>, Parsetree.expression)) => {
      ...longident.loc,
      loc_end: expr.pexp_loc.loc_end,
    }, ~walkNode=walkExprRecordRow, rows, t, comments)
  | Pexp_extension(extension) => walkExtension(extension, t, comments)
  | Pexp_letexception(extensionConstructor, expr2) =>
    let (leading, comments) = partitionLeadingTrailing(comments, expr.pexp_loc)
    attach(t.leading, {...expr.pexp_loc, loc_end: extensionConstructor.pext_loc.loc_end}, leading)
    let (leading, inside, trailing) = partitionByLoc(comments, extensionConstructor.pext_loc)
    attach(t.leading, extensionConstructor.pext_loc, leading)
    walkExtConstr(extensionConstructor, t, inside)
    let (afterExtConstr, rest) = partitionByOnSameLine(extensionConstructor.pext_loc, trailing)
    attach(t.trailing, extensionConstructor.pext_loc, afterExtConstr)
    if isBlockExpr(expr2) {
      walkExpr(expr2, t, rest)
    } else {
      let (leading, inside, trailing) = partitionByLoc(rest, expr2.pexp_loc)
      attach(t.leading, expr2.pexp_loc, leading)
      walkExpr(expr2, t, inside)
      attach(t.trailing, expr2.pexp_loc, trailing)
    }
  | Pexp_letmodule(stringLoc, modExpr, expr2) =>
    let (leading, comments) = partitionLeadingTrailing(comments, expr.pexp_loc)
    attach(t.leading, {...expr.pexp_loc, loc_end: modExpr.pmod_loc.loc_end}, leading)
    let (leading, trailing) = partitionLeadingTrailing(comments, stringLoc.loc)
    attach(t.leading, stringLoc.loc, leading)
    let (afterString, rest) = partitionAdjacentTrailing(stringLoc.loc, trailing)
    attach(t.trailing, stringLoc.loc, afterString)
    let (beforeModExpr, insideModExpr, afterModExpr) = partitionByLoc(rest, modExpr.pmod_loc)
    attach(t.leading, modExpr.pmod_loc, beforeModExpr)
    walkModExpr(modExpr, t, insideModExpr)
    let (afterModExpr, rest) = partitionByOnSameLine(modExpr.pmod_loc, afterModExpr)
    attach(t.trailing, modExpr.pmod_loc, afterModExpr)
    if isBlockExpr(expr2) {
      walkExpr(expr2, t, rest)
    } else {
      let (leading, inside, trailing) = partitionByLoc(rest, expr2.pexp_loc)
      attach(t.leading, expr2.pexp_loc, leading)
      walkExpr(expr2, t, inside)
      attach(t.trailing, expr2.pexp_loc, trailing)
    }
  | Pexp_assert(expr)
  | Pexp_lazy(expr) =>
    if isBlockExpr(expr) {
      walkExpr(expr, t, comments)
    } else {
      let (leading, inside, trailing) = partitionByLoc(comments, expr.pexp_loc)
      attach(t.leading, expr.pexp_loc, leading)
      walkExpr(expr, t, inside)
      attach(t.trailing, expr.pexp_loc, trailing)
    }
  | Pexp_coerce(expr, optTypexpr, typexpr) =>
    let (leading, inside, trailing) = partitionByLoc(comments, expr.pexp_loc)
    attach(t.leading, expr.pexp_loc, leading)
    walkExpr(expr, t, inside)
    let (afterExpr, rest) = partitionAdjacentTrailing(expr.pexp_loc, trailing)
    attach(t.trailing, expr.pexp_loc, afterExpr)
    let rest = switch optTypexpr {
    | Some(typexpr) =>
      let (leading, inside, trailing) = partitionByLoc(comments, typexpr.ptyp_loc)
      attach(t.leading, typexpr.ptyp_loc, leading)
      walkTypExpr(typexpr, t, inside)
      let (afterTyp, rest) = partitionAdjacentTrailing(typexpr.ptyp_loc, trailing)
      attach(t.trailing, typexpr.ptyp_loc, afterTyp)
      rest
    | None => rest
    }

    let (leading, inside, trailing) = partitionByLoc(rest, typexpr.ptyp_loc)
    attach(t.leading, typexpr.ptyp_loc, leading)
    walkTypExpr(typexpr, t, inside)
    attach(t.trailing, typexpr.ptyp_loc, trailing)
  | Pexp_constraint(expr, typexpr) =>
    let (leading, inside, trailing) = partitionByLoc(comments, expr.pexp_loc)
    attach(t.leading, expr.pexp_loc, leading)
    walkExpr(expr, t, inside)
    let (afterExpr, rest) = partitionAdjacentTrailing(expr.pexp_loc, trailing)
    attach(t.trailing, expr.pexp_loc, afterExpr)
    let (leading, inside, trailing) = partitionByLoc(rest, typexpr.ptyp_loc)
    attach(t.leading, typexpr.ptyp_loc, leading)
    walkTypExpr(typexpr, t, inside)
    attach(t.trailing, typexpr.ptyp_loc, trailing)
  | Pexp_tuple(list{})
  | Pexp_array(list{})
  | Pexp_construct({txt: Longident.Lident("[]")}, _) =>
    attach(t.inside, expr.pexp_loc, comments)
  | Pexp_construct({txt: Longident.Lident("::")}, _) =>
    walkList(
      ~getLoc=n => n.Parsetree.pexp_loc,
      ~walkNode=walkExpr,
      collectListExprs(list{}, expr),
      t,
      comments,
    )
  | Pexp_construct(longident, args) =>
    let (leading, trailing) = partitionLeadingTrailing(comments, longident.loc)
    attach(t.leading, longident.loc, leading)
    switch args {
    | Some(expr) =>
      let (afterLongident, rest) = partitionAdjacentTrailing(longident.loc, trailing)
      attach(t.trailing, longident.loc, afterLongident)
      walkExpr(expr, t, rest)
    | None => attach(t.trailing, longident.loc, trailing)
    }
  | Pexp_variant(_label, None) => ()
  | Pexp_variant(_label, Some(expr)) => walkExpr(expr, t, comments)
  | Pexp_array(exprs) | Pexp_tuple(exprs) =>
    walkList(~getLoc=n => n.Parsetree.pexp_loc, ~walkNode=walkExpr, exprs, t, comments)
  | Pexp_record(rows, spreadExpr) =>
    let comments = switch spreadExpr {
    | None => comments
    | Some(expr) =>
      let (leading, inside, trailing) = partitionByLoc(comments, expr.pexp_loc)
      attach(t.leading, expr.pexp_loc, leading)
      walkExpr(expr, t, inside)
      let (afterExpr, rest) = partitionAdjacentTrailing(expr.pexp_loc, trailing)
      attach(t.trailing, expr.pexp_loc, afterExpr)
      rest
    }

    walkList(~getLoc=((longident, expr): (Asttypes.loc<Longident.t>, Parsetree.expression)) => {
      ...longident.loc,
      loc_end: expr.pexp_loc.loc_end,
    }, ~walkNode=walkExprRecordRow, rows, t, comments)
  | Pexp_field(expr, longident) =>
    let (leading, inside, trailing) = partitionByLoc(comments, expr.pexp_loc)
    let trailing = if isBlockExpr(expr) {
      let (afterExpr, rest) = partitionAdjacentTrailing(expr.pexp_loc, trailing)
      walkExpr(expr, t, List.concat(list{leading, inside, afterExpr}))
      rest
    } else {
      attach(t.leading, expr.pexp_loc, leading)
      walkExpr(expr, t, inside)
      trailing
    }
    let (afterExpr, rest) = partitionAdjacentTrailing(expr.pexp_loc, trailing)
    attach(t.trailing, expr.pexp_loc, afterExpr)
    let (leading, trailing) = partitionLeadingTrailing(rest, longident.loc)
    attach(t.leading, longident.loc, leading)
    attach(t.trailing, longident.loc, trailing)
  | Pexp_setfield(expr1, longident, expr2) =>
    let (leading, inside, trailing) = partitionByLoc(comments, expr1.pexp_loc)
    let rest = if isBlockExpr(expr1) {
      let (afterExpr, rest) = partitionAdjacentTrailing(expr1.pexp_loc, trailing)
      walkExpr(expr1, t, List.concat(list{leading, inside, afterExpr}))
      rest
    } else {
      let (afterExpr, rest) = partitionAdjacentTrailing(expr1.pexp_loc, trailing)
      attach(t.leading, expr1.pexp_loc, leading)
      walkExpr(expr1, t, inside)
      attach(t.trailing, expr1.pexp_loc, afterExpr)
      rest
    }
    let (beforeLongident, afterLongident) = partitionLeadingTrailing(rest, longident.loc)
    attach(t.leading, longident.loc, beforeLongident)
    let (afterLongident, rest) = partitionAdjacentTrailing(longident.loc, afterLongident)
    attach(t.trailing, longident.loc, afterLongident)
    if isBlockExpr(expr2) {
      walkExpr(expr2, t, rest)
    } else {
      let (leading, inside, trailing) = partitionByLoc(rest, expr2.pexp_loc)
      attach(t.leading, expr2.pexp_loc, leading)
      walkExpr(expr2, t, inside)
      attach(t.trailing, expr2.pexp_loc, trailing)
    }
  | Pexp_ifthenelse(ifExpr, thenExpr, elseExpr) =>
    let (leading, inside, trailing) = partitionByLoc(comments, ifExpr.pexp_loc)
    let comments = if isBlockExpr(ifExpr) {
      let (afterExpr, comments) = partitionAdjacentTrailing(ifExpr.pexp_loc, trailing)
      walkExpr(ifExpr, t, List.concat(list{leading, inside, afterExpr}))
      comments
    } else {
      attach(t.leading, ifExpr.pexp_loc, leading)
      walkExpr(ifExpr, t, inside)
      let (afterExpr, comments) = partitionAdjacentTrailing(ifExpr.pexp_loc, trailing)
      attach(t.trailing, ifExpr.pexp_loc, afterExpr)
      comments
    }
    let (leading, inside, trailing) = partitionByLoc(comments, thenExpr.pexp_loc)
    let comments = if isBlockExpr(thenExpr) {
      let (afterExpr, trailing) = partitionAdjacentTrailing(thenExpr.pexp_loc, trailing)
      walkExpr(thenExpr, t, List.concat(list{leading, inside, afterExpr}))
      trailing
    } else {
      attach(t.leading, thenExpr.pexp_loc, leading)
      walkExpr(thenExpr, t, inside)
      let (afterExpr, comments) = partitionAdjacentTrailing(thenExpr.pexp_loc, trailing)
      attach(t.trailing, thenExpr.pexp_loc, afterExpr)
      comments
    }
    switch elseExpr {
    | None => ()
    | Some(expr) =>
      if isBlockExpr(expr) || isIfThenElseExpr(expr) {
        walkExpr(expr, t, comments)
      } else {
        let (leading, inside, trailing) = partitionByLoc(comments, expr.pexp_loc)
        attach(t.leading, expr.pexp_loc, leading)
        walkExpr(expr, t, inside)
        attach(t.trailing, expr.pexp_loc, trailing)
      }
    }
  | Pexp_while(expr1, expr2) =>
    let (leading, inside, trailing) = partitionByLoc(comments, expr1.pexp_loc)
    let rest = if isBlockExpr(expr1) {
      let (afterExpr, rest) = partitionAdjacentTrailing(expr1.pexp_loc, trailing)
      walkExpr(expr1, t, List.concat(list{leading, inside, afterExpr}))
      rest
    } else {
      attach(t.leading, expr1.pexp_loc, leading)
      walkExpr(expr1, t, inside)
      let (afterExpr, rest) = partitionAdjacentTrailing(expr1.pexp_loc, trailing)
      attach(t.trailing, expr1.pexp_loc, afterExpr)
      rest
    }
    if isBlockExpr(expr2) {
      walkExpr(expr2, t, rest)
    } else {
      let (leading, inside, trailing) = partitionByLoc(rest, expr2.pexp_loc)
      attach(t.leading, expr2.pexp_loc, leading)
      walkExpr(expr2, t, inside)
      attach(t.trailing, expr2.pexp_loc, trailing)
    }
  | Pexp_for(pat, expr1, expr2, _, expr3) =>
    let (leading, inside, trailing) = partitionByLoc(comments, pat.ppat_loc)
    attach(t.leading, pat.ppat_loc, leading)
    walkPattern(pat, t, inside)
    let (afterPat, rest) = partitionAdjacentTrailing(pat.ppat_loc, trailing)
    attach(t.trailing, pat.ppat_loc, afterPat)
    let (leading, inside, trailing) = partitionByLoc(rest, expr1.pexp_loc)
    attach(t.leading, expr1.pexp_loc, leading)
    walkExpr(expr1, t, inside)
    let (afterExpr, rest) = partitionAdjacentTrailing(expr1.pexp_loc, trailing)
    attach(t.trailing, expr1.pexp_loc, afterExpr)
    let (leading, inside, trailing) = partitionByLoc(rest, expr2.pexp_loc)
    attach(t.leading, expr2.pexp_loc, leading)
    walkExpr(expr2, t, inside)
    let (afterExpr, rest) = partitionAdjacentTrailing(expr2.pexp_loc, trailing)
    attach(t.trailing, expr2.pexp_loc, afterExpr)
    if isBlockExpr(expr3) {
      walkExpr(expr3, t, rest)
    } else {
      let (leading, inside, trailing) = partitionByLoc(rest, expr3.pexp_loc)
      attach(t.leading, expr3.pexp_loc, leading)
      walkExpr(expr3, t, inside)
      attach(t.trailing, expr3.pexp_loc, trailing)
    }
  | Pexp_pack(modExpr) =>
    let (before, inside, after) = partitionByLoc(comments, modExpr.pmod_loc)
    attach(t.leading, modExpr.pmod_loc, before)
    walkModExpr(modExpr, t, inside)
    attach(t.trailing, modExpr.pmod_loc, after)
  | Pexp_match(expr1, list{case, elseBranch})
    if Res_parsetree_viewer.hasIfLetAttribute(expr.pexp_attributes) =>
    let (before, inside, after) = partitionByLoc(comments, case.pc_lhs.ppat_loc)
    attach(t.leading, case.pc_lhs.ppat_loc, before)
    walkPattern(case.pc_lhs, t, inside)
    let (afterPat, rest) = partitionAdjacentTrailing(case.pc_lhs.ppat_loc, after)
    attach(t.trailing, case.pc_lhs.ppat_loc, afterPat)
    let (before, inside, after) = partitionByLoc(rest, expr1.pexp_loc)
    attach(t.leading, expr1.pexp_loc, before)
    walkExpr(expr1, t, inside)
    let (afterExpr, rest) = partitionAdjacentTrailing(expr1.pexp_loc, after)
    attach(t.trailing, expr1.pexp_loc, afterExpr)
    let (before, inside, after) = partitionByLoc(rest, case.pc_rhs.pexp_loc)
    let after = if isBlockExpr(case.pc_rhs) {
      let (afterExpr, rest) = partitionAdjacentTrailing(case.pc_rhs.pexp_loc, after)
      walkExpr(case.pc_rhs, t, List.concat(list{before, inside, afterExpr}))
      rest
    } else {
      attach(t.leading, case.pc_rhs.pexp_loc, before)
      walkExpr(case.pc_rhs, t, inside)
      after
    }
    let (afterExpr, rest) = partitionAdjacentTrailing(case.pc_rhs.pexp_loc, after)
    attach(t.trailing, case.pc_rhs.pexp_loc, afterExpr)
    let (before, inside, after) = partitionByLoc(rest, elseBranch.pc_rhs.pexp_loc)
    let after = if isBlockExpr(elseBranch.pc_rhs) {
      let (afterExpr, rest) = partitionAdjacentTrailing(elseBranch.pc_rhs.pexp_loc, after)
      walkExpr(elseBranch.pc_rhs, t, List.concat(list{before, inside, afterExpr}))
      rest
    } else {
      attach(t.leading, elseBranch.pc_rhs.pexp_loc, before)
      walkExpr(elseBranch.pc_rhs, t, inside)
      after
    }
    attach(t.trailing, elseBranch.pc_rhs.pexp_loc, after)

  | Pexp_match(expr, cases) | Pexp_try(expr, cases) =>
    let (before, inside, after) = partitionByLoc(comments, expr.pexp_loc)
    let after = if isBlockExpr(expr) {
      let (afterExpr, rest) = partitionAdjacentTrailing(expr.pexp_loc, after)
      walkExpr(expr, t, List.concat(list{before, inside, afterExpr}))
      rest
    } else {
      attach(t.leading, expr.pexp_loc, before)
      walkExpr(expr, t, inside)
      after
    }
    let (afterExpr, rest) = partitionAdjacentTrailing(expr.pexp_loc, after)
    attach(t.trailing, expr.pexp_loc, afterExpr)
    walkList(~getLoc=n => {
      ...n.Parsetree.pc_lhs.ppat_loc,
      loc_end: n.pc_rhs.pexp_loc.loc_end,
    }, ~walkNode=walkCase, cases, t, rest)
  /* unary expression: todo use parsetreeviewer */
  | Pexp_apply(
      {pexp_desc: Pexp_ident({txt: Longident.Lident("~+" | "~+." | "~-" | "~-." | "not" | "!")})},
      list{(Nolabel, argExpr)},
    ) =>
    let (before, inside, after) = partitionByLoc(comments, argExpr.pexp_loc)
    attach(t.leading, argExpr.pexp_loc, before)
    walkExpr(argExpr, t, inside)
    attach(t.trailing, argExpr.pexp_loc, after)
  /* binary expression */
  | Pexp_apply(
      {
        pexp_desc: Pexp_ident({
          txt: Longident.Lident(
            ":="
            | "||"
            | "&&"
            | "="
            | "=="
            | "<"
            | ">"
            | "!="
            | "!=="
            | "<="
            | ">="
            | "|>"
            | "+"
            | "+."
            | "-"
            | "-."
            | "++"
            | "^"
            | "*"
            | "*."
            | "/"
            | "/."
            | "**"
            | "->"
            | "<>",
          ),
        }),
      },
      list{(Nolabel, operand1), (Nolabel, operand2)},
    ) =>
    let (before, inside, after) = partitionByLoc(comments, operand1.pexp_loc)
    attach(t.leading, operand1.pexp_loc, before)
    walkExpr(operand1, t, inside)
    let (afterOperand1, rest) = partitionAdjacentTrailing(operand1.pexp_loc, after)
    attach(t.trailing, operand1.pexp_loc, afterOperand1)
    let (before, inside, after) = partitionByLoc(rest, operand2.pexp_loc)
    attach(t.leading, operand2.pexp_loc, before)
    walkExpr(operand2, t, inside) /* (List.concat [inside; after]); */
    attach(t.trailing, operand2.pexp_loc, after)
  | Pexp_apply(callExpr, arguments) =>
    let (before, inside, after) = partitionByLoc(comments, callExpr.pexp_loc)
    let after = if isBlockExpr(callExpr) {
      let (afterExpr, rest) = partitionAdjacentTrailing(callExpr.pexp_loc, after)
      walkExpr(callExpr, t, List.concat(list{before, inside, afterExpr}))
      rest
    } else {
      attach(t.leading, callExpr.pexp_loc, before)
      walkExpr(callExpr, t, inside)
      after
    }
    let (afterExpr, rest) = partitionAdjacentTrailing(callExpr.pexp_loc, after)
    attach(t.trailing, callExpr.pexp_loc, afterExpr)
    walkList(~getLoc=((_argLabel, expr)) =>
      switch expr.Parsetree.pexp_attributes {
      | list{({Location.txt: "ns.namedArgLoc", loc}, _), ..._attrs} => {
          ...loc,
          loc_end: expr.pexp_loc.loc_end,
        }
      | _ => expr.pexp_loc
      }
    , ~walkNode=walkExprArgument, arguments, t, rest)
  | Pexp_fun(_, _, _, _) | Pexp_newtype(_) =>
    let (_, parameters, returnExpr) = funExpr(expr)
    let comments = visitListButContinueWithRemainingComments(
      ~newlineDelimited=false,
      ~walkNode=walkExprPararameter,
      ~getLoc=((_attrs, _argLbl, exprOpt, pattern)) => {
        open Parsetree
        let startPos = switch pattern.ppat_attributes {
        | list{({Location.txt: "ns.namedArgLoc", loc}, _), ..._attrs} => loc.loc_start
        | _ => pattern.ppat_loc.loc_start
        }

        switch exprOpt {
        | None => {...pattern.ppat_loc, loc_start: startPos}
        | Some(expr) => {
            ...pattern.ppat_loc,
            loc_start: startPos,
            loc_end: expr.pexp_loc.loc_end,
          }
        }
      },
      parameters,
      t,
      comments,
    )

    switch returnExpr.pexp_desc {
    | Pexp_constraint(expr, typ)
      if expr.pexp_loc.loc_start.pos_cnum >= typ.ptyp_loc.loc_end.pos_cnum =>
      let (leading, inside, trailing) = partitionByLoc(comments, typ.ptyp_loc)
      attach(t.leading, typ.ptyp_loc, leading)
      walkTypExpr(typ, t, inside)
      let (afterTyp, comments) = partitionAdjacentTrailing(typ.ptyp_loc, trailing)
      attach(t.trailing, typ.ptyp_loc, afterTyp)
      if isBlockExpr(expr) {
        walkExpr(expr, t, comments)
      } else {
        let (leading, inside, trailing) = partitionByLoc(comments, expr.pexp_loc)
        attach(t.leading, expr.pexp_loc, leading)
        walkExpr(expr, t, inside)
        attach(t.trailing, expr.pexp_loc, trailing)
      }
    | _ =>
      if isBlockExpr(returnExpr) {
        walkExpr(returnExpr, t, comments)
      } else {
        let (leading, inside, trailing) = partitionByLoc(comments, returnExpr.pexp_loc)
        attach(t.leading, returnExpr.pexp_loc, leading)
        walkExpr(returnExpr, t, inside)
        attach(t.trailing, returnExpr.pexp_loc, trailing)
      }
    }
  | _ => ()
  }
}

and walkExprPararameter = ((_attrs, _argLbl, exprOpt, pattern), t, comments) => {
  let (leading, inside, trailing) = partitionByLoc(comments, pattern.ppat_loc)
  attach(t.leading, pattern.ppat_loc, leading)
  walkPattern(pattern, t, inside)
  switch exprOpt {
  | Some(expr) =>
    let (_afterPat, rest) = partitionAdjacentTrailing(pattern.ppat_loc, trailing)
    attach(t.trailing, pattern.ppat_loc, trailing)
    if isBlockExpr(expr) {
      walkExpr(expr, t, rest)
    } else {
      let (leading, inside, trailing) = partitionByLoc(rest, expr.pexp_loc)
      attach(t.leading, expr.pexp_loc, leading)
      walkExpr(expr, t, inside)
      attach(t.trailing, expr.pexp_loc, trailing)
    }
  | None => attach(t.trailing, pattern.ppat_loc, trailing)
  }
}

and walkExprArgument = ((_argLabel, expr), t, comments) =>
  switch expr.Parsetree.pexp_attributes {
  | list{({Location.txt: "ns.namedArgLoc", loc}, _), ..._attrs} =>
    let (leading, trailing) = partitionLeadingTrailing(comments, loc)
    attach(t.leading, loc, leading)
    let (afterLabel, rest) = partitionAdjacentTrailing(loc, trailing)
    attach(t.trailing, loc, afterLabel)
    let (before, inside, after) = partitionByLoc(rest, expr.pexp_loc)
    attach(t.leading, expr.pexp_loc, before)
    walkExpr(expr, t, inside)
    attach(t.trailing, expr.pexp_loc, after)
  | _ =>
    let (before, inside, after) = partitionByLoc(comments, expr.pexp_loc)
    attach(t.leading, expr.pexp_loc, before)
    walkExpr(expr, t, inside)
    attach(t.trailing, expr.pexp_loc, after)
  }

and walkCase = (case, t, comments) => {
  let (before, inside, after) = partitionByLoc(comments, case.pc_lhs.ppat_loc)
  /* cases don't have a location on their own, leading comments should go
   * after the bar on the pattern */
  walkPattern(case.pc_lhs, t, List.concat(list{before, inside}))
  let (afterPat, rest) = partitionAdjacentTrailing(case.pc_lhs.ppat_loc, after)
  attach(t.trailing, case.pc_lhs.ppat_loc, afterPat)
  let comments = switch case.pc_guard {
  | Some(expr) =>
    let (before, inside, after) = partitionByLoc(rest, expr.pexp_loc)
    let (afterExpr, rest) = partitionAdjacentTrailing(expr.pexp_loc, after)
    if isBlockExpr(expr) {
      walkExpr(expr, t, List.concat(list{before, inside, afterExpr}))
    } else {
      attach(t.leading, expr.pexp_loc, before)
      walkExpr(expr, t, inside)
      attach(t.trailing, expr.pexp_loc, afterExpr)
    }
    rest
  | None => rest
  }

  if isBlockExpr(case.pc_rhs) {
    walkExpr(case.pc_rhs, t, comments)
  } else {
    let (before, inside, after) = partitionByLoc(comments, case.pc_rhs.pexp_loc)
    attach(t.leading, case.pc_rhs.pexp_loc, before)
    walkExpr(case.pc_rhs, t, inside)
    attach(t.trailing, case.pc_rhs.pexp_loc, after)
  }
}

and walkExprRecordRow = ((longident, expr), t, comments) => {
  let (beforeLongident, afterLongident) = partitionLeadingTrailing(comments, longident.loc)

  attach(t.leading, longident.loc, beforeLongident)
  let (afterLongident, rest) = partitionAdjacentTrailing(longident.loc, afterLongident)
  attach(t.trailing, longident.loc, afterLongident)
  let (leading, inside, trailing) = partitionByLoc(rest, expr.pexp_loc)
  attach(t.leading, expr.pexp_loc, leading)
  walkExpr(expr, t, inside)
  attach(t.trailing, expr.pexp_loc, trailing)
}

and walkExtConstr = (extConstr, t, comments) => {
  let (leading, trailing) = partitionLeadingTrailing(comments, extConstr.pext_name.loc)
  attach(t.leading, extConstr.pext_name.loc, leading)
  let (afterName, rest) = partitionAdjacentTrailing(extConstr.pext_name.loc, trailing)
  attach(t.trailing, extConstr.pext_name.loc, afterName)
  walkExtensionConstructorKind(extConstr.pext_kind, t, rest)
}

and walkExtensionConstructorKind = (kind, t, comments) =>
  switch kind {
  | Pext_rebind(longident) =>
    let (leading, trailing) = partitionLeadingTrailing(comments, longident.loc)
    attach(t.leading, longident.loc, leading)
    attach(t.trailing, longident.loc, trailing)
  | Pext_decl(constructorArguments, maybeTypExpr) =>
    let rest = walkConstructorArguments(constructorArguments, t, comments)
    switch maybeTypExpr {
    | None => ()
    | Some(typexpr) =>
      let (before, inside, after) = partitionByLoc(rest, typexpr.ptyp_loc)
      attach(t.leading, typexpr.ptyp_loc, before)
      walkTypExpr(typexpr, t, inside)
      attach(t.trailing, typexpr.ptyp_loc, after)
    }
  }

and walkModExpr = (modExpr, t, comments) =>
  switch modExpr.pmod_desc {
  | Pmod_ident(longident) =>
    let (before, after) = partitionLeadingTrailing(comments, longident.loc)
    attach(t.leading, longident.loc, before)
    attach(t.trailing, longident.loc, after)
  | Pmod_structure(list{}) => attach(t.inside, modExpr.pmod_loc, comments)
  | Pmod_structure(structure) => walkStructure(structure, t, comments)
  | Pmod_extension(extension) => walkExtension(extension, t, comments)
  | Pmod_unpack(expr) =>
    let (before, inside, after) = partitionByLoc(comments, expr.pexp_loc)
    attach(t.leading, expr.pexp_loc, before)
    walkExpr(expr, t, inside)
    attach(t.trailing, expr.pexp_loc, after)
  | Pmod_constraint(modexpr, modtype) =>
    if modtype.pmty_loc.loc_start >= modexpr.pmod_loc.loc_end {
      let (before, inside, after) = partitionByLoc(comments, modexpr.pmod_loc)
      attach(t.leading, modexpr.pmod_loc, before)
      walkModExpr(modexpr, t, inside)
      let (after, rest) = partitionAdjacentTrailing(modexpr.pmod_loc, after)
      attach(t.trailing, modexpr.pmod_loc, after)
      let (before, inside, after) = partitionByLoc(rest, modtype.pmty_loc)
      attach(t.leading, modtype.pmty_loc, before)
      walkModType(modtype, t, inside)
      attach(t.trailing, modtype.pmty_loc, after)
    } else {
      let (before, inside, after) = partitionByLoc(comments, modtype.pmty_loc)
      attach(t.leading, modtype.pmty_loc, before)
      walkModType(modtype, t, inside)
      let (after, rest) = partitionAdjacentTrailing(modtype.pmty_loc, after)
      attach(t.trailing, modtype.pmty_loc, after)
      let (before, inside, after) = partitionByLoc(rest, modexpr.pmod_loc)
      attach(t.leading, modexpr.pmod_loc, before)
      walkModExpr(modexpr, t, inside)
      attach(t.trailing, modexpr.pmod_loc, after)
    }
  | Pmod_apply(_callModExpr, _argModExpr) =>
    let modExprs = modExprApply(modExpr)
    walkList(~getLoc=n => n.Parsetree.pmod_loc, ~walkNode=walkModExpr, modExprs, t, comments)
  | Pmod_functor(_) =>
    let (parameters, returnModExpr) = modExprFunctor(modExpr)
    let comments = visitListButContinueWithRemainingComments(~getLoc=((_, lbl, modTypeOption)) =>
      switch modTypeOption {
      | None => lbl.Asttypes.loc
      | Some(modType) => {...lbl.loc, loc_end: modType.Parsetree.pmty_loc.loc_end}
      }
    , ~walkNode=walkModExprParameter, ~newlineDelimited=false, parameters, t, comments)

    switch returnModExpr.pmod_desc {
    | Pmod_constraint(modExpr, modType)
      if modType.pmty_loc.loc_end.pos_cnum <= modExpr.pmod_loc.loc_start.pos_cnum =>
      let (before, inside, after) = partitionByLoc(comments, modType.pmty_loc)
      attach(t.leading, modType.pmty_loc, before)
      walkModType(modType, t, inside)
      let (after, rest) = partitionAdjacentTrailing(modType.pmty_loc, after)
      attach(t.trailing, modType.pmty_loc, after)
      let (before, inside, after) = partitionByLoc(rest, modExpr.pmod_loc)
      attach(t.leading, modExpr.pmod_loc, before)
      walkModExpr(modExpr, t, inside)
      attach(t.trailing, modExpr.pmod_loc, after)
    | _ =>
      let (before, inside, after) = partitionByLoc(comments, returnModExpr.pmod_loc)
      attach(t.leading, returnModExpr.pmod_loc, before)
      walkModExpr(returnModExpr, t, inside)
      attach(t.trailing, returnModExpr.pmod_loc, after)
    }
  }

and walkModExprParameter = (parameter, t, comments) => {
  let (_attrs, lbl, modTypeOption) = parameter
  let (leading, trailing) = partitionLeadingTrailing(comments, lbl.loc)
  attach(t.leading, lbl.loc, leading)
  switch modTypeOption {
  | None => attach(t.trailing, lbl.loc, trailing)
  | Some(modType) =>
    let (afterLbl, rest) = partitionAdjacentTrailing(lbl.loc, trailing)
    attach(t.trailing, lbl.loc, afterLbl)
    let (before, inside, after) = partitionByLoc(rest, modType.pmty_loc)
    attach(t.leading, modType.pmty_loc, before)
    walkModType(modType, t, inside)
    attach(t.trailing, modType.pmty_loc, after)
  }
}

and walkModType = (modType, t, comments) =>
  switch modType.pmty_desc {
  | Pmty_ident(longident) | Pmty_alias(longident) =>
    let (leading, trailing) = partitionLeadingTrailing(comments, longident.loc)
    attach(t.leading, longident.loc, leading)
    attach(t.trailing, longident.loc, trailing)
  | Pmty_signature(list{}) => attach(t.inside, modType.pmty_loc, comments)
  | Pmty_signature(signature) => walkSignature(signature, t, comments)
  | Pmty_extension(extension) => walkExtension(extension, t, comments)
  | Pmty_typeof(modExpr) =>
    let (before, inside, after) = partitionByLoc(comments, modExpr.pmod_loc)
    attach(t.leading, modExpr.pmod_loc, before)
    walkModExpr(modExpr, t, inside)
    attach(t.trailing, modExpr.pmod_loc, after)
  | Pmty_with(modType, _withConstraints) =>
    let (before, inside, after) = partitionByLoc(comments, modType.pmty_loc)
    attach(t.leading, modType.pmty_loc, before)
    walkModType(modType, t, inside)
    attach(t.trailing, modType.pmty_loc, after)
  /* TODO: withConstraints */
  | Pmty_functor(_) =>
    let (parameters, returnModType) = functorType(modType)
    let comments = visitListButContinueWithRemainingComments(~getLoc=((_, lbl, modTypeOption)) =>
      switch modTypeOption {
      | None => lbl.Asttypes.loc
      | Some(modType) =>
        if lbl.txt == "_" {
          modType.Parsetree.pmty_loc
        } else {
          {...lbl.loc, loc_end: modType.Parsetree.pmty_loc.loc_end}
        }
      }
    , ~walkNode=walkModTypeParameter, ~newlineDelimited=false, parameters, t, comments)

    let (before, inside, after) = partitionByLoc(comments, returnModType.pmty_loc)
    attach(t.leading, returnModType.pmty_loc, before)
    walkModType(returnModType, t, inside)
    attach(t.trailing, returnModType.pmty_loc, after)
  }

and walkModTypeParameter = ((_, lbl, modTypeOption), t, comments) => {
  let (leading, trailing) = partitionLeadingTrailing(comments, lbl.loc)
  attach(t.leading, lbl.loc, leading)
  switch modTypeOption {
  | None => attach(t.trailing, lbl.loc, trailing)
  | Some(modType) =>
    let (afterLbl, rest) = partitionAdjacentTrailing(lbl.loc, trailing)
    attach(t.trailing, lbl.loc, afterLbl)
    let (before, inside, after) = partitionByLoc(rest, modType.pmty_loc)
    attach(t.leading, modType.pmty_loc, before)
    walkModType(modType, t, inside)
    attach(t.trailing, modType.pmty_loc, after)
  }
}

and walkPattern = (pat, t, comments) => {
  open Location
  switch pat.Parsetree.ppat_desc {
  | _ if comments == list{} => ()
  | Ppat_alias(pat, alias) =>
    let (leading, inside, trailing) = partitionByLoc(comments, pat.ppat_loc)
    attach(t.leading, pat.ppat_loc, leading)
    walkPattern(pat, t, inside)
    let (afterPat, rest) = partitionAdjacentTrailing(pat.ppat_loc, trailing)
    attach(t.leading, pat.ppat_loc, leading)
    attach(t.trailing, pat.ppat_loc, afterPat)
    let (beforeAlias, afterAlias) = partitionLeadingTrailing(rest, alias.loc)
    attach(t.leading, alias.loc, beforeAlias)
    attach(t.trailing, alias.loc, afterAlias)
  | Ppat_tuple(list{})
  | Ppat_array(list{})
  | Ppat_construct({txt: Longident.Lident("()")}, _)
  | Ppat_construct({txt: Longident.Lident("[]")}, _) =>
    attach(t.inside, pat.ppat_loc, comments)
  | Ppat_array(patterns) =>
    walkList(~getLoc=n => n.Parsetree.ppat_loc, ~walkNode=walkPattern, patterns, t, comments)
  | Ppat_tuple(patterns) =>
    walkList(~getLoc=n => n.Parsetree.ppat_loc, ~walkNode=walkPattern, patterns, t, comments)
  | Ppat_construct({txt: Longident.Lident("::")}, _) =>
    walkList(
      ~getLoc=n => n.Parsetree.ppat_loc,
      ~walkNode=walkPattern,
      collectListPatterns(list{}, pat),
      t,
      comments,
    )
  | Ppat_construct(constr, None) =>
    let (beforeConstr, afterConstr) = partitionLeadingTrailing(comments, constr.loc)

    attach(t.leading, constr.loc, beforeConstr)
    attach(t.trailing, constr.loc, afterConstr)
  | Ppat_construct(constr, Some(pat)) =>
    let (leading, trailing) = partitionLeadingTrailing(comments, constr.loc)
    attach(t.leading, constr.loc, leading)
    let (afterConstructor, rest) = partitionAdjacentTrailing(constr.loc, trailing)

    attach(t.trailing, constr.loc, afterConstructor)
    let (leading, inside, trailing) = partitionByLoc(rest, pat.ppat_loc)
    attach(t.leading, pat.ppat_loc, leading)
    walkPattern(pat, t, inside)
    attach(t.trailing, pat.ppat_loc, trailing)
  | Ppat_variant(_label, None) => ()
  | Ppat_variant(_label, Some(pat)) => walkPattern(pat, t, comments)
  | Ppat_type(_) => ()
  | Ppat_record(recordRows, _) =>
    walkList(~getLoc=((longidentLoc, pattern): (Asttypes.loc<Longident.t>, Parsetree.pattern)) => {
      ...longidentLoc.loc,
      loc_end: pattern.Parsetree.ppat_loc.loc_end,
    }, ~walkNode=walkPatternRecordRow, recordRows, t, comments)
  | Ppat_or(_) =>
    walkList(
      ~getLoc=pattern => pattern.Parsetree.ppat_loc,
      ~walkNode=pattern => walkPattern(pattern),
      Res_parsetree_viewer.collectOrPatternChain(pat),
      t,
      comments,
    )
  | Ppat_constraint(pattern, typ) =>
    let (beforePattern, insidePattern, afterPattern) = partitionByLoc(comments, pattern.ppat_loc)

    attach(t.leading, pattern.ppat_loc, beforePattern)
    walkPattern(pattern, t, insidePattern)
    let (afterPattern, rest) = partitionAdjacentTrailing(pattern.ppat_loc, afterPattern)

    attach(t.trailing, pattern.ppat_loc, afterPattern)
    let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(rest, typ.ptyp_loc)

    attach(t.leading, typ.ptyp_loc, beforeTyp)
    walkTypExpr(typ, t, insideTyp)
    attach(t.trailing, typ.ptyp_loc, afterTyp)
  | Ppat_lazy(pattern) | Ppat_exception(pattern) =>
    let (leading, inside, trailing) = partitionByLoc(comments, pattern.ppat_loc)
    attach(t.leading, pattern.ppat_loc, leading)
    walkPattern(pattern, t, inside)
    attach(t.trailing, pattern.ppat_loc, trailing)
  | Ppat_unpack(stringLoc) =>
    let (leading, trailing) = partitionLeadingTrailing(comments, stringLoc.loc)
    attach(t.leading, stringLoc.loc, leading)
    attach(t.trailing, stringLoc.loc, trailing)
  | Ppat_extension(extension) => walkExtension(extension, t, comments)
  | _ => ()
  }
}

/* name: firstName */
and walkPatternRecordRow = (row, t, comments) =>
  switch row {
  /* punned {x} */
  | (
      {Location.txt: Longident.Lident(ident), loc: longidentLoc},
      {Parsetree.ppat_desc: Ppat_var({txt, _})},
    ) if ident == txt =>
    let (beforeLbl, afterLbl) = partitionLeadingTrailing(comments, longidentLoc)

    attach(t.leading, longidentLoc, beforeLbl)
    attach(t.trailing, longidentLoc, afterLbl)
  | (longident, pattern) =>
    let (beforeLbl, afterLbl) = partitionLeadingTrailing(comments, longident.loc)

    attach(t.leading, longident.loc, beforeLbl)
    let (afterLbl, rest) = partitionAdjacentTrailing(longident.loc, afterLbl)
    attach(t.trailing, longident.loc, afterLbl)
    let (leading, inside, trailing) = partitionByLoc(rest, pattern.ppat_loc)
    attach(t.leading, pattern.ppat_loc, leading)
    walkPattern(pattern, t, inside)
    attach(t.trailing, pattern.ppat_loc, trailing)
  }

and walkTypExpr = (typ, t, comments) =>
  switch typ.Parsetree.ptyp_desc {
  | _ if comments == list{} => ()
  | Ptyp_tuple(typexprs) =>
    walkList(~getLoc=n => n.Parsetree.ptyp_loc, ~walkNode=walkTypExpr, typexprs, t, comments)
  | Ptyp_extension(extension) => walkExtension(extension, t, comments)
  | Ptyp_package(packageType) => walkPackageType(packageType, t, comments)
  | Ptyp_alias(typexpr, _alias) =>
    let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(comments, typexpr.ptyp_loc)
    attach(t.leading, typexpr.ptyp_loc, beforeTyp)
    walkTypExpr(typexpr, t, insideTyp)
    attach(t.trailing, typexpr.ptyp_loc, afterTyp)
  | Ptyp_poly(strings, typexpr) =>
    let comments = visitListButContinueWithRemainingComments(
      ~getLoc=n => n.Asttypes.loc,
      ~walkNode=(longident, t, comments) => {
        let (beforeLongident, afterLongident) = partitionLeadingTrailing(comments, longident.loc)
        attach(t.leading, longident.loc, beforeLongident)
        attach(t.trailing, longident.loc, afterLongident)
      },
      ~newlineDelimited=false,
      strings,
      t,
      comments,
    )

    let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(comments, typexpr.ptyp_loc)
    attach(t.leading, typexpr.ptyp_loc, beforeTyp)
    walkTypExpr(typexpr, t, insideTyp)
    attach(t.trailing, typexpr.ptyp_loc, afterTyp)
  | Ptyp_constr(longident, typexprs) =>
    let (beforeLongident, _afterLongident) = partitionLeadingTrailing(comments, longident.loc)
    let (afterLongident, rest) = partitionAdjacentTrailing(longident.loc, comments)
    attach(t.leading, longident.loc, beforeLongident)
    attach(t.trailing, longident.loc, afterLongident)
    walkList(~getLoc=n => n.Parsetree.ptyp_loc, ~walkNode=walkTypExpr, typexprs, t, rest)
  | Ptyp_arrow(_) =>
    let (_, parameters, typexpr) = arrowType(typ)
    let comments = walkTypeParameters(parameters, t, comments)
    let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(comments, typexpr.ptyp_loc)
    attach(t.leading, typexpr.ptyp_loc, beforeTyp)
    walkTypExpr(typexpr, t, insideTyp)
    attach(t.trailing, typexpr.ptyp_loc, afterTyp)
  | Ptyp_object(fields, _) => walkTypObjectFields(fields, t, comments)
  | _ => ()
  }

and walkTypObjectFields = (fields, t, comments) => walkList(~getLoc=field =>
    switch field {
    | Parsetree.Otag(lbl, _, typ) => {...lbl.loc, loc_end: typ.ptyp_loc.loc_end}
    | _ => Location.none
    }
  , ~walkNode=walkTypObjectField, fields, t, comments)

and walkTypObjectField = (field, t, comments) =>
  switch field {
  | Otag(lbl, _, typexpr) =>
    let (beforeLbl, afterLbl) = partitionLeadingTrailing(comments, lbl.loc)
    attach(t.leading, lbl.loc, beforeLbl)
    let (afterLbl, rest) = partitionAdjacentTrailing(lbl.loc, afterLbl)
    attach(t.trailing, lbl.loc, afterLbl)
    let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(rest, typexpr.ptyp_loc)
    attach(t.leading, typexpr.ptyp_loc, beforeTyp)
    walkTypExpr(typexpr, t, insideTyp)
    attach(t.trailing, typexpr.ptyp_loc, afterTyp)
  | _ => ()
  }

and walkTypeParameters = (typeParameters, t, comments) =>
  visitListButContinueWithRemainingComments(~getLoc=((_, _, typexpr)) =>
    switch typexpr.Parsetree.ptyp_attributes {
    | list{({Location.txt: "ns.namedArgLoc", loc}, _), ..._attrs} => {
        ...loc,
        loc_end: typexpr.ptyp_loc.loc_end,
      }
    | _ => typexpr.ptyp_loc
    }
  , ~walkNode=walkTypeParameter, ~newlineDelimited=false, typeParameters, t, comments)

and walkTypeParameter = ((_attrs, _lbl, typexpr), t, comments) => {
  let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(comments, typexpr.ptyp_loc)
  attach(t.leading, typexpr.ptyp_loc, beforeTyp)
  walkTypExpr(typexpr, t, insideTyp)
  attach(t.trailing, typexpr.ptyp_loc, afterTyp)
}

and walkPackageType = (packageType, t, comments) => {
  let (longident, packageConstraints) = packageType
  let (beforeLongident, afterLongident) = partitionLeadingTrailing(comments, longident.loc)
  attach(t.leading, longident.loc, beforeLongident)
  let (afterLongident, rest) = partitionAdjacentTrailing(longident.loc, afterLongident)
  attach(t.trailing, longident.loc, afterLongident)
  walkPackageConstraints(packageConstraints, t, rest)
}

and walkPackageConstraints = (packageConstraints, t, comments) =>
  walkList(~getLoc=((longident, typexpr)) => {
    ...longident.Asttypes.loc,
    loc_end: typexpr.Parsetree.ptyp_loc.loc_end,
  }, ~walkNode=walkPackageConstraint, packageConstraints, t, comments)

and walkPackageConstraint = (packageConstraint, t, comments) => {
  let (longident, typexpr) = packageConstraint
  let (beforeLongident, afterLongident) = partitionLeadingTrailing(comments, longident.loc)
  attach(t.leading, longident.loc, beforeLongident)
  let (afterLongident, rest) = partitionAdjacentTrailing(longident.loc, afterLongident)
  attach(t.trailing, longident.loc, afterLongident)
  let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(rest, typexpr.ptyp_loc)
  attach(t.leading, typexpr.ptyp_loc, beforeTyp)
  walkTypExpr(typexpr, t, insideTyp)
  attach(t.trailing, typexpr.ptyp_loc, afterTyp)
}

and walkExtension = (extension, t, comments) => {
  let (id, payload) = extension
  let (beforeId, afterId) = partitionLeadingTrailing(comments, id.loc)
  attach(t.leading, id.loc, beforeId)
  let (afterId, rest) = partitionAdjacentTrailing(id.loc, afterId)
  attach(t.trailing, id.loc, afterId)
  walkPayload(payload, t, rest)
}

and walkAttribute = ((id, payload), t, comments) => {
  let (beforeId, afterId) = partitionLeadingTrailing(comments, id.loc)
  attach(t.leading, id.loc, beforeId)
  let (afterId, rest) = partitionAdjacentTrailing(id.loc, afterId)
  attach(t.trailing, id.loc, afterId)
  walkPayload(payload, t, rest)
}

and walkPayload = (payload, t, comments) =>
  switch payload {
  | PStr(s) => walkStructure(s, t, comments)
  | _ => ()
  }

