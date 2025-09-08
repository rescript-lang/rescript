/* AST for js externals */
type scope =
  | Global
  | Module(string) /* bs.module("path") */
  | Scope(Longident.t) /* bs.scope(/"window", "location"/) */

type label_declaration = {
  @live jld_attributes: Parsetree.attributes,
  jld_name: string,
  jld_alias: string,
  jld_type: Parsetree.core_type,
  jld_loc: Location.t,
}

type importSpec =
  | Default(label_declaration)
  | Spec(list<label_declaration>)

type import_description = {
  jid_loc: Location.t,
  jid_spec: importSpec,
  jid_scope: scope,
  jid_attributes: Parsetree.attributes,
}

let decl = (~attrs, ~loc, ~name, ~alias, ~typ) => {
  jld_loc: loc,
  jld_attributes: attrs,
  jld_name: name,
  jld_alias: alias,
  jld_type: typ,
}

let importDescr = (~attrs, ~scope, ~importSpec, ~loc) => {
  jid_loc: loc,
  jid_spec: importSpec,
  jid_scope: scope,
  jid_attributes: attrs,
}

let toParsetree = importDescr => {
  let bsVal = (Location.mknoloc("val"), Parsetree.PStr(list{}))
  let attrs = switch importDescr.jid_scope {
  | Global => list{bsVal}
  /* @genType.import("./MyMath"),
   * @genType.import(/"./MyMath", "default"/) */
  | Module(s) =>
    let structure = list{
      Parsetree.Pconst_string(s, None) |> Ast_helper.Exp.constant |> Ast_helper.Str.eval,
    }
    let genType = (Location.mknoloc("genType.import"), Parsetree.PStr(structure))
    list{genType}
  | Scope(longident) =>
    let structureItem = {
      let expr = switch Longident.flatten(longident) |> List.map(s =>
        Ast_helper.Exp.constant(Parsetree.Pconst_string(s, None))
      ) {
      | list{expr} => expr
      | list{} as exprs | _ as exprs => exprs |> Ast_helper.Exp.tuple
      }

      Ast_helper.Str.eval(expr)
    }

    let bsScope = (Location.mknoloc("scope"), Parsetree.PStr(list{structureItem}))
    list{bsVal, bsScope}
  }

  let valueDescrs = switch importDescr.jid_spec {
  | Default(decl) =>
    let prim = list{decl.jld_name}
    let allAttrs = List.concat(list{attrs, importDescr.jid_attributes}) |> List.map(attr =>
      switch attr {
      | (
          {Location.txt: "genType.import"} as id,
          Parsetree.PStr(list{{pstr_desc: Parsetree.Pstr_eval(moduleName, _)}}),
        ) =>
        let default = Parsetree.Pconst_string("default", None) |> Ast_helper.Exp.constant

        let structureItem = list{moduleName, default} |> Ast_helper.Exp.tuple |> Ast_helper.Str.eval

        (id, Parsetree.PStr(list{structureItem}))
      | attr => attr
      }
    )

    list{
      Ast_helper.Val.mk(
        ~loc=importDescr.jid_loc,
        ~prim,
        ~attrs=allAttrs,
        Location.mknoloc(decl.jld_alias),
        decl.jld_type,
      ) |> Ast_helper.Str.primitive,
    }
  | Spec(decls) => List.map(decl => {
      let prim = list{decl.jld_name}
      let allAttrs = List.concat(list{attrs, decl.jld_attributes})
      Ast_helper.Val.mk(
        ~loc=importDescr.jid_loc,
        ~prim,
        ~attrs=allAttrs,
        Location.mknoloc(decl.jld_alias),
        decl.jld_type,
      ) |> Ast_helper.Str.primitive(~loc=decl.jld_loc)
    }, decls)
  }

  let jsFfiAttr = (Location.mknoloc("ns.jsFfi"), Parsetree.PStr(list{}))
  Ast_helper.Mod.structure(~loc=importDescr.jid_loc, valueDescrs)
  |> Ast_helper.Incl.mk(~attrs=list{jsFfiAttr}, ~loc=importDescr.jid_loc)
  |> Ast_helper.Str.include_(~loc=importDescr.jid_loc)
}

