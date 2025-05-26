type \"import" = {
  name: string,
  importPath: ImportPath.t,
}

type rec attributePayload =
  | BoolPayload(bool)
  | FloatPayload(string)
  | IdentPayload(Longident.t)
  | IntPayload(string)
  | StringPayload(string)
  | TuplePayload(list<attributePayload>)
  | UnrecognizedPayload

type t =
  | GenType
  | GenTypeOpaque
  | NoGenType

let toString = annotation =>
  switch annotation {
  | GenType => "GenType"
  | GenTypeOpaque => "GenTypeOpaque"
  | NoGenType => "NoGenType"
  }

let tagIsGenType = s => s == "genType" || s == "gentype"
let tagIsGenTypeAs = s => s == "genType.as" || s == "gentype.as"

let tagIsAs = s => s == "as"

let tagIsUnboxed = s => s == "unboxed" || s == "ocaml.unboxed"

let tagIsGenTypeImport = s => s == "genType.import" || s == "gentype.import"

let tagIsGenTypeOpaque = s => s == "genType.opaque" || s == "gentype.opaque"

let tagIsOneOfTheGenTypeAnnotations = s =>
  tagIsGenType(s) || (tagIsGenTypeImport(s) || tagIsGenTypeOpaque(s))

let tagIsGenTypeIgnoreInterface = s =>
  s == "genType.ignoreInterface" || s == "gentype.ignoreInterface"

let tagIsOcamlDoc = s => s == "ocaml.doc"

let tagIsInternLocal = s => s == "internal.local"

let rec getAttributePayload = (checkText, attributes: Typedtree.attributes) => {
  let rec fromExpr = (expr: Parsetree.expression) =>
    switch expr {
    | {pexp_desc: Pexp_constant(Pconst_string(s, _))} => Some(StringPayload(s))
    | {pexp_desc: Pexp_constant(Pconst_integer(n, _))} => Some(IntPayload(n))
    | {pexp_desc: Pexp_constant(Pconst_float(s, _))} => Some(FloatPayload(s))
    | {pexp_desc: Pexp_construct({txt: Lident(("true" | "false") as s)}, _), _} =>
      Some(BoolPayload(s == "true"))
    | {pexp_desc: Pexp_tuple(exprs)} =>
      let payloads = exprs->List.rev->List.fold_left((payloads, expr) =>
          switch expr->fromExpr {
          | Some(payload) => list{payload, ...payloads}
          | None => payloads
          }
        , list{})
      Some(TuplePayload(payloads))
    | {pexp_desc: Pexp_ident({txt})} => Some(IdentPayload(txt))
    | _ => None
    }
  switch attributes {
  | list{} => None
  | list{({Asttypes.txt: txt}, payload), ..._tl} if checkText(txt) =>
    switch payload {
    | PStr(list{}) => Some(UnrecognizedPayload)
    | PStr(list{{pstr_desc: Pstr_eval(expr, _)}, ..._}) => expr->fromExpr
    | PStr(list{{pstr_desc: Pstr_extension(_)}, ..._}) => Some(UnrecognizedPayload)
    | PStr(list{{pstr_desc: Pstr_value(_)}, ..._}) => Some(UnrecognizedPayload)
    | PStr(list{{pstr_desc: Pstr_primitive(_)}, ..._}) => Some(UnrecognizedPayload)
    | PStr(list{{pstr_desc: Pstr_type(_)}, ..._}) => Some(UnrecognizedPayload)
    | PStr(list{{pstr_desc: Pstr_typext(_)}, ..._}) => Some(UnrecognizedPayload)
    | PStr(list{{pstr_desc: Pstr_exception(_)}, ..._}) => Some(UnrecognizedPayload)
    | PStr(list{{pstr_desc: Pstr_module(_)}, ..._}) => Some(UnrecognizedPayload)
    | PStr(list{{pstr_desc: Pstr_recmodule(_)}, ..._}) => Some(UnrecognizedPayload)
    | PStr(list{{pstr_desc: Pstr_modtype(_)}, ..._}) => Some(UnrecognizedPayload)
    | PStr(list{{pstr_desc: Pstr_open(_)}, ..._}) => Some(UnrecognizedPayload)
    | PStr(list{{pstr_desc: Pstr_class(_)}, ..._}) => Some(UnrecognizedPayload)
    | PStr(list{{pstr_desc: Pstr_class_type(_)}, ..._}) => Some(UnrecognizedPayload)
    | PStr(list{{pstr_desc: Pstr_include(_)}, ..._}) => Some(UnrecognizedPayload)
    | PStr(list{{pstr_desc: Pstr_attribute(_)}, ..._}) => Some(UnrecognizedPayload)
    | PPat(_) => Some(UnrecognizedPayload)
    | PSig(_) => Some(UnrecognizedPayload)
    | PTyp(_) => Some(UnrecognizedPayload)
    }
  | list{_hd, ...tl} => getAttributePayload(checkText, tl)
  }
}

let getGenTypeAsRenaming = attributes =>
  switch attributes->getAttributePayload(tagIsGenTypeAs) {
  | Some(StringPayload(s)) => Some(s)
  | None =>
    switch attributes->getAttributePayload(tagIsGenType) {
    | Some(StringPayload(s)) => Some(s)
    | _ => None
    }
  | _ => None
  }

let getAsRenaming = attributes =>
  switch attributes->getAttributePayload(tagIsAs) {
  | Some(StringPayload(s)) => Some(s)
  | _ => None
  }

let getAttributeImportRenaming = attributes => {
  let attributeImport = attributes->getAttributePayload(tagIsGenTypeImport)
  let genTypeAsRenaming = attributes->getGenTypeAsRenaming
  switch (attributeImport, genTypeAsRenaming) {
  | (Some(StringPayload(importString)), _) => (Some(importString), genTypeAsRenaming)
  | (Some(TuplePayload(list{StringPayload(importString), StringPayload(renameString)})), _) => (
      Some(importString),
      Some(renameString),
    )
  | _ => (None, genTypeAsRenaming)
  }
}

let getDocString = attributes => {
  let docPayload = attributes->getAttributePayload(tagIsOcamlDoc)
  switch docPayload {
  | Some(StringPayload(docString)) => "/** " ++ (docString ++ " */\n")
  | _ => ""
  }
}

let hasAttribute = (checkText, attributes: Typedtree.attributes) =>
  getAttributePayload(checkText, attributes) != None

let fromAttributes = (attributes: Typedtree.attributes) =>
  if hasAttribute(tagIsGenType, attributes) {
    GenType
  } else if hasAttribute(tagIsGenTypeOpaque, attributes) {
    GenTypeOpaque
  } else {
    NoGenType
  }

let rec moduleTypeCheckAnnotation = (~checkAnnotation, {mty_desc}: Typedtree.module_type) =>
  switch mty_desc {
  | Tmty_signature(signature) => signature->signatureCheckAnnotation(~checkAnnotation)
  | Tmty_ident(_)
  | Tmty_functor(_)
  | Tmty_with(_)
  | Tmty_typeof(_)
  | Tmty_alias(_) => false
  }
and moduleDeclarationCheckAnnotation = (
  ~checkAnnotation,
  {md_attributes, md_type, md_loc: loc}: Typedtree.module_declaration,
) =>
  md_attributes->checkAnnotation(~loc) || md_type->moduleTypeCheckAnnotation(~checkAnnotation)
and signatureItemCheckAnnotation = (~checkAnnotation, signatureItem: Typedtree.signature_item) =>
  switch signatureItem {
  | {Typedtree.sig_desc: Typedtree.Tsig_type(_, typeDeclarations)} =>
    typeDeclarations->List.exists(({typ_attributes, typ_loc: loc}: Typedtree.type_declaration) =>
      typ_attributes->checkAnnotation(~loc)
    )
  | {sig_desc: Tsig_value({val_attributes, val_loc: loc})} =>
    val_attributes->checkAnnotation(~loc)
  | {sig_desc: Tsig_module(moduleDeclaration)} =>
    moduleDeclaration->moduleDeclarationCheckAnnotation(~checkAnnotation)
  | {sig_desc: Tsig_attribute(attribute), sig_loc: loc} => list{attribute}->checkAnnotation(~loc)
  | _ => false
  }
and signatureCheckAnnotation = (~checkAnnotation, signature: Typedtree.signature) =>
  signature.sig_items->List.exists(signatureItemCheckAnnotation(~checkAnnotation))

let rec structureItemCheckAnnotation = (
  ~checkAnnotation,
  structureItem: Typedtree.structure_item,
) =>
  switch structureItem {
  | {Typedtree.str_desc: Typedtree.Tstr_type(_, typeDeclarations)} =>
    typeDeclarations->List.exists(({typ_attributes, typ_loc: loc}: Typedtree.type_declaration) =>
      typ_attributes->checkAnnotation(~loc)
    )
  | {str_desc: Tstr_value(_loc, valueBindings)} =>
    valueBindings->List.exists(({vb_attributes, vb_loc: loc}: Typedtree.value_binding) =>
      vb_attributes->checkAnnotation(~loc)
    )
  | {str_desc: Tstr_primitive({val_attributes, val_loc: loc})} =>
    val_attributes->checkAnnotation(~loc)
  | {str_desc: Tstr_module(moduleBinding)} =>
    moduleBinding->moduleBindingCheckAnnotation(~checkAnnotation)
  | {str_desc: Tstr_recmodule(moduleBindings)} =>
    moduleBindings->List.exists(moduleBindingCheckAnnotation(~checkAnnotation))
  | {str_desc: Tstr_include({incl_attributes, incl_mod, incl_loc: loc})} =>
    incl_attributes->checkAnnotation(~loc) ||
      incl_mod->moduleExprCheckAnnotation(~checkAnnotation)
  | _ => false
  }
and moduleExprCheckAnnotation = (~checkAnnotation, moduleExpr: Typedtree.module_expr) =>
  switch moduleExpr.mod_desc {
  | Tmod_structure(structure)
  | Tmod_constraint({mod_desc: Tmod_structure(structure)}, _, _, _) =>
    structure->structureCheckAnnotation(~checkAnnotation)
  | Tmod_constraint(_)
  | Tmod_ident(_)
  | Tmod_functor(_)
  | Tmod_apply(_)
  | Tmod_unpack(_) => false
  }
and moduleBindingCheckAnnotation = (
  ~checkAnnotation,
  {mb_expr, mb_attributes, mb_loc: loc}: Typedtree.module_binding,
) =>
  mb_attributes->checkAnnotation(~loc) || mb_expr->moduleExprCheckAnnotation(~checkAnnotation)
and structureCheckAnnotation = (~checkAnnotation, structure: Typedtree.structure) =>
  structure.str_items->List.exists(structureItemCheckAnnotation(~checkAnnotation))

let sanitizeVariableName = name => name->Str.global_replace(Str.regexp("-"), "_")

let importFromString = (importString): \"import" => {
  let name = {
    let base = importString->Filename.basename
    try base->Filename.chop_extension catch {
    | Invalid_argument(_) => base
    }->sanitizeVariableName
  }
  let importPath = ImportPath.fromStringUnsafe(importString)
  {name: name, importPath: importPath}
}
