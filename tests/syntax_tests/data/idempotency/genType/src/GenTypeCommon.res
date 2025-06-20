/* **
 * Copyright 2004-present Facebook. All Rights Reserved.
 */

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

module Config = Config_
include Config

let logNotImplemented = x =>
  if Debug.notImplemented.contents {
    Log_.item("Not Implemented: %s\n", x)
  }

type optional =
  | Mandatory
  | Optional

type mutable_ =
  | Immutable
  | Mutable

type labelJS =
  | BoolLabel(bool)
  | FloatLabel(string)
  | IntLabel(string)
  | StringLabel(string)

let labelJSToString = (~alwaysQuotes=false, labelJS) => {
  let addQuotes = x => alwaysQuotes ? x->EmitText.quotes : x
  switch labelJS {
  | BoolLabel(b) => b->string_of_bool->addQuotes
  | FloatLabel(s) => s->addQuotes
  | IntLabel(i) => i->addQuotes
  | StringLabel(s) => s->EmitText.quotes
  }
}

type case = {
  label: string,
  labelJS: labelJS,
}

type closedFlag =
  | Open
  | Closed

type rec type_ =
  | Array(type_, mutable_)
  | Function(function_)
  | GroupOfLabeledArgs(fields)
  | Ident(ident)
  | Null(type_)
  | Nullable(type_)
  | Object(closedFlag, fields)
  | Option(type_)
  | Promise(type_)
  | Record(fields)
  | Tuple(list<type_>)
  | TypeVar(string)
  | Variant(variant)
and fields = list<field>
and argType = {
  aName: string,
  aType: type_,
}
and field = {
  mutable_: mutable_,
  nameJS: string,
  nameRE: string,
  optional: optional,
  type_: type_,
}
and function_ = {
  argTypes: list<argType>,
  componentName: option<string>,
  retType: type_,
  typeVars: list<string>,
  uncurried: bool,
}
and ident = {
  builtin: bool,
  name: string,
  typeArgs: list<type_>,
}
and variant = {
  hash: int,
  noPayloads: list<case>,
  payloads: list<(case, int, type_)>,
  polymorphic: bool,
  unboxed: bool,
}

let typeIsObject = type_ =>
  switch type_ {
  | Array(_) => true
  | Function(_) => false
  | GroupOfLabeledArgs(_) => false
  | Ident(_) => false
  | Null(_) => false
  | Nullable(_) => false
  | Object(_) => true
  | Option(_) => false
  | Promise(_) => true
  | Record(_) => true
  | Tuple(_) => true
  | TypeVar(_) => false
  | Variant(_) => false
  }

type label =
  | Nolabel
  | Label(string)
  | OptLabel(string)

type rec dep =
  | External(string)
  | Internal(ResolvedName.t)
  | Dot(dep, string)

module ScopedPackage = {
  // Taken from ext_namespace.ml in bukclescript
  let namespace_of_package_name = (s: string): string => {
    let len = String.length(s)
    let buf = Buffer.create(len)
    let add = (capital, ch) =>
      Buffer.add_char(
        buf,
        if capital {
          Char.uppercase_ascii(ch)
        } else {
          ch
        },
      )
    let rec aux = (capital, off, len) =>
      if off >= len {
        ()
      } else {
        let ch = String.unsafe_get(s, off)
        switch ch {
        | 'a' .. 'z'
        | 'A' .. 'Z'
        | '0' .. '9' =>
          add(capital, ch)
          aux(false, off + 1, len)
        | '/'
        | '-' =>
          aux(true, off + 1, len)
        | _ => aux(capital, off + 1, len)
        }
      }

    aux(true, 0, len)
    Buffer.contents(buf)
  }

  // @demo/some-library -> DemoSomelibrary
  let packageNameToGeneratedModuleName = packageName =>
    if String.contains(packageName, '/') {
      Some(packageName->namespace_of_package_name)
    } else {
      None
    }

  let isGeneratedModule = (id, ~config) =>
    config.bsDependencies->List.exists(packageName =>
      packageName->packageNameToGeneratedModuleName == Some(id->Ident.name)
    )

  // (Common, DemoSomelibrary) -> Common-DemoSomelibrary
  let addGeneratedModule = (s, ~generatedModule) => s ++ ("-" ++ Ident.name(generatedModule))

  // Common-DemoSomelibrary -> Common
  let removeGeneratedModule = s =>
    switch s->String.split_on_char('-') {
    | list{name, _scope} => name
    | _ => s
    }
}

let rec depToString = dep =>
  switch dep {
  | External(name) => name->ScopedPackage.removeGeneratedModule
  | Internal(resolvedName) => resolvedName->ResolvedName.toString
  | Dot(d, s) => depToString(d) ++ ("_" ++ s)
  }

let rec depToResolvedName = (dep: dep) =>
  switch dep {
  | External(name) => name->ResolvedName.fromString
  | Internal(resolvedName) => resolvedName
  | Dot(p, s) => ResolvedName.dot(s, p->depToResolvedName)
  }

let createVariant = (~noPayloads, ~payloads, ~polymorphic) => {
  let hash =
    noPayloads->List.map(case => (case.label, case.labelJS))->Array.of_list->Hashtbl.hash

  let unboxed = payloads == list{}
  Variant({
    hash: hash,
    noPayloads: noPayloads,
    payloads: payloads,
    polymorphic: polymorphic,
    unboxed: unboxed,
  })
}

let variantTable = (hash, ~toJS) => (toJS ? "$$toJS" : "$$toRE") ++ string_of_int(hash)

let ident = (~builtin=true, ~typeArgs=list{}, name) => Ident({
  builtin: builtin,
  name: name,
  typeArgs: typeArgs,
})

let mixedOrUnknown = (~config) =>
  ident(
    switch config.language {
    | Flow => "mixed"
    | TypeScript
    | Untyped => "unknown"
    },
  )

let booleanT = ident("boolean")
let dateT = ident("Date")
let numberT = ident("number")
let stringT = ident("string")
let unitT = ident("void")
let int64T = Tuple(list{numberT, numberT})

module NodeFilename = {
  include Filename

  /* Force "/" separator. */
  let dirSep = "/"

  module Path: {
    type t
    let normalize: string => t
    let concat: (t, string) => t
    let toString: t => string
  } = {
    type t = string

    let normalize = (path): t =>
      switch Sys.os_type {
      | "Win32" => path->Str.split(Str.regexp("\\"))->String.concat(dirSep)
      | _ => path
      }

    let toString = path => path
    let length = path => String.length(path)

    let concat = (dirname, filename) => {
      let isDirSep = (s, i) => {
        let c = String.get(s, i)
        c == '/' || (c == '\\' || c == ':')
      }
      let l = length(dirname)
      if l == 0 || isDirSep(dirname, l - 1) {
        dirname ++ filename
      } else {
        dirname ++ (dirSep ++ filename)
      }
    }
  }

  let concat = (dirname: string, filename) => {
    open Path
    Path.concat(normalize(dirname), filename)->toString
  }
}
