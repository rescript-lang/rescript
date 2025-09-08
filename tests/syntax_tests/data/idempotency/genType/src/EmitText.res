type nameGen = Hashtbl.t<string, int>

let name = (~nameGen, s) =>
  switch Hashtbl.find(nameGen, s) {
  | n =>
    Hashtbl.replace(nameGen, s, n + 1)
    s ++ string_of_int(n + 1)
  | exception Not_found =>
    Hashtbl.replace(nameGen, s, 0)
    s
  }

let parens = xs => "(" ++ ((xs->String.concat(", ")) ++ ")")

let arg = (~nameGen, x) => "Arg" ++ x->name(~nameGen)

let argi = (~nameGen, i) => "Arg" ++ (i->string_of_int)->name(~nameGen)

let array = xs => "[" ++ ((xs->String.concat(", ")) ++ "]")

let comment = x => "/* " ++ (x ++ " */")

let curry = (~args, ~numArgs, name) =>
  switch numArgs {
  | 0
  | 1 =>
    name ++ parens(args)
  | (2 | 3 | 4 | 5 | 6 | 7 | 8) as n =>
    "Curry._" ++ ((n->string_of_int) ++ parens(\"@"(list{name}, args)))
  | _ => "Curry.app" ++ parens(list{name, args->array})
  }

let funCall = (~args, ~useCurry=false, name) =>
  useCurry ? name->curry(~args, ~numArgs=args->List.length) : name ++ parens(args)

let genericsString = (~typeVars) =>
  typeVars === list{} ? "" : "<" ++ (String.concat(",", typeVars) ++ ">")

let funDef = (~bodyArgs, ~functionName, ~funParams, ~indent, ~mkBody, ~typeVars) =>
  "function " ++
  (switch functionName {
  | None => ""
  | Some(name) => name
  } ++
  (genericsString(~typeVars) ++
  ((funParams->parens) ++
  (" {" ++ ((bodyArgs->mkBody) ++ (Indent.break(~indent) ++ "}"))))))

let ifThenElse = (~indent, if_, then_, else_) => {
  let indent1 = indent->Indent.more
  if_(~indent=indent1) ++
  (Indent.break(~indent) ++
  ("? " ++ (then_(~indent=indent1) ++ (Indent.break(~indent) ++ (": " ++ else_(~indent=indent1))))))
}

let newNameGen = () => Hashtbl.create(1)

let quotes = x => "\"" ++ (x ++ "\"")

let resultName = (~nameGen) => "result"->name(~nameGen)

let \"switch" = (~indent, ~cases, expr) => {
  let lastCase = (cases->List.length) - 1

  cases
  ->List.mapi((i, (label, code)) =>
    if i == lastCase {
      code
    } else {
      expr ++
      ("===" ++
      (label ++ (Indent.break(~indent) ++ ("? " ++ (code ++ (Indent.break(~indent) ++ ": "))))))
    }
  )
  ->String.concat("")
}

let typeOfObject = x => "typeof(" ++ (x ++ (")" ++ (" === " ++ "\'object\'")))

let addComment = (~comment, x) => "\n/* " ++ (comment ++ (" */\n  " ++ x))

let arrayAccess = (~index, value) => value ++ ("[" ++ (string_of_int(index) ++ "]"))

let arraySlice = value => value ++ ".slice()"

let fieldAccess = (~label, value) => value ++ ("." ++ label)
