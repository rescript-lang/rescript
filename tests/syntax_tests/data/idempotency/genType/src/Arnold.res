let verbose = DeadCommon.verbose

let printPos = (ppf, pos: Lexing.position) => {
  let file = pos.Lexing.pos_fname
  let line = pos.Lexing.pos_lnum
  Format.fprintf(ppf, "@{<filename>%s@} @{<dim>%i@}", file->Filename.basename, line)
}

module StringSet = Set.Make(String)

// Type Definitions

module FunctionName = {
  type t = string
}

module FunctionArgs = {
  type arg = {
    label: string,
    functionName: FunctionName.t,
  }

  type t = list<arg>

  let empty = list{}

  let argToString = ({label, functionName}) => label ++ (":" ++ functionName)

  let toString = functionArgs =>
    functionArgs == list{}
      ? ""
      : "<" ++ ((functionArgs->List.map(argToString)->String.concat(",")) ++ ">")

  let find = (t: t, ~label) =>
    switch t->List.find_opt(arg => arg.label == label) {
    | Some({functionName}) => Some(functionName)
    | None => None
    }

  let compareArg = (a1, a2) => {
    let n = compare(a1.label, a2.label)
    if n != 0 {
      n
    } else {
      compare(a1.functionName, a2.functionName)
    }
  }

  let rec compare = (l1, l2) =>
    switch (l1, l2) {
    | (list{}, list{}) => 0
    | (list{}, list{_, ..._}) => -1
    | (list{_, ..._}, list{}) => 1
    | (list{x1, ...l1}, list{x2, ...l2}) =>
      let n = compareArg(x1, x2)
      if n != 0 {
        n
      } else {
        compare(l1, l2)
      }
    }
}

module FunctionCall = {
  type t = {
    functionName: FunctionName.t,
    functionArgs: FunctionArgs.t,
  }

  let substituteName = (~sub, name) =>
    switch sub->FunctionArgs.find(~label=name) {
    | Some(functionName) => functionName
    | None => name
    }

  let applySubstitution = (~sub: FunctionArgs.t, t: t) =>
    if sub == list{} {
      t
    } else {
      {
        functionName: t.functionName->substituteName(~sub),
        functionArgs: t.functionArgs->List.map((arg: FunctionArgs.arg) => {
          ...arg,
          functionName: arg.functionName->substituteName(~sub),
        }),
      }
    }

  let noArgs = functionName => {functionName: functionName, functionArgs: list{}}

  let toString = ({functionName, functionArgs}) =>
    functionName ++ FunctionArgs.toString(functionArgs)

  let compare = (x1: t, x2) => {
    let n = compare(x1.functionName, x2.functionName)
    if n != 0 {
      n
    } else {
      FunctionArgs.compare(x1.functionArgs, x2.functionArgs)
    }
  }
}

module FunctionCallSet = Set.Make(FunctionCall)

module Stats = {
  let nCacheChecks = ref(0)
  let nCacheHits = ref(0)
  let nFiles = ref(0)
  let nFunctions = ref(0)
  let nHygieneErrors = ref(0)
  let nInfiniteLoops = ref(0)
  let nRecursiveBlocks = ref(0)

  let print = (ppf, ()) => {
    Format.fprintf(ppf, "@[<v 2>@,@{<warning>Termination Analysis Stats@}@,")
    Format.fprintf(ppf, "Files:@{<dim>%d@}@,", nFiles.contents)
    Format.fprintf(ppf, "Recursive Blocks:@{<dim>%d@}@,", nRecursiveBlocks.contents)
    Format.fprintf(ppf, "Functions:@{<dim>%d@}@,", nFunctions.contents)
    Format.fprintf(ppf, "Infinite Loops:@{<dim>%d@}@,", nInfiniteLoops.contents)
    Format.fprintf(ppf, "Hygiene Errors:@{<dim>%d@}@,", nHygieneErrors.contents)
    Format.fprintf(
      ppf,
      "Cache Hits:@{<dim>%d@}/@{<dim>%d@}@,",
      nCacheHits.contents,
      nCacheChecks.contents,
    )
    Format.fprintf(ppf, "@]")
  }

  let dump = () => Format.fprintf(Format.std_formatter, "%a@.", print, ())

  let newFile = () => incr(nFiles)

  let newRecursiveFunctions = (~numFunctions) => {
    incr(nRecursiveBlocks)
    nFunctions := nFunctions.contents + numFunctions
  }

  let logLoop = () => incr(nInfiniteLoops)

  let logCache = (~functionCall, ~hit, ~loc) => {
    incr(nCacheChecks)
    if hit {
      incr(nCacheHits)
    }
    if verbose {
      Log_.info(~loc, ~name="Termination Analysis", (ppf, ()) =>
        Format.fprintf(
          ppf,
          "Cache %s for @{<info>%s@}",
          hit ? "hit" : "miss",
          FunctionCall.toString(functionCall),
        )
      )
    }
  }

  let logResult = (~functionCall, ~loc, ~resString) =>
    if verbose {
      Log_.info(~loc, ~name="Termination Analysis", (ppf, ()) =>
        Format.fprintf(
          ppf,
          "@{<info>%s@} returns %s",
          FunctionCall.toString(functionCall),
          resString,
        )
      )
    }

  let logHygieneParametric = (~functionName, ~loc) => {
    incr(nHygieneErrors)
    Log_.error(~loc, ~name="Error Hygiene", (ppf, ()) =>
      Format.fprintf(
        ppf,
        "@{<error>%s@} cannot be analyzed directly as it is parametric",
        functionName,
      )
    )
  }

  let logHygieneOnlyCallDirectly = (~path, ~loc) => {
    incr(nHygieneErrors)
    Log_.error(~loc, ~name="Error Hygiene", (ppf, ()) =>
      Format.fprintf(
        ppf,
        "@{<error>%s@} can only be called directly, or passed as labeled argument",
        Path.name(path),
      )
    )
  }

  let logHygieneMustHaveNamedArgument = (~label, ~loc) => {
    incr(nHygieneErrors)
    Log_.error(~loc, ~name="Error Hygiene", (ppf, ()) =>
      Format.fprintf(ppf, "Call must have named argument @{<error>%s@}", label)
    )
  }

  let logHygieneNamedArgValue = (~label, ~loc) => {
    incr(nHygieneErrors)
    Log_.error(~loc, ~name="Error Hygiene", (ppf, ()) =>
      Format.fprintf(ppf, "Named argument @{<error>%s@} must be passed a recursive function", label)
    )
  }

  let logHygieneNoNestedLetRec = (~loc) => {
    incr(nHygieneErrors)
    Log_.error(~loc, ~name="Error Hygiene", (ppf, ()) =>
      Format.fprintf(ppf, "Nested multiple let rec not supported yet")
    )
  }
}

module Progress = {
  type t =
    | Progress
    | NoProgress

  let toString = progress => progress == Progress ? "Progress" : "NoProgress"
}

module Call = {
  type progressFunction = Path.t
  type t =
    | FunctionCall(FunctionCall.t)
    | ProgressFunction(progressFunction)

  let toString = call =>
    switch call {
    | ProgressFunction(progressFunction) => "+" ++ Path.name(progressFunction)
    | FunctionCall(functionCall) => FunctionCall.toString(functionCall)
    }
}

module Trace = {
  type retOption =
    | Rsome
    | Rnone

  type rec t =
    | Tcall(Call.t, Progress.t)
    | Tnondet(list<t>)
    | Toption(retOption)
    | Tseq(list<t>)

  let empty = Tseq(list{})

  let nd = (t1: t, t2: t): t =>
    switch (t1, t2) {
    | (Tnondet(l1), Tnondet(l2)) => Tnondet(\"@"(l1, l2))
    | (_, Tnondet(l2)) => Tnondet(list{t1, ...l2})
    | (Tnondet(l1), _) => Tnondet(\"@"(l1, list{t2}))
    | _ => Tnondet(list{t1, t2})
    }

  let seq = (t1: t, t2: t): t =>
    switch (t1, t2) {
    | (Tseq(l1), Tseq(l2)) => Tseq(\"@"(l1, l2))
    | (_, Tseq(l2)) => Tseq(list{t1, ...l2})
    | (Tseq(l1), _) => Tseq(\"@"(l1, list{t2}))
    | _ => Tseq(list{t1, t2})
    }

  let some = Toption(Rsome)
  let none = Toption(Rnone)

  let retOptionToString = r => r == Rsome ? "Some" : "None"

  let rec toString = trace =>
    switch trace {
    | Tcall(ProgressFunction(progressFunction), progress) =>
      Path.name(progressFunction) ++ (":" ++ Progress.toString(progress))
    | Tcall(FunctionCall(functionCall), progress) =>
      FunctionCall.toString(functionCall) ++ (":" ++ Progress.toString(progress))
    | Tnondet(traces) => "[" ++ ((traces->List.map(toString)->String.concat(" || ")) ++ "]")
    | Toption(retOption) => retOption->retOptionToString
    | Tseq(traces) =>
      let tracesNotEmpty = traces->List.filter(\"<>"(empty))
      switch tracesNotEmpty {
      | list{} => "_"
      | list{t} => t->toString
      | list{_, ..._} => tracesNotEmpty->List.map(toString)->String.concat("; ")
      }
    }
}

module Values: {
  type t
  let getNone: t => option<Progress.t>
  let getSome: t => option<Progress.t>
  let nd: (t, t) => t
  let none: (~progress: Progress.t) => t
  let some: (~progress: Progress.t) => t
  let toString: t => string
} = {
  type t = {
    none: option<Progress.t>,
    some: option<Progress.t>,
  }

  let getNone = ({none}) => none
  let getSome = ({some}) => some

  let toString = x =>
    \"@"(
      switch x.some {
      | None => list{}
      | Some(p) => list{"some: " ++ Progress.toString(p)}
      },
      switch x.none {
      | None => list{}
      | Some(p) => list{"none: " ++ Progress.toString(p)}
      },
    )->String.concat(", ")

  let none = (~progress) => {none: Some(progress), some: None}
  let some = (~progress) => {none: None, some: Some(progress)}

  let nd = (v1: t, v2: t): t => {
    let combine = (x, y) =>
      switch (x, y) {
      | (Some(progress1), Some(progress2)) =>
        Some(
          progress1 == Progress.Progress && progress2 == Progress ? Progress.Progress : NoProgress,
        )
      | (None, progressOpt)
      | (progressOpt, None) => progressOpt
      }
    let none = combine(v1.none, v2.none)
    let some = combine(v1.some, v2.some)
    {none: none, some: some}
  }
}

module State = {
  type t = {
    progress: Progress.t,
    trace: Trace.t,
    valuesOpt: option<Values.t>,
  }

  let toString = ({progress, trace, valuesOpt}) => {
    let progressStr = switch valuesOpt {
    | None => progress->Progress.toString
    | Some(values) => "{" ++ ((values->Values.toString) ++ "}")
    }
    progressStr ++ (" with trace " ++ Trace.toString(trace))
  }

  let init = (~progress=Progress.NoProgress, ~trace=Trace.empty, ~valuesOpt=None, ()) => {
    progress: progress,
    trace: trace,
    valuesOpt: valuesOpt,
  }

  let seq = (s1, s2) => {
    let progress =
      s1.progress == Progress || s2.progress == Progress ? Progress.Progress : NoProgress
    let trace = Trace.seq(s1.trace, s2.trace)
    let valuesOpt = s2.valuesOpt
    {progress: progress, trace: trace, valuesOpt: valuesOpt}
  }

  let sequence = states =>
    switch states {
    | list{} => assert(false)
    | list{s, ...nextStates} => List.fold_left(seq, s, nextStates)
    }

  let nd = (s1, s2) => {
    let progress =
      s1.progress == Progress && s2.progress == Progress ? Progress.Progress : NoProgress
    let trace = Trace.nd(s1.trace, s2.trace)
    let valuesOpt = switch (s1.valuesOpt, s2.valuesOpt) {
    | (None, valuesOpt) => s1.progress == Progress ? valuesOpt : None
    | (valuesOpt, None) => s2.progress == Progress ? valuesOpt : None
    | (Some(values1), Some(values2)) => Some(Values.nd(values1, values2))
    }
    {progress: progress, trace: trace, valuesOpt: valuesOpt}
  }

  let nondet = states =>
    switch states {
    | list{} => assert(false)
    | list{s, ...nextStates} => List.fold_left(nd, s, nextStates)
    }

  let unorderedSequence = states => {...states->sequence, valuesOpt: None}

  let none = (~progress) =>
    init(~progress, ~trace=Trace.none, ~valuesOpt=Some(Values.none(~progress)), ())

  let some = (~progress) =>
    init(~progress, ~trace=Trace.some, ~valuesOpt=Some(Values.some(~progress)), ())
}

module Command = {
  type progress = Progress.t

  type retOption = Trace.retOption

  type rec t =
    | Call(Call.t, Location.t)
    | ConstrOption(retOption)
    | Nondet(list<t>)
    | Nothing
    | Sequence(list<t>)
    | SwitchOption({functionCall: FunctionCall.t, loc: Location.t, some: t, none: t})
    | UnorderedSequence(list<t>)

  let rec toString = command =>
    switch command {
    | Call(call, _pos) => call->Call.toString
    | ConstrOption(r) => r->Trace.retOptionToString
    | Nondet(commands) => "[" ++ ((commands->List.map(toString)->String.concat(" || ")) ++ "]")
    | Nothing => "_"
    | Sequence(commands) => commands->List.map(toString)->String.concat("; ")
    | SwitchOption({functionCall, some: cSome, none: cNone}) =>
      "switch " ++
      (FunctionCall.toString(functionCall) ++
      (" {some: " ++ (toString(cSome) ++ (", none: " ++ (toString(cNone) ++ "}")))))
    | UnorderedSequence(commands) =>
      "{" ++ ((commands->List.map(toString)->String.concat(", ")) ++ "}")
    }

  let nothing = Nothing

  let nondet = commands => {
    let rec loop = commands =>
      switch commands {
      | list{} => nothing
      | list{Nondet(commands), ...rest} => loop(\"@"(commands, rest))
      | list{command} => command
      | _ => Nondet(commands)
      }
    loop(commands)
  }

  let sequence = commands => {
    let rec loop = (acc, commands) =>
      switch commands {
      | list{} => List.rev(acc)
      | list{Nothing, ...cs} if cs != list{} => loop(acc, cs)
      | list{Sequence(cs1), ...cs2} => loop(acc, \"@"(cs1, cs2))
      | list{c, ...cs} => loop(list{c, ...acc}, cs)
      }
    switch loop(list{}, commands) {
    | list{c} => c
    | cs => Sequence(cs)
    }
  }

  let \"+++" = (c1, c2) => sequence(list{c1, c2})

  let unorderedSequence = commands => {
    let relevantCommands = commands->List.filter(x => x != nothing)
    switch relevantCommands {
    | list{} => nothing
    | list{c} => c
    | list{_, _, ..._} => UnorderedSequence(relevantCommands)
    }
  }
}

module Kind = {
  type rec t = list<entry>
  and entry = {
    label: string,
    k: t,
  }

  let empty: t = list{}

  let hasLabel = (~label, k: t) => k->List.exists(entry => entry.label == label)

  let rec entryToString = ({label, k}) => k == list{} ? label : label ++ (":" ++ (k->toString))

  and toString = (kind: t) =>
    kind == list{} ? "" : "<" ++ ((kind->List.map(entryToString)->String.concat(", ")) ++ ">")

  let addLabelWithEmptyKind = (~label, kind) =>
    if !(kind->hasLabel(~label)) {
      list{{label: label, k: empty}, ...kind}->List.sort(compare)
    } else {
      kind
    }
}

module FunctionTable = {
  type functionDefinition = {
    mutable body: option<Command.t>,
    mutable kind: Kind.t,
  }

  type t = Hashtbl.t<FunctionName.t, functionDefinition>

  let create = (): t => Hashtbl.create(1)

  let print = (ppf, tbl: t) => {
    Format.fprintf(ppf, "@[<v 2>@,@{<warning>Function Table@}")
    let definitions =
      Hashtbl.fold(
        (functionName, {kind, body}, definitions) => list{
          (functionName, kind, body),
          ...definitions,
        },
        tbl,
        list{},
      )->List.sort(((fn1, _, _), (fn2, _, _)) => String.compare(fn1, fn2))
    definitions->List.iteri((i, (functionName, kind, body)) =>
      Format.fprintf(
        ppf,
        "@,@{<dim>%d@} @{<info>%s%s@}: %s",
        i + 1,
        functionName,
        Kind.toString(kind),
        switch body {
        | Some(command) => Command.toString(command)
        | None => "None"
        },
      )
    )
    Format.fprintf(ppf, "@]")
  }

  let dump = tbl => Format.fprintf(Format.std_formatter, "%a@.", print, tbl)

  let initialFunctionDefinition = () => {kind: Kind.empty, body: None}

  let getFunctionDefinition = (~functionName, tbl: t) =>
    try Hashtbl.find(tbl, functionName) catch {
    | Not_found => assert(false)
    }

  let isInFunctionInTable = (~functionTable, path) => Hashtbl.mem(functionTable, Path.name(path))

  let addFunction = (~functionName, tbl: t) => {
    if Hashtbl.mem(tbl, functionName) {
      assert(false)
    }
    Hashtbl.replace(tbl, functionName, initialFunctionDefinition())
  }

  let addLabelToKind = (~functionName, ~label, tbl: t) => {
    let functionDefinition = tbl->getFunctionDefinition(~functionName)
    functionDefinition.kind = functionDefinition.kind->Kind.addLabelWithEmptyKind(~label)
  }

  let addBody = (~body, ~functionName, tbl: t) => {
    let functionDefinition = tbl->getFunctionDefinition(~functionName)
    functionDefinition.body = body
  }

  let functionGetKindOfLabel = (~functionName, ~label, tbl: t) =>
    switch Hashtbl.find(tbl, functionName) {
    | {kind} => kind->Kind.hasLabel(~label) ? Some(Kind.empty) : None
    | exception Not_found => None
    }
}

module FindFunctionsCalled = {
  let traverseExpr = (~callees) => {
    let super = Tast_mapper.default

    let expr = (self: Tast_mapper.mapper, e: Typedtree.expression) => {
      switch e.exp_desc {
      | Texp_apply({exp_desc: Texp_ident(callee, _, _)}, _args) =>
        let functionName = Path.name(callee)
        callees := StringSet.add(functionName, callees.contents)
      | _ => ()
      }
      super.expr(self, e)
    }

    open Tast_mapper
    {...super, expr: expr}
  }

  let findCallees = (expression: Typedtree.expression) => {
    let isFunction = switch expression.exp_desc {
    | Texp_function(_) => true
    | _ => false
    }
    let callees = ref(StringSet.empty)
    let traverseExpr = traverseExpr(~callees)
    if isFunction {
      expression->traverseExpr.expr(traverseExpr)->ignore
    }
    callees.contents
  }
}

module ExtendFunctionTable = {
  // Add functions passed a recursive function via a labeled argument,
  // and functions calling progress functions, to the function table.

  let extractLabelledArgument = (~kindOpt=None, argOpt: option<Typedtree.expression>) =>
    switch argOpt {
    | Some({exp_desc: Texp_ident(path, {loc}, _)}) => Some((path, loc))
    | Some({
        exp_desc: Texp_let(
          Nonrecursive,
          list{{
            vb_pat: {pat_desc: Tpat_var(_, _)},
            vb_expr: {exp_desc: Texp_ident(path, {loc}, _)},
            vb_loc: {loc_ghost: true},
          }},
          _,
        ),
      }) =>
      Some((path, loc))
    | Some({exp_desc: Texp_apply({exp_desc: Texp_ident(path, {loc}, _)}, args)})
      if kindOpt != None =>
      let checkArg = ((argLabel: Asttypes.arg_label, _argOpt)) =>
        switch (argLabel, kindOpt) {
        | (Labelled(l) | Optional(l), Some(kind)) =>
          kind->List.for_all(({Kind.label: label}) => label != l)
        | _ => true
        }
      if args->List.for_all(checkArg) {
        Some((path, loc))
      } else {
        None
      }
    | _ => None
    }

  let traverseExpr = (~functionTable, ~progressFunctions, ~valueBindingsTable) => {
    let super = Tast_mapper.default

    let expr = (self: Tast_mapper.mapper, e: Typedtree.expression) => {
      switch e.exp_desc {
      | Texp_ident(callee, _, _) =>
        let loc = e.exp_loc
        switch Hashtbl.find_opt(valueBindingsTable, Path.name(callee)) {
        | None => ()
        | Some((id_pos, _, callees)) =>
          if !StringSet.is_empty(StringSet.inter(Lazy.force(callees), progressFunctions)) {
            let functionName = Path.name(callee)
            if !(callee->FunctionTable.isInFunctionInTable(~functionTable)) {
              functionTable->FunctionTable.addFunction(~functionName)
              if verbose {
                Log_.info(~loc, ~name="Termination Analysis", (ppf, ()) =>
                  Format.fprintf(
                    ppf,
                    "Extend Function Table with @{<info>%s@} (%a) as it calls a progress function",
                    functionName,
                    printPos,
                    id_pos,
                  )
                )
              }
            }
          }
        }
      | Texp_apply({exp_desc: Texp_ident(callee, _, _)}, args)
        if callee->FunctionTable.isInFunctionInTable(~functionTable) =>
        let functionName = Path.name(callee)
        args->List.iter(((argLabel: Asttypes.arg_label, argOpt)) =>
          switch (argLabel, argOpt->extractLabelledArgument) {
          | (Labelled(label), Some((path, loc)))
            if path->FunctionTable.isInFunctionInTable(~functionTable) =>
            functionTable->FunctionTable.addLabelToKind(~functionName, ~label)
            if verbose {
              Log_.info(~loc, ~name="Termination Analysis", (ppf, ()) =>
                Format.fprintf(
                  ppf,
                  "@{<info>%s@} is parametric ~@{<info>%s@}=@{<info>%s@}",
                  functionName,
                  label,
                  Path.name(path),
                )
              )
            }

          | _ => ()
          }
        )
      | _ => ()
      }
      super.expr(self, e)
    }

    open Tast_mapper
    {...super, expr: expr}
  }

  let run = (
    ~functionTable,
    ~progressFunctions,
    ~valueBindingsTable,
    expression: Typedtree.expression,
  ) => {
    let traverseExpr = traverseExpr(~functionTable, ~progressFunctions, ~valueBindingsTable)
    expression->traverseExpr.expr(traverseExpr)->ignore
  }
}

module CheckExpressionWellFormed = {
  let traverseExpr = (~functionTable, ~valueBindingsTable) => {
    let super = Tast_mapper.default

    let checkIdent = (~path, ~loc) =>
      if path->FunctionTable.isInFunctionInTable(~functionTable) {
        Stats.logHygieneOnlyCallDirectly(~path, ~loc)
      }

    let expr = (self: Tast_mapper.mapper, e: Typedtree.expression) =>
      switch e.exp_desc {
      | Texp_ident(path, {loc}, _) =>
        checkIdent(~path, ~loc)
        e
      | Texp_apply({exp_desc: Texp_ident(functionPath, _, _)}, args) =>
        let functionName = Path.name(functionPath)
        args->List.iter(((argLabel: Asttypes.arg_label, argOpt)) =>
          switch argOpt->ExtendFunctionTable.extractLabelledArgument {
          | Some((path, loc)) =>
            switch argLabel {
            | Labelled(label) =>
              if (
                functionTable->FunctionTable.functionGetKindOfLabel(~functionName, ~label) != None
              ) {
                ()
              } else {
                switch Hashtbl.find_opt(valueBindingsTable, functionName) {
                | Some((_pos, body: Typedtree.expression, _))
                  if path->FunctionTable.isInFunctionInTable(~functionTable) =>
                  let inTable = functionPath->FunctionTable.isInFunctionInTable(~functionTable)
                  if !inTable {
                    functionTable->FunctionTable.addFunction(~functionName)
                  }
                  functionTable->FunctionTable.addLabelToKind(~functionName, ~label)
                  if verbose {
                    Log_.info(~loc=body.exp_loc, ~name="Termination Analysis", (ppf, ()) =>
                      Format.fprintf(
                        ppf,
                        "Extend Function Table with @{<info>%s@} as parametric ~@{<info>%s@}=@{<info>%s@}",
                        functionName,
                        label,
                        Path.name(path),
                      )
                    )
                  }

                | _ => checkIdent(~path, ~loc)
                }
              }

            | Optional(_)
            | Nolabel =>
              checkIdent(~path, ~loc)
            }

          | _ => ()
          }
        )
        e

      | _ => super.expr(self, e)
      }

    open Tast_mapper
    {...super, expr: expr}
  }

  let run = (~functionTable, ~valueBindingsTable, expression: Typedtree.expression) => {
    let traverseExpr = traverseExpr(~functionTable, ~valueBindingsTable)
    expression->traverseExpr.expr(traverseExpr)->ignore
  }
}

module Compile = {
  type ctx = {
    currentFunctionName: FunctionName.t,
    functionTable: FunctionTable.t,
    innerRecursiveFunctions: Hashtbl.t<FunctionName.t, FunctionName.t>,
    isProgressFunction: Path.t => bool,
  }

  let rec expression = (~ctx, expr: Typedtree.expression) => {
    let {currentFunctionName, functionTable, isProgressFunction} = ctx
    let loc = expr.exp_loc
    switch expr.exp_desc {
    | Texp_ident(_) => Command.nothing

    | Texp_apply({exp_desc: Texp_ident(calleeToRename, l, vd)} as expr, argsToExtend) =>
      let (callee, args) = switch Hashtbl.find_opt(
        ctx.innerRecursiveFunctions,
        Path.name(calleeToRename),
      ) {
      | Some(innerFunctionName) =>
        let innerFunctionDefinition =
          functionTable->FunctionTable.getFunctionDefinition(~functionName=innerFunctionName)
        let argsFromKind = innerFunctionDefinition.kind->List.map((entry: Kind.entry) => (
          Asttypes.Labelled(entry.label),
          Some({
            ...expr,
            exp_desc: Texp_ident(Path.Pident(Ident.create(entry.label)), l, vd),
          }),
        ))
        (Path.Pident(Ident.create(innerFunctionName)), \"@"(argsFromKind, argsToExtend))
      | None => (calleeToRename, argsToExtend)
      }
      if callee->FunctionTable.isInFunctionInTable(~functionTable) {
        let functionName = Path.name(callee)
        let functionDefinition = functionTable->FunctionTable.getFunctionDefinition(~functionName)
        exception ArgError
        let getFunctionArg = ({Kind.label: label}) => {
          let argOpt = args->List.find_opt(arg =>
            switch arg {
            | (Asttypes.Labelled(s), Some(_)) => s == label
            | _ => false
            }
          )
          let argOpt = switch argOpt {
          | Some((_, Some(e))) => Some(e)
          | _ => None
          }
          let functionArg = switch argOpt->ExtendFunctionTable.extractLabelledArgument(
            ~kindOpt=Some(functionDefinition.kind),
          ) {
          | None =>
            Stats.logHygieneMustHaveNamedArgument(~label, ~loc)
            raise(ArgError)

          | Some((path, _pos)) if path->FunctionTable.isInFunctionInTable(~functionTable) =>
            let functionName = Path.name(path)
            {FunctionArgs.label: label, functionName: functionName}

          | Some((path, _pos))
            if functionTable
            ->FunctionTable.functionGetKindOfLabel(
              ~functionName=currentFunctionName,
              ~label=Path.name(path),
            ) ==
              Some(list{}) /* TODO: when kinds are inferred, support and check non-empty kinds */ =>
            let functionName = Path.name(path)
            {FunctionArgs.label: label, functionName: functionName}

          | _ =>
            Stats.logHygieneNamedArgValue(~label, ~loc)
            raise(ArgError)
          }
          functionArg
        }
        let functionArgsOpt = try Some(functionDefinition.kind->List.map(getFunctionArg)) catch {
        | ArgError => None
        }
        switch functionArgsOpt {
        | None => Command.nothing
        | Some(functionArgs) =>
          Command.Call(
            FunctionCall({functionName: functionName, functionArgs: functionArgs}),
            loc,
          )->evalArgs(~args, ~ctx)
        }
      } else if callee->isProgressFunction {
        Command.Call(ProgressFunction(callee), loc)->evalArgs(~args, ~ctx)
      } else {
        switch functionTable->FunctionTable.functionGetKindOfLabel(
          ~functionName=currentFunctionName,
          ~label=Path.name(callee),
        ) {
        | Some(kind) if kind == Kind.empty =>
          Command.Call(FunctionCall(Path.name(callee)->FunctionCall.noArgs), loc)->evalArgs(
            ~args,
            ~ctx,
          )
        | Some(_kind) =>
          // TODO when kinds are extended in future: check that args matches with kind
          // and create a function call with the appropriate arguments
          assert(false)
        | None => expr->expression(~ctx)->evalArgs(~args, ~ctx)
        }
      }
    | Texp_apply(expr, args) => expr->expression(~ctx)->evalArgs(~args, ~ctx)
    | Texp_let(Recursive, list{{vb_pat: {pat_desc: Tpat_var(id, _), pat_loc}, vb_expr}}, inExpr) =>
      let oldFunctionName = Ident.name(id)
      let newFunctionName = currentFunctionName ++ ("$" ++ oldFunctionName)
      functionTable->FunctionTable.addFunction(~functionName=newFunctionName)
      let newFunctionDefinition =
        functionTable->FunctionTable.getFunctionDefinition(~functionName=newFunctionName)
      let currentFunctionDefinition =
        functionTable->FunctionTable.getFunctionDefinition(~functionName=currentFunctionName)
      newFunctionDefinition.kind = currentFunctionDefinition.kind
      let newCtx = {...ctx, currentFunctionName: newFunctionName}
      Hashtbl.replace(ctx.innerRecursiveFunctions, oldFunctionName, newFunctionName)
      newFunctionDefinition.body = Some(vb_expr->expression(~ctx=newCtx))
      if verbose {
        Log_.info(~loc=pat_loc, ~name="Termination Analysis", (ppf, ()) =>
          Format.fprintf(ppf, "Adding recursive definition @{<info>%s@}", newFunctionName)
        )
      }
      inExpr->expression(~ctx)

    | Texp_let(recFlag, valueBindings, inExpr) =>
      if recFlag == Recursive {
        Stats.logHygieneNoNestedLetRec(~loc)
      }
      let commands = \"@"(
        valueBindings->List.map((vb: Typedtree.value_binding) => vb.vb_expr->expression(~ctx)),
        list{inExpr->expression(~ctx)},
      )
      Command.sequence(commands)
    | Texp_sequence(e1, e2) =>
      open Command
      \"+++"(expression(~ctx, e1), expression(~ctx, e2))
    | Texp_ifthenelse(e1, e2, eOpt) =>
      let c1 = e1->expression(~ctx)
      let c2 = e2->expression(~ctx)
      let c3 = eOpt->expressionOpt(~ctx)
      open Command
      \"+++"(c1, nondet(list{c2, c3}))
    | Texp_constant(_) => Command.nothing
    | Texp_construct({loc: {loc_ghost}}, {cstr_name}, expressions) =>
      let c = expressions->List.map(e => e->expression(~ctx))->Command.unorderedSequence
      switch cstr_name {
      | "Some" if loc_ghost == false =>
        open Command
        \"+++"(c, ConstrOption(Rsome))
      | "None" if loc_ghost == false =>
        open Command
        \"+++"(c, ConstrOption(Rnone))
      | _ => c
      }
    | Texp_function({cases}) => cases->List.map(case(~ctx))->Command.nondet

    | Texp_match(e, cases, list{}, _) =>
      let cE = e->expression(~ctx)
      let cCases = cases->List.map(case(~ctx))
      switch (cE, cases) {
      | (
          Call(FunctionCall(functionCall), loc),
          list{
            {c_lhs: {pat_desc: Tpat_construct(_, {cstr_name: ("Some" | "None") as name1}, _)}},
            {c_lhs: {pat_desc: Tpat_construct(_, {cstr_name: "Some" | "None"}, _)}},
          },
        ) =>
        let casesArr = Array.of_list(cCases)
        let (some, none) = name1 == "Some" ? (casesArr[0], casesArr[1]) : (casesArr[1], casesArr[0])
        SwitchOption({functionCall: functionCall, loc: loc, some: some, none: none})
      | _ =>
        open Command
        \"+++"(cE, nondet(cCases))
      }

    | Texp_match(_, _, list{_, ..._} as _casesExn, _) => assert(false)

    | Texp_field(e, _lid, _desc) => e->expression(~ctx)

    | Texp_record({fields, extended_expression}) =>
      list{
        extended_expression,
        ...fields
        ->Array.to_list
        ->List.map(((_desc, recordLabelDefinition: Typedtree.record_label_definition)) =>
          switch recordLabelDefinition {
          | Kept(_typeExpr) => None
          | Overridden(_loc, e) => Some(e)
          }
        ),
      }
      ->List.map(expressionOpt(~ctx))
      ->Command.unorderedSequence

    | Texp_setfield(e1, _loc, _desc, e2) =>
      list{e1, e2}->List.map(expression(~ctx))->Command.unorderedSequence

    | Texp_tuple(expressions) =>
      expressions->List.map(expression(~ctx))->Command.unorderedSequence

    | Texp_assert(_) => Command.nothing

    | Texp_try(_) => assert(false)
    | Texp_variant(_) => assert(false)
    | Texp_array(_) => assert(false)
    | Texp_while(_) => assert(false)
    | Texp_for(_) => assert(false)
    | Texp_send(_) => assert(false)
    | Texp_new(_) => assert(false)
    | Texp_instvar(_) => assert(false)
    | Texp_setinstvar(_) => assert(false)
    | Texp_override(_) => assert(false)
    | Texp_letmodule(_) => assert(false)
    | Texp_letexception(_) => assert(false)
    | Texp_lazy(_) => assert(false)
    | Texp_object(_) => assert(false)
    | Texp_pack(_) => assert(false)
    | Texp_unreachable => assert(false)
    | Texp_extension_constructor(_) => assert(false)
    }
  }
  and expressionOpt = (~ctx, eOpt) =>
    switch eOpt {
    | None => Command.nothing
    | Some(e) => e->expression(~ctx)
    }
  and evalArgs = (~args, ~ctx, command) => {
    // Don't assume any evaluation order on the arguments
    let commands = args->List.map(((_, eOpt)) => eOpt->expressionOpt(~ctx))
    open Command
    \"+++"(unorderedSequence(commands), command)
  }
  and case = (~ctx, {c_guard, c_rhs}: Typedtree.case) =>
    switch c_guard {
    | None => c_rhs->expression(~ctx)
    | Some(e) =>
      open Command
      \"+++"(expression(~ctx, e), expression(~ctx, c_rhs))
    }
}

module CallStack = {
  type frame = {
    frameNumber: int,
    pos: Lexing.position,
  }

  type t = {
    tbl: Hashtbl.t<FunctionCall.t, frame>,
    mutable size: int,
  }

  let create = () => {tbl: Hashtbl.create(1), size: 0}

  let toSet = ({tbl}) =>
    Hashtbl.fold((frame, _i, set) => FunctionCallSet.add(frame, set), tbl, FunctionCallSet.empty)

  let hasFunctionCall = (~functionCall, t: t) => Hashtbl.mem(t.tbl, functionCall)

  let addFunctionCall = (~functionCall, ~pos, t: t) => {
    t.size = t.size + 1
    Hashtbl.replace(t.tbl, functionCall, {frameNumber: t.size, pos: pos})
  }

  let removeFunctionCall = (~functionCall, t: t) => {
    t.size = t.size - 1
    Hashtbl.remove(t.tbl, functionCall)
  }

  let print = (ppf, t: t) => {
    Format.fprintf(ppf, "@[<v 2>CallStack:")
    let frames =
      Hashtbl.fold(
        (functionCall, {frameNumber, pos}, frames) => list{
          (functionCall, frameNumber, pos),
          ...frames,
        },
        t.tbl,
        list{},
      )->List.sort(((_, i1, _), (_, i2, _)) => i2 - i1)
    frames->List.iter(((functionCall: FunctionCall.t, i, pos)) =>
      Format.fprintf(
        ppf,
        "@,@{<dim>%d@} %s (%a)",
        i,
        FunctionCall.toString(functionCall),
        printPos,
        pos,
      )
    )
    Format.fprintf(ppf, "@]")
  }
}

module Eval = {
  type progress = Progress.t

  type cache = Hashtbl.t<FunctionCall.t, State.t>

  let createCache = (): cache => Hashtbl.create(1)

  let lookupCache = (~functionCall, cache: cache) => Hashtbl.find_opt(cache, functionCall)

  let updateCache = (~functionCall, ~loc, ~state, cache: cache) => {
    Stats.logResult(~functionCall, ~resString=state->State.toString, ~loc)
    if !Hashtbl.mem(cache, functionCall) {
      Hashtbl.replace(cache, functionCall, state)
    }
  }

  let hasInfiniteLoop = (~callStack, ~functionCallToInstantiate, ~functionCall, ~loc, ~state) =>
    if callStack->CallStack.hasFunctionCall(~functionCall) {
      if state.State.progress == NoProgress {
        Log_.error(~loc, ~name="Error Termination", (ppf, ()) => {
          Format.fprintf(ppf, "Possible infinite loop when calling ")
          functionCallToInstantiate == functionCall
            ? Format.fprintf(
                ppf,
                "@{<error>%s@}",
                functionCallToInstantiate->FunctionCall.toString,
              )
            : Format.fprintf(
                ppf,
                "@{<error>%s@} which is @{<error>%s@}",
                functionCallToInstantiate->FunctionCall.toString,
                functionCall->FunctionCall.toString,
              )
          Format.fprintf(ppf, "@,%a", CallStack.print, callStack)
        })
        Stats.logLoop()
      }
      true
    } else {
      false
    }

  let rec runFunctionCall = (
    ~cache,
    ~callStack,
    ~functionArgs,
    ~functionTable,
    ~madeProgressOn,
    ~loc,
    ~state,
    functionCallToInstantiate,
  ): State.t => {
    let pos = loc.Location.loc_start
    let functionCall =
      functionCallToInstantiate->FunctionCall.applySubstitution(~sub=functionArgs)
    let functionName = functionCall.functionName
    let call = Call.FunctionCall(functionCall)
    let stateAfterCall = switch cache->lookupCache(~functionCall) {
    | Some(stateAfterCall) =>
      Stats.logCache(~functionCall, ~hit=true, ~loc)
      {
        ...stateAfterCall,
        trace: Trace.Tcall(call, stateAfterCall.progress),
      }
    | None =>
      if FunctionCallSet.mem(functionCall, madeProgressOn) {
        State.init(~progress=Progress, ~trace=Trace.Tcall(call, Progress), ())
      } else if (
        hasInfiniteLoop(~callStack, ~functionCallToInstantiate, ~functionCall, ~loc, ~state)
      ) {
        {
          ...state,
          trace: Trace.Tcall(call, state.progress),
        }
      } else {
        Stats.logCache(~functionCall, ~hit=false, ~loc)
        let functionDefinition = functionTable->FunctionTable.getFunctionDefinition(~functionName)
        callStack->CallStack.addFunctionCall(~functionCall, ~pos)
        let body = switch functionDefinition.body {
        | Some(body) => body
        | None => assert(false)
        }
        let stateAfterCall =
          body->run(
            ~cache,
            ~callStack,
            ~functionArgs=functionCall.functionArgs,
            ~functionTable,
            ~madeProgressOn,
            ~state=State.init(),
          )
        cache->updateCache(~functionCall, ~loc, ~state=stateAfterCall)
        // Invariant: run should restore the callStack
        callStack->CallStack.removeFunctionCall(~functionCall)
        let trace = Trace.Tcall(call, stateAfterCall.progress)
        {...stateAfterCall, trace: trace}
      }
    }
    State.seq(state, stateAfterCall)
  }
  and run = (
    ~cache: cache,
    ~callStack,
    ~functionArgs,
    ~functionTable,
    ~madeProgressOn,
    ~state,
    command: Command.t,
  ): State.t =>
    switch command {
    | Call(FunctionCall(functionCall), loc) =>
      functionCall->runFunctionCall(
        ~cache,
        ~callStack,
        ~functionArgs,
        ~functionTable,
        ~madeProgressOn,
        ~loc,
        ~state,
      )
    | Call(ProgressFunction(_) as call, _pos) =>
      let state1 = State.init(~progress=Progress, ~trace=Tcall(call, Progress), ())
      State.seq(state, state1)

    | ConstrOption(r) =>
      let state1 =
        r == Rsome ? State.some(~progress=state.progress) : State.none(~progress=state.progress)
      State.seq(state, state1)

    | Nothing =>
      let state1 = State.init()
      State.seq(state, state1)

    | Sequence(commands) =>
      // if one command makes progress, then the sequence makes progress
      let rec findFirstProgress = (~callStack, ~commands, ~madeProgressOn, ~state) =>
        switch commands {
        | list{} => state
        | list{c, ...nextCommands} =>
          let state1 =
            c->run(~cache, ~callStack, ~functionArgs, ~functionTable, ~madeProgressOn, ~state)
          let (madeProgressOn, callStack) = switch state1.progress {
          | Progress => // look for infinite loops in the rest of the sequence, remembering what has made progress
            (
              FunctionCallSet.union(madeProgressOn, callStack->CallStack.toSet),
              CallStack.create(),
            )
          | NoProgress => (madeProgressOn, callStack)
          }
          findFirstProgress(~callStack, ~commands=nextCommands, ~madeProgressOn, ~state=state1)
        }
      findFirstProgress(~callStack, ~commands, ~madeProgressOn, ~state)

    | UnorderedSequence(commands) =>
      let stateNoTrace = {...state, trace: Trace.empty}
      // the commands could be executed in any order: progess if any one does
      let states =
        commands->List.map(c =>
          c->run(
            ~cache,
            ~callStack,
            ~functionArgs,
            ~functionTable,
            ~madeProgressOn,
            ~state=stateNoTrace,
          )
        )
      State.seq(state, states->State.unorderedSequence)

    | Nondet(commands) =>
      let stateNoTrace = {...state, trace: Trace.empty}
      // the commands could be executed in any order: progess if any one does
      let states =
        commands->List.map(c =>
          c->run(
            ~cache,
            ~callStack,
            ~functionArgs,
            ~functionTable,
            ~madeProgressOn,
            ~state=stateNoTrace,
          )
        )
      State.seq(state, states->State.nondet)

    | SwitchOption({functionCall, loc, some, none}) =>
      let stateAfterCall =
        functionCall->runFunctionCall(
          ~cache,
          ~callStack,
          ~functionArgs,
          ~functionTable,
          ~madeProgressOn,
          ~loc,
          ~state,
        )
      switch stateAfterCall.valuesOpt {
      | None =>
        Command.nondet(list{some, none})->run(
          ~cache,
          ~callStack,
          ~functionArgs,
          ~functionTable,
          ~madeProgressOn,
          ~state=stateAfterCall,
        )
      | Some(values) =>
        let runOpt = (c, progressOpt) =>
          switch progressOpt {
          | None => State.init(~progress=Progress, ())
          | Some(progress) =>
            c->run(
              ~cache,
              ~callStack,
              ~functionArgs,
              ~functionTable,
              ~madeProgressOn,
              ~state=State.init(~progress, ()),
            )
          }
        let stateNone = values->Values.getNone->runOpt(none)
        let stateSome = values->Values.getSome->runOpt(some)
        State.seq(stateAfterCall, State.nondet(list{stateSome, stateNone}))
      }
    }

  let analyzeFunction = (~cache, ~functionTable, ~loc, functionName) => {
    if verbose {
      Log_.log("@[<v 2>@,@{<warning>Termination Analysis@} for @{<info>%s@}@]@.", functionName)
    }
    let pos = loc.Location.loc_start
    let callStack = CallStack.create()
    let functionArgs = FunctionArgs.empty
    let functionCall = FunctionCall.noArgs(functionName)
    callStack->CallStack.addFunctionCall(~functionCall, ~pos)
    let functionDefinition = functionTable->FunctionTable.getFunctionDefinition(~functionName)
    if functionDefinition.kind != Kind.empty {
      Stats.logHygieneParametric(~functionName, ~loc)
    } else {
      let body = switch functionDefinition.body {
      | Some(body) => body
      | None => assert(false)
      }
      let state =
        body->run(
          ~cache,
          ~callStack,
          ~functionArgs,
          ~functionTable,
          ~madeProgressOn=FunctionCallSet.empty,
          ~state=State.init(),
        )
      cache->updateCache(~functionCall, ~loc, ~state)
    }
  }
}

let progressFunctionsFromAttributes = attributes => {
  let lidToString = lid => lid->Longident.flatten->String.concat(".")
  let isProgress = \"="("progress")
  if attributes->Annotation.hasAttribute(isProgress) {
    Some(
      switch attributes->Annotation.getAttributePayload(isProgress) {
      | None => list{}
      | Some(IdentPayload(lid)) => list{lidToString(lid)}
      | Some(TuplePayload(l)) =>
        l
        ->List.filter(x =>
          switch x {
          | Annotation.IdentPayload(_) => true
          | _ => false
          }
        )
        ->List.map(x =>
          switch x {
          | Annotation.IdentPayload(lid) => lidToString(lid)
          | _ => assert(false)
          }
        )
      | _ => list{}
      },
    )
  } else {
    None
  }
}

let traverseAst = (~valueBindingsTable) => {
  let super = Tast_mapper.default

  let value_bindings = (self: Tast_mapper.mapper, (recFlag, valueBindings)) => {
    // Update the table of value bindings for variables
    valueBindings->List.iter((vb: Typedtree.value_binding) =>
      switch vb.vb_pat.pat_desc {
      | Tpat_var(id, {loc: {loc_start: pos}}) =>
        let callees = Lazy.from_fun(() => FindFunctionsCalled.findCallees(vb.vb_expr))
        Hashtbl.replace(valueBindingsTable, Ident.name(id), (pos, vb.vb_expr, callees))
      | _ => ()
      }
    )

    let (progressFunctions, functionsToAnalyze) = if recFlag == Asttypes.Nonrecursive {
      (StringSet.empty, list{})
    } else {
      let (progressFunctions0, functionsToAnalyze0) =
        valueBindings->List.fold_left(
          ((progressFunctions, functionsToAnalyze), valueBinding: Typedtree.value_binding) =>
            switch progressFunctionsFromAttributes(valueBinding.vb_attributes) {
            | None => (progressFunctions, functionsToAnalyze)
            | Some(newProgressFunctions) => (
                StringSet.union(StringSet.of_list(newProgressFunctions), progressFunctions),
                switch valueBinding.vb_pat.pat_desc {
                | Tpat_var(id, _) => list{
                    (Ident.name(id), valueBinding.vb_expr.exp_loc),
                    ...functionsToAnalyze,
                  }
                | _ => functionsToAnalyze
                },
              )
            },
          (StringSet.empty, list{}),
        )
      (progressFunctions0, functionsToAnalyze0->List.rev)
    }

    if functionsToAnalyze != list{} {
      let functionTable = FunctionTable.create()
      let isProgressFunction = path => StringSet.mem(Path.name(path), progressFunctions)

      let recursiveFunctions = List.fold_left((defs, valueBinding: Typedtree.value_binding) =>
        switch valueBinding.vb_pat.pat_desc {
        | Tpat_var(id, _) => list{Ident.name(id), ...defs}
        | _ => defs
        }
      , list{}, valueBindings)->List.rev
      let recursiveDefinitions = recursiveFunctions->List.map(functionName => (
        functionName,
        {
          let (_pos, e, _set) = Hashtbl.find(valueBindingsTable, functionName)
          e
        },
      ))

      recursiveDefinitions->List.iter(((functionName, _body)) =>
        functionTable->FunctionTable.addFunction(~functionName)
      )

      recursiveDefinitions->List.iter(((_, body)) =>
        body->ExtendFunctionTable.run(~functionTable, ~progressFunctions, ~valueBindingsTable)
      )

      recursiveDefinitions->List.iter(((_, body)) =>
        body->CheckExpressionWellFormed.run(~functionTable, ~valueBindingsTable)
      )

      functionTable->Hashtbl.iter((
        functionName,
        functionDefinition: FunctionTable.functionDefinition,
      ) =>
        if functionDefinition.body == None {
          let (_pos, body, _) = Hashtbl.find(valueBindingsTable, functionName)
          functionTable->FunctionTable.addBody(
            ~body=Some(
              body->Compile.expression(
                ~ctx={
                  currentFunctionName: functionName,
                  functionTable: functionTable,
                  innerRecursiveFunctions: Hashtbl.create(1),
                  isProgressFunction: isProgressFunction,
                },
              ),
            ),
            ~functionName,
          )
        }
      )

      if verbose {
        FunctionTable.dump(functionTable)
      }

      let cache = Eval.createCache()
      functionsToAnalyze->List.iter(((functionName, loc)) =>
        functionName->Eval.analyzeFunction(~cache, ~functionTable, ~loc)
      )
      Stats.newRecursiveFunctions(~numFunctions=Hashtbl.length(functionTable))
    }

    valueBindings->List.iter(valueBinding => super.value_binding(self, valueBinding)->ignore)

    (recFlag, valueBindings)
  }

  open Tast_mapper
  {...super, value_bindings: value_bindings}
}

let processStructure = (structure: Typedtree.structure) => {
  Stats.newFile()
  let valueBindingsTable = Hashtbl.create(1)
  let traverseAst = traverseAst(~valueBindingsTable)
  structure->traverseAst.structure(traverseAst)->ignore
}

let reportResults = () =>
  if verbose {
    Stats.dump()
  }
