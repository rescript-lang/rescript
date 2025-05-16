let printPos ppf (pos : Lexing.position) =
  let file = pos.Lexing.pos_fname in
  let line = pos.Lexing.pos_lnum in
  Format.fprintf ppf "@{<filename>%s@} @{<dim>%i@}"
    (file |> Filename.basename)
    line

module StringSet = Set.Make (String)

(** Type Definitions *)
module FunctionName = struct
  type t = string
end

module FunctionArgs = struct
  type arg = {label: string; functionName: FunctionName.t}
  type t = arg list

  let empty = []
  let argToString {label; functionName} = label ^ ":" ^ functionName

  let toString functionArgs =
    match functionArgs = [] with
    | true -> ""
    | false ->
      "<" ^ (functionArgs |> List.map argToString |> String.concat ",") ^ ">"

  let find (t : t) ~label =
    match t |> List.find_opt (fun arg -> arg.label = label) with
    | Some {functionName} -> Some functionName
    | None -> None

  let compareArg a1 a2 =
    let n = compare a1.label a2.label in
    if n <> 0 then n else compare a1.functionName a2.functionName

  let rec compare l1 l2 =
    match (l1, l2) with
    | [], [] -> 0
    | [], _ :: _ -> -1
    | _ :: _, [] -> 1
    | x1 :: l1, x2 :: l2 ->
      let n = compareArg x1 x2 in
      if n <> 0 then n else compare l1 l2
end

module FunctionCall = struct
  type t = {functionName: FunctionName.t; functionArgs: FunctionArgs.t}

  let substituteName ~sub name =
    match sub |> FunctionArgs.find ~label:name with
    | Some functionName -> functionName
    | None -> name

  let applySubstitution ~(sub : FunctionArgs.t) (t : t) =
    if sub = [] then t
    else
      {
        functionName = t.functionName |> substituteName ~sub;
        functionArgs =
          t.functionArgs
          |> List.map (fun (arg : FunctionArgs.arg) ->
                 {
                   arg with
                   functionName = arg.functionName |> substituteName ~sub;
                 });
      }

  let noArgs functionName = {functionName; functionArgs = []}

  let toString {functionName; functionArgs} =
    functionName ^ FunctionArgs.toString functionArgs

  let compare (x1 : t) x2 =
    let n = compare x1.functionName x2.functionName in
    if n <> 0 then n else FunctionArgs.compare x1.functionArgs x2.functionArgs
end

module FunctionCallSet = Set.Make (FunctionCall)

module Stats = struct
  let nCacheChecks = ref 0
  let nCacheHits = ref 0
  let nFiles = ref 0
  let nFunctions = ref 0
  let nHygieneErrors = ref 0
  let nInfiniteLoops = ref 0
  let nRecursiveBlocks = ref 0

  let print ppf () =
    Format.fprintf ppf "@[<v 2>@,@{<warning>Termination Analysis Stats@}@,";
    Format.fprintf ppf "Files:@{<dim>%d@}@," !nFiles;
    Format.fprintf ppf "Recursive Blocks:@{<dim>%d@}@," !nRecursiveBlocks;
    Format.fprintf ppf "Functions:@{<dim>%d@}@," !nFunctions;
    Format.fprintf ppf "Infinite Loops:@{<dim>%d@}@," !nInfiniteLoops;
    Format.fprintf ppf "Hygiene Errors:@{<dim>%d@}@," !nHygieneErrors;
    Format.fprintf ppf "Cache Hits:@{<dim>%d@}/@{<dim>%d@}@," !nCacheHits
      !nCacheChecks;
    Format.fprintf ppf "@]"

  let dump ~ppf = Format.fprintf ppf "%a@." print ()
  let newFile () = incr nFiles

  let newRecursiveFunctions ~numFunctions =
    incr nRecursiveBlocks;
    nFunctions := !nFunctions + numFunctions

  let logLoop () = incr nInfiniteLoops

  let logCache ~functionCall ~hit ~loc =
    incr nCacheChecks;
    if hit then incr nCacheHits;
    if !Common.Cli.debug then
      Log_.warning ~forStats:false ~loc
        (Termination
           {
             termination = TerminationAnalysisInternal;
             message =
               Format.asprintf "Cache %s for @{<info>%s@}"
                 (match hit with
                 | true -> "hit"
                 | false -> "miss")
                 (FunctionCall.toString functionCall);
           })

  let logResult ~functionCall ~loc ~resString =
    if !Common.Cli.debug then
      Log_.warning ~forStats:false ~loc
        (Termination
           {
             termination = TerminationAnalysisInternal;
             message =
               Format.asprintf "@{<info>%s@} returns %s"
                 (FunctionCall.toString functionCall)
                 resString;
           })

  let logHygieneParametric ~functionName ~loc =
    incr nHygieneErrors;
    Log_.error ~loc
      (Termination
         {
           termination = ErrorHygiene;
           message =
             Format.asprintf
               "@{<error>%s@} cannot be analyzed directly as it is parametric"
               functionName;
         })

  let logHygieneOnlyCallDirectly ~path ~loc =
    incr nHygieneErrors;
    Log_.error ~loc
      (Termination
         {
           termination = ErrorHygiene;
           message =
             Format.asprintf
               "@{<error>%s@} can only be called directly, or passed as \
                labeled argument"
               (Path.name path);
         })

  let logHygieneMustHaveNamedArgument ~label ~loc =
    incr nHygieneErrors;
    Log_.error ~loc
      (Termination
         {
           termination = ErrorHygiene;
           message =
             Format.asprintf "Call must have named argument @{<error>%s@}" label;
         })

  let logHygieneNamedArgValue ~label ~loc =
    incr nHygieneErrors;
    Log_.error ~loc
      (Termination
         {
           termination = ErrorHygiene;
           message =
             Format.asprintf
               "Named argument @{<error>%s@} must be passed a recursive \
                function"
               label;
         })

  let logHygieneNoNestedLetRec ~loc =
    incr nHygieneErrors;
    Log_.error ~loc
      (Termination
         {
           termination = ErrorHygiene;
           message = Format.asprintf "Nested multiple let rec not supported yet";
         })
end

module Progress = struct
  type t = Progress | NoProgress

  let toString progress =
    match progress = Progress with
    | true -> "Progress"
    | false -> "NoProgress"
end

module Call = struct
  type progressFunction = Path.t

  type t =
    | FunctionCall of FunctionCall.t
    | ProgressFunction of progressFunction

  let toString call =
    match call with
    | ProgressFunction progressFunction -> "+" ^ Path.name progressFunction
    | FunctionCall functionCall -> FunctionCall.toString functionCall
end

module Trace = struct
  type retOption = Rsome | Rnone

  type t =
    | Tcall of Call.t * Progress.t
    | Tnondet of t list
    | Toption of retOption
    | Tseq of t list

  let empty = Tseq []

  let nd (t1 : t) (t2 : t) : t =
    match (t1, t2) with
    | Tnondet l1, Tnondet l2 -> Tnondet (l1 @ l2)
    | _, Tnondet l2 -> Tnondet (t1 :: l2)
    | Tnondet l1, _ -> Tnondet (l1 @ [t2])
    | _ -> Tnondet [t1; t2]

  let seq (t1 : t) (t2 : t) : t =
    match (t1, t2) with
    | Tseq l1, Tseq l2 -> Tseq (l1 @ l2)
    | _, Tseq l2 -> Tseq (t1 :: l2)
    | Tseq l1, _ -> Tseq (l1 @ [t2])
    | _ -> Tseq [t1; t2]

  let some = Toption Rsome
  let none = Toption Rnone

  let retOptionToString r =
    match r = Rsome with
    | true -> "Some"
    | false -> "None"

  let rec toString trace =
    match trace with
    | Tcall (ProgressFunction progressFunction, progress) ->
      Path.name progressFunction ^ ":" ^ Progress.toString progress
    | Tcall (FunctionCall functionCall, progress) ->
      FunctionCall.toString functionCall ^ ":" ^ Progress.toString progress
    | Tnondet traces ->
      "[" ^ (traces |> List.map toString |> String.concat " || ") ^ "]"
    | Toption retOption -> retOption |> retOptionToString
    | Tseq traces -> (
      let tracesNotEmpty = traces |> List.filter (( <> ) empty) in
      match tracesNotEmpty with
      | [] -> "_"
      | [t] -> t |> toString
      | _ :: _ -> tracesNotEmpty |> List.map toString |> String.concat "; ")
end

module Values : sig
  type t

  val getNone : t -> Progress.t option
  val getSome : t -> Progress.t option
  val nd : t -> t -> t
  val none : progress:Progress.t -> t
  val some : progress:Progress.t -> t
  val toString : t -> string
end = struct
  type t = {none: Progress.t option; some: Progress.t option}

  let getNone {none} = none
  let getSome {some} = some

  let toString x =
    ((match x.some with
     | None -> []
     | Some p -> ["some: " ^ Progress.toString p])
    @
    match x.none with
    | None -> []
    | Some p -> ["none: " ^ Progress.toString p])
    |> String.concat ", "

  let none ~progress = {none = Some progress; some = None}
  let some ~progress = {none = None; some = Some progress}

  let nd (v1 : t) (v2 : t) : t =
    let combine x y =
      match (x, y) with
      | Some progress1, Some progress2 ->
        Some
          (match progress1 = Progress.Progress && progress2 = Progress with
          | true -> Progress.Progress
          | false -> NoProgress)
      | None, progressOpt | progressOpt, None -> progressOpt
    in
    let none = combine v1.none v2.none in
    let some = combine v1.some v2.some in
    {none; some}
end

module State = struct
  type t = {progress: Progress.t; trace: Trace.t; valuesOpt: Values.t option}

  let toString {progress; trace; valuesOpt} =
    let progressStr =
      match valuesOpt with
      | None -> progress |> Progress.toString
      | Some values -> "{" ^ (values |> Values.toString) ^ "}"
    in
    progressStr ^ " with trace " ^ Trace.toString trace

  let init ?(progress = Progress.NoProgress) ?(trace = Trace.empty)
      ?(valuesOpt = None) () =
    {progress; trace; valuesOpt}

  let seq s1 s2 =
    let progress =
      match s1.progress = Progress || s2.progress = Progress with
      | true -> Progress.Progress
      | false -> NoProgress
    in
    let trace = Trace.seq s1.trace s2.trace in
    let valuesOpt = s2.valuesOpt in
    {progress; trace; valuesOpt}

  let sequence states =
    match states with
    | [] -> assert false
    | s :: nextStates -> List.fold_left seq s nextStates

  let nd s1 s2 =
    let progress =
      match s1.progress = Progress && s2.progress = Progress with
      | true -> Progress.Progress
      | false -> NoProgress
    in
    let trace = Trace.nd s1.trace s2.trace in
    let valuesOpt =
      match (s1.valuesOpt, s2.valuesOpt) with
      | None, valuesOpt -> (
        match s1.progress = Progress with
        | true -> valuesOpt
        | false -> None)
      | valuesOpt, None -> (
        match s2.progress = Progress with
        | true -> valuesOpt
        | false -> None)
      | Some values1, Some values2 -> Some (Values.nd values1 values2)
    in
    {progress; trace; valuesOpt}

  let nondet states =
    match states with
    | [] -> assert false
    | s :: nextStates -> List.fold_left nd s nextStates

  let unorderedSequence states = {(states |> sequence) with valuesOpt = None}

  let none ~progress =
    init ~progress ~trace:Trace.none
      ~valuesOpt:(Some (Values.none ~progress))
      ()

  let some ~progress =
    init ~progress ~trace:Trace.some
      ~valuesOpt:(Some (Values.some ~progress))
      ()
end

module Command = struct
  type progress = Progress.t
  type retOption = Trace.retOption

  type t =
    | Call of Call.t * Location.t
    | ConstrOption of retOption
    | Nondet of t list
    | Nothing
    | Sequence of t list
    | SwitchOption of {
        functionCall: FunctionCall.t;
        loc: Location.t;
        some: t;
        none: t;
      }
    | UnorderedSequence of t list

  let rec toString command =
    match command with
    | Call (call, _pos) -> call |> Call.toString
    | ConstrOption r -> r |> Trace.retOptionToString
    | Nondet commands ->
      "[" ^ (commands |> List.map toString |> String.concat " || ") ^ "]"
    | Nothing -> "_"
    | Sequence commands -> commands |> List.map toString |> String.concat "; "
    | SwitchOption {functionCall; some = cSome; none = cNone} ->
      "switch "
      ^ FunctionCall.toString functionCall
      ^ " {some: " ^ toString cSome ^ ", none: " ^ toString cNone ^ "}"
    | UnorderedSequence commands ->
      "{" ^ (commands |> List.map toString |> String.concat ", ") ^ "}"

  let nothing = Nothing

  let nondet commands =
    let rec loop commands =
      match commands with
      | [] -> nothing
      | Nondet commands :: rest -> loop (commands @ rest)
      | [command] -> command
      | _ -> Nondet commands
    in
    loop commands

  let sequence commands =
    let rec loop acc commands =
      match commands with
      | [] -> List.rev acc
      | Nothing :: cs when cs <> [] -> loop acc cs
      | Sequence cs1 :: cs2 -> loop acc (cs1 @ cs2)
      | c :: cs -> loop (c :: acc) cs
    in
    match loop [] commands with
    | [c] -> c
    | cs -> Sequence cs

  let ( +++ ) c1 c2 = sequence [c1; c2]

  let unorderedSequence commands =
    let relevantCommands = commands |> List.filter (fun x -> x <> nothing) in
    match relevantCommands with
    | [] -> nothing
    | [c] -> c
    | _ :: _ :: _ -> UnorderedSequence relevantCommands
end

module Kind = struct
  type t = entry list
  and entry = {label: string; k: t}

  let empty = ([] : t)

  let hasLabel ~label (k : t) =
    k |> List.exists (fun entry -> entry.label = label)

  let rec entryToString {label; k} =
    match k = [] with
    | true -> label
    | false -> label ^ ":" ^ (k |> toString)

  and toString (kind : t) =
    match kind = [] with
    | true -> ""
    | false ->
      "<" ^ (kind |> List.map entryToString |> String.concat ", ") ^ ">"

  let addLabelWithEmptyKind ~label kind =
    if not (kind |> hasLabel ~label) then
      {label; k = empty} :: kind |> List.sort compare
    else kind
end

module FunctionTable = struct
  type functionDefinition = {
    mutable body: Command.t option;
    mutable kind: Kind.t;
  }

  type t = (FunctionName.t, functionDefinition) Hashtbl.t

  let create () : t = Hashtbl.create 1

  let print ppf (tbl : t) =
    Format.fprintf ppf "@[<v 2>@,@{<warning>Function Table@}";
    let definitions =
      Hashtbl.fold
        (fun functionName {kind; body} definitions ->
          (functionName, kind, body) :: definitions)
        tbl []
      |> List.sort (fun (fn1, _, _) (fn2, _, _) -> String.compare fn1 fn2)
    in
    definitions
    |> List.iteri (fun i (functionName, kind, body) ->
           Format.fprintf ppf "@,@{<dim>%d@} @{<info>%s%s@}: %s" (i + 1)
             functionName (Kind.toString kind)
             (match body with
             | Some command -> Command.toString command
             | None -> "None"));
    Format.fprintf ppf "@]"

  let dump tbl = Format.fprintf Format.std_formatter "%a@." print tbl
  let initialFunctionDefinition () = {kind = Kind.empty; body = None}

  let getFunctionDefinition ~functionName (tbl : t) =
    try Hashtbl.find tbl functionName with Not_found -> assert false

  let isInFunctionInTable ~functionTable path =
    Hashtbl.mem functionTable (Path.name path)

  let addFunction ~functionName (tbl : t) =
    if Hashtbl.mem tbl functionName then assert false;
    Hashtbl.replace tbl functionName (initialFunctionDefinition ())

  let addLabelToKind ~functionName ~label (tbl : t) =
    let functionDefinition = tbl |> getFunctionDefinition ~functionName in
    functionDefinition.kind <-
      functionDefinition.kind |> Kind.addLabelWithEmptyKind ~label

  let addBody ~body ~functionName (tbl : t) =
    let functionDefinition = tbl |> getFunctionDefinition ~functionName in
    functionDefinition.body <- body

  let functionGetKindOfLabel ~functionName ~label (tbl : t) =
    match Hashtbl.find tbl functionName with
    | {kind} -> (
      match kind |> Kind.hasLabel ~label with
      | true -> Some Kind.empty
      | false -> None)
    | exception Not_found -> None
end

module FindFunctionsCalled = struct
  let traverseExpr ~callees =
    let super = Tast_mapper.default in
    let expr (self : Tast_mapper.mapper) (e : Typedtree.expression) =
      (match e.exp_desc with
      | Texp_apply {funct = {exp_desc = Texp_ident (callee, _, _)}} ->
        let functionName = Path.name callee in
        callees := !callees |> StringSet.add functionName
      | _ -> ());
      super.expr self e
    in
    {super with Tast_mapper.expr}

  let findCallees (expression : Typedtree.expression) =
    let isFunction =
      match expression.exp_desc with
      | Texp_function {arity = None} -> true
      | _ -> false
    in
    let callees = ref StringSet.empty in
    let traverseExpr = traverseExpr ~callees in
    if isFunction then expression |> traverseExpr.expr traverseExpr |> ignore;
    !callees
end

module ExtendFunctionTable = struct
  (* Add functions passed a recursive function via a labeled argument,
     and functions calling progress functions, to the function table. *)
  let extractLabelledArgument ?(kindOpt = None)
      (argOpt : Typedtree.expression option) =
    match argOpt with
    | Some {exp_desc = Texp_ident (path, {loc}, _)} -> Some (path, loc)
    | Some
        {
          exp_desc =
            Texp_let
              ( Nonrecursive,
                [
                  {
                    vb_pat = {pat_desc = Tpat_var (_, _)};
                    vb_expr = {exp_desc = Texp_ident (path, {loc}, _)};
                    vb_loc = {loc_ghost = true};
                  };
                ],
                _ );
        } ->
      Some (path, loc)
    | Some
        {
          exp_desc =
            Texp_apply {funct = {exp_desc = Texp_ident (path, {loc}, _)}; args};
        }
      when kindOpt <> None ->
      let checkArg ((argLabel : Asttypes.Noloc.arg_label), _argOpt) =
        match (argLabel, kindOpt) with
        | (Labelled l | Optional l), Some kind ->
          kind |> List.for_all (fun {Kind.label} -> label <> l)
        | _ -> true
      in
      if args |> List.for_all checkArg then Some (path, loc) else None
    | _ -> None

  let traverseExpr ~functionTable ~progressFunctions ~valueBindingsTable =
    let super = Tast_mapper.default in
    let expr (self : Tast_mapper.mapper) (e : Typedtree.expression) =
      (match e.exp_desc with
      | Texp_ident (callee, _, _) -> (
        let loc = e.exp_loc in
        match Hashtbl.find_opt valueBindingsTable (Path.name callee) with
        | None -> ()
        | Some (id_pos, _, callees) ->
          if
            not
              (StringSet.is_empty
                 (StringSet.inter (Lazy.force callees) progressFunctions))
          then
            let functionName = Path.name callee in
            if not (callee |> FunctionTable.isInFunctionInTable ~functionTable)
            then (
              functionTable |> FunctionTable.addFunction ~functionName;
              if !Common.Cli.debug then
                Log_.warning ~forStats:false ~loc
                  (Termination
                     {
                       termination = TerminationAnalysisInternal;
                       message =
                         Format.asprintf
                           "Extend Function Table with @{<info>%s@} (%a) as it \
                            calls a progress function"
                           functionName printPos id_pos;
                     })))
      | Texp_apply {funct = {exp_desc = Texp_ident (callee, _, _)}; args}
        when callee |> FunctionTable.isInFunctionInTable ~functionTable ->
        let functionName = Path.name callee in
        args
        |> List.iter (fun ((argLabel : Asttypes.Noloc.arg_label), argOpt) ->
               match (argLabel, argOpt |> extractLabelledArgument) with
               | Labelled label, Some (path, loc)
                 when path |> FunctionTable.isInFunctionInTable ~functionTable
                 ->
                 functionTable
                 |> FunctionTable.addLabelToKind ~functionName ~label;
                 if !Common.Cli.debug then
                   Log_.warning ~forStats:false ~loc
                     (Termination
                        {
                          termination = TerminationAnalysisInternal;
                          message =
                            Format.asprintf
                              "@{<info>%s@} is parametric \
                               ~@{<info>%s@}=@{<info>%s@}"
                              functionName label (Path.name path);
                        })
               | _ -> ())
      | _ -> ());
      super.expr self e
    in
    {super with Tast_mapper.expr}

  let run ~functionTable ~progressFunctions ~valueBindingsTable
      (expression : Typedtree.expression) =
    let traverseExpr =
      traverseExpr ~functionTable ~progressFunctions ~valueBindingsTable
    in
    expression |> traverseExpr.expr traverseExpr |> ignore
end

module CheckExpressionWellFormed = struct
  let traverseExpr ~functionTable ~valueBindingsTable =
    let super = Tast_mapper.default in
    let checkIdent ~path ~loc =
      if path |> FunctionTable.isInFunctionInTable ~functionTable then
        Stats.logHygieneOnlyCallDirectly ~path ~loc
    in
    let expr (self : Tast_mapper.mapper) (e : Typedtree.expression) =
      match e.exp_desc with
      | Texp_ident (path, {loc}, _) ->
        checkIdent ~path ~loc;
        e
      | Texp_apply {funct = {exp_desc = Texp_ident (functionPath, _, _)}; args}
        ->
        let functionName = Path.name functionPath in
        args
        |> List.iter (fun ((argLabel : Asttypes.Noloc.arg_label), argOpt) ->
               match argOpt |> ExtendFunctionTable.extractLabelledArgument with
               | Some (path, loc) -> (
                 match argLabel with
                 | Labelled label -> (
                   if
                     functionTable
                     |> FunctionTable.functionGetKindOfLabel ~functionName
                          ~label
                     <> None
                   then ()
                   else
                     match Hashtbl.find_opt valueBindingsTable functionName with
                     | Some (_pos, (body : Typedtree.expression), _)
                       when path
                            |> FunctionTable.isInFunctionInTable ~functionTable
                       ->
                       let inTable =
                         functionPath
                         |> FunctionTable.isInFunctionInTable ~functionTable
                       in
                       if not inTable then
                         functionTable
                         |> FunctionTable.addFunction ~functionName;
                       functionTable
                       |> FunctionTable.addLabelToKind ~functionName ~label;
                       if !Common.Cli.debug then
                         Log_.warning ~forStats:false ~loc:body.exp_loc
                           (Termination
                              {
                                termination = TerminationAnalysisInternal;
                                message =
                                  Format.asprintf
                                    "Extend Function Table with @{<info>%s@} \
                                     as parametric ~@{<info>%s@}=@{<info>%s@}"
                                    functionName label (Path.name path);
                              })
                     | _ -> checkIdent ~path ~loc)
                 | Optional _ | Nolabel -> checkIdent ~path ~loc)
               | _ -> ());
        e
      | _ -> super.expr self e
    in
    {super with Tast_mapper.expr}

  let run ~functionTable ~valueBindingsTable (expression : Typedtree.expression)
      =
    let traverseExpr = traverseExpr ~functionTable ~valueBindingsTable in
    expression |> traverseExpr.expr traverseExpr |> ignore
end

module Compile = struct
  type ctx = {
    currentFunctionName: FunctionName.t;
    functionTable: FunctionTable.t;
    innerRecursiveFunctions: (FunctionName.t, FunctionName.t) Hashtbl.t;
    isProgressFunction: Path.t -> bool;
  }

  let rec expression ~ctx (expr : Typedtree.expression) =
    let {currentFunctionName; functionTable; isProgressFunction} = ctx in
    let loc = expr.exp_loc in
    let notImplemented case =
      Log_.error ~loc
        (Termination
           {termination = ErrorNotImplemented; message = Format.asprintf case})
    in

    match expr.exp_desc with
    | Texp_ident _ -> Command.nothing
    | Texp_apply
        {
          funct = {exp_desc = Texp_ident (calleeToRename, l, vd)} as expr;
          args = argsToExtend;
        } -> (
      let callee, args =
        match
          Hashtbl.find_opt ctx.innerRecursiveFunctions
            (Path.name calleeToRename)
        with
        | Some innerFunctionName ->
          let innerFunctionDefinition =
            functionTable
            |> FunctionTable.getFunctionDefinition
                 ~functionName:innerFunctionName
          in
          let argsFromKind =
            innerFunctionDefinition.kind
            |> List.map (fun (entry : Kind.entry) ->
                   ( Asttypes.Noloc.Labelled entry.label,
                     Some
                       {
                         expr with
                         exp_desc =
                           Texp_ident
                             (Path.Pident (Ident.create entry.label), l, vd);
                       } ))
          in
          ( Path.Pident (Ident.create innerFunctionName),
            argsFromKind @ argsToExtend )
        | None -> (calleeToRename, argsToExtend)
      in
      if callee |> FunctionTable.isInFunctionInTable ~functionTable then
        let functionName = Path.name callee in
        let functionDefinition =
          functionTable |> FunctionTable.getFunctionDefinition ~functionName
        in
        let exception ArgError in
        let getFunctionArg {Kind.label} =
          let argOpt =
            args
            |> List.find_opt (fun arg ->
                   match arg with
                   | Asttypes.Noloc.Labelled s, Some _ -> s = label
                   | _ -> false)
          in
          let argOpt =
            match argOpt with
            | Some (_, Some e) -> Some e
            | _ -> None
          in
          let functionArg () =
            match
              argOpt
              |> ExtendFunctionTable.extractLabelledArgument
                   ~kindOpt:(Some functionDefinition.kind)
            with
            | None ->
              Stats.logHygieneMustHaveNamedArgument ~label ~loc;
              raise ArgError
            | Some (path, _pos)
              when path |> FunctionTable.isInFunctionInTable ~functionTable ->
              let functionName = Path.name path in
              {FunctionArgs.label; functionName}
            | Some (path, _pos)
              when functionTable
                   |> FunctionTable.functionGetKindOfLabel
                        ~functionName:currentFunctionName
                        ~label:(Path.name path)
                   = Some []
                   (* TODO: when kinds are inferred, support and check non-empty kinds *)
              ->
              let functionName = Path.name path in
              {FunctionArgs.label; functionName}
            | _ ->
              Stats.logHygieneNamedArgValue ~label ~loc;
              raise ArgError
              [@@raises ArgError]
          in
          functionArg ()
            [@@raises ArgError]
        in
        let functionArgsOpt =
          try Some (functionDefinition.kind |> List.map getFunctionArg)
          with ArgError -> None
        in
        match functionArgsOpt with
        | None -> Command.nothing
        | Some functionArgs ->
          Command.Call (FunctionCall {functionName; functionArgs}, loc)
          |> evalArgs ~args ~ctx
      else if callee |> isProgressFunction then
        Command.Call (ProgressFunction callee, loc) |> evalArgs ~args ~ctx
      else
        match
          functionTable
          |> FunctionTable.functionGetKindOfLabel
               ~functionName:currentFunctionName ~label:(Path.name callee)
        with
        | Some kind when kind = Kind.empty ->
          Command.Call
            (FunctionCall (Path.name callee |> FunctionCall.noArgs), loc)
          |> evalArgs ~args ~ctx
        | Some _kind ->
          (* TODO when kinds are extended in future: check that args matches with kind
             and create a function call with the appropriate arguments *)
          assert false
        | None -> expr |> expression ~ctx |> evalArgs ~args ~ctx)
    | Texp_apply {funct = expr; args} ->
      expr |> expression ~ctx |> evalArgs ~args ~ctx
    | Texp_let
        ( Recursive,
          [{vb_pat = {pat_desc = Tpat_var (id, _); pat_loc}; vb_expr}],
          inExpr ) ->
      let oldFunctionName = Ident.name id in
      let newFunctionName = currentFunctionName ^ "$" ^ oldFunctionName in
      functionTable |> FunctionTable.addFunction ~functionName:newFunctionName;
      let newFunctionDefinition =
        functionTable
        |> FunctionTable.getFunctionDefinition ~functionName:newFunctionName
      in
      let currentFunctionDefinition =
        functionTable
        |> FunctionTable.getFunctionDefinition ~functionName:currentFunctionName
      in
      newFunctionDefinition.kind <- currentFunctionDefinition.kind;
      let newCtx = {ctx with currentFunctionName = newFunctionName} in
      Hashtbl.replace ctx.innerRecursiveFunctions oldFunctionName
        newFunctionName;
      newFunctionDefinition.body <- Some (vb_expr |> expression ~ctx:newCtx);
      if !Common.Cli.debug then
        Log_.warning ~forStats:false ~loc:pat_loc
          (Termination
             {
               termination = TerminationAnalysisInternal;
               message =
                 Format.asprintf "Adding recursive definition @{<info>%s@}"
                   newFunctionName;
             });
      inExpr |> expression ~ctx
    | Texp_let (recFlag, valueBindings, inExpr) ->
      if recFlag = Recursive then Stats.logHygieneNoNestedLetRec ~loc;
      let commands =
        (valueBindings
        |> List.map (fun (vb : Typedtree.value_binding) ->
               vb.vb_expr |> expression ~ctx))
        @ [inExpr |> expression ~ctx]
      in
      Command.sequence commands
    | Texp_sequence (e1, e2) ->
      let open Command in
      expression ~ctx e1 +++ expression ~ctx e2
    | Texp_ifthenelse (e1, e2, eOpt) ->
      let c1 = e1 |> expression ~ctx in
      let c2 = e2 |> expression ~ctx in
      let c3 = eOpt |> expressionOpt ~ctx in
      let open Command in
      c1 +++ nondet [c2; c3]
    | Texp_constant _ -> Command.nothing
    | Texp_construct ({loc = {loc_ghost}}, {cstr_name}, expressions) -> (
      let c =
        expressions
        |> List.map (fun e -> e |> expression ~ctx)
        |> Command.unorderedSequence
      in
      match cstr_name with
      | "Some" when loc_ghost = false ->
        let open Command in
        c +++ ConstrOption Rsome
      | "None" when loc_ghost = false ->
        let open Command in
        c +++ ConstrOption Rnone
      | _ -> c)
    | Texp_function {case = case_} -> case ~ctx case_
    | Texp_match (e, casesOk, casesExn, _partial)
      when not
             (casesExn
             |> List.map (fun (case : Typedtree.case) -> case.c_lhs.pat_desc)
             != []) -> (
      (* No exceptions *)
      let cases = casesOk @ casesExn in
      let cE = e |> expression ~ctx in
      let cCases = cases |> List.map (case ~ctx) in
      let fail () =
        let open Command in
        cE +++ nondet cCases
      in
      match (cE, cases) with
      | ( Call (FunctionCall functionCall, loc),
          [{c_lhs = pattern1}; {c_lhs = pattern2}] ) -> (
        match (pattern1.pat_desc, pattern2.pat_desc) with
        | ( Tpat_construct (_, {cstr_name = ("Some" | "None") as name1}, _),
            Tpat_construct (_, {cstr_name = "Some" | "None"}, _) ) ->
          let casesArr = Array.of_list cCases in
          let some, none =
            try
              match name1 = "Some" with
              | true -> (casesArr.(0), casesArr.(1))
              | false -> (casesArr.(1), casesArr.(0))
            with Invalid_argument _ -> (Nothing, Nothing)
          in
          Command.SwitchOption {functionCall; loc; some; none}
        | _ -> fail ())
      | _ -> fail ())
    | Texp_match _ -> assert false (* exceptions *)
    | Texp_field (e, _lid, _desc) -> e |> expression ~ctx
    | Texp_record {fields; extended_expression} ->
      extended_expression
      :: (fields |> Array.to_list
         |> List.map
              (fun
                ( _desc,
                  (recordLabelDefinition : Typedtree.record_label_definition),
                  _ )
              ->
                match recordLabelDefinition with
                | Kept _typeExpr -> None
                | Overridden (_loc, e) -> Some e))
      |> List.map (expressionOpt ~ctx)
      |> Command.unorderedSequence
    | Texp_setfield (e1, _loc, _desc, e2) ->
      [e1; e2] |> List.map (expression ~ctx) |> Command.unorderedSequence
    | Texp_tuple expressions | Texp_array expressions ->
      expressions |> List.map (expression ~ctx) |> Command.unorderedSequence
    | Texp_assert _ -> Command.nothing
    | Texp_try (e, cases) ->
      let cE = e |> expression ~ctx in
      let cCases = cases |> List.map (case ~ctx) |> Command.nondet in
      let open Command in
      cE +++ cCases
    | Texp_variant (_label, eOpt) -> eOpt |> expressionOpt ~ctx
    | Texp_while _ ->
      notImplemented "Texp_while";
      assert false
    | Texp_for _ ->
      notImplemented "Texp_for";
      assert false
    | Texp_send _ ->
      notImplemented "Texp_send";
      assert false
    | Texp_letmodule _ ->
      notImplemented "Texp_letmodule";
      assert false
    | Texp_letexception _ ->
      notImplemented "Texp_letexception";
      assert false
    | Texp_pack _ ->
      notImplemented "Texp_pack";
      assert false
    | Texp_extension_constructor _ when true ->
      notImplemented "Texp_extension_constructor";
      assert false
    | _ ->
      (* ocaml 4.08: Texp_letop(_) | Texp_open(_) *)
      notImplemented "Texp_letop(_) | Texp_open(_)";
      assert false

  and expressionOpt ~ctx eOpt =
    match eOpt with
    | None -> Command.nothing
    | Some e -> e |> expression ~ctx

  and evalArgs ~args ~ctx command =
    (* Don't assume any evaluation order on the arguments *)
    let commands =
      args |> List.map (fun (_, eOpt) -> eOpt |> expressionOpt ~ctx)
    in
    let open Command in
    unorderedSequence commands +++ command

  and case : ctx:ctx -> Typedtree.case -> _ =
   fun ~ctx {c_guard; c_rhs} ->
    match c_guard with
    | None -> c_rhs |> expression ~ctx
    | Some e ->
      let open Command in
      expression ~ctx e +++ expression ~ctx c_rhs
end

module CallStack = struct
  type frame = {frameNumber: int; pos: Lexing.position}
  type t = {tbl: (FunctionCall.t, frame) Hashtbl.t; mutable size: int}

  let create () = {tbl = Hashtbl.create 1; size = 0}

  let toSet {tbl} =
    Hashtbl.fold
      (fun frame _i set -> FunctionCallSet.add frame set)
      tbl FunctionCallSet.empty

  let hasFunctionCall ~functionCall (t : t) = Hashtbl.mem t.tbl functionCall

  let addFunctionCall ~functionCall ~pos (t : t) =
    t.size <- t.size + 1;
    Hashtbl.replace t.tbl functionCall {frameNumber = t.size; pos}

  let removeFunctionCall ~functionCall (t : t) =
    t.size <- t.size - 1;
    Hashtbl.remove t.tbl functionCall

  let print ppf (t : t) =
    Format.fprintf ppf "  CallStack:";
    let frames =
      Hashtbl.fold
        (fun functionCall {frameNumber; pos} frames ->
          (functionCall, frameNumber, pos) :: frames)
        t.tbl []
      |> List.sort (fun (_, i1, _) (_, i2, _) -> i2 - i1)
    in
    frames
    |> List.iter (fun ((functionCall : FunctionCall.t), i, pos) ->
           Format.fprintf ppf "\n    @{<dim>%d@} %s (%a)" i
             (FunctionCall.toString functionCall)
             printPos pos)
end

module Eval = struct
  type progress = Progress.t
  type cache = (FunctionCall.t, State.t) Hashtbl.t

  let createCache () : cache = Hashtbl.create 1

  let lookupCache ~functionCall (cache : cache) =
    Hashtbl.find_opt cache functionCall

  let updateCache ~functionCall ~loc ~state (cache : cache) =
    Stats.logResult ~functionCall ~resString:(state |> State.toString) ~loc;
    if not (Hashtbl.mem cache functionCall) then
      Hashtbl.replace cache functionCall state

  let hasInfiniteLoop ~callStack ~functionCallToInstantiate ~functionCall ~loc
      ~state =
    if callStack |> CallStack.hasFunctionCall ~functionCall then (
      if state.State.progress = NoProgress then (
        Log_.error ~loc
          (Termination
             {
               termination = ErrorTermination;
               message =
                 Format.asprintf "%a"
                   (fun ppf () ->
                     Format.fprintf ppf "Possible infinite loop when calling ";
                     (match functionCallToInstantiate = functionCall with
                     | true ->
                       Format.fprintf ppf "@{<error>%s@}"
                         (functionCallToInstantiate |> FunctionCall.toString)
                     | false ->
                       Format.fprintf ppf "@{<error>%s@} which is @{<error>%s@}"
                         (functionCallToInstantiate |> FunctionCall.toString)
                         (functionCall |> FunctionCall.toString));
                     Format.fprintf ppf "@,%a" CallStack.print callStack)
                   ();
             });
        Stats.logLoop ());
      true)
    else false

  let rec runFunctionCall ~cache ~callStack ~functionArgs ~functionTable
      ~madeProgressOn ~loc ~state functionCallToInstantiate : State.t =
    let pos = loc.Location.loc_start in
    let functionCall =
      functionCallToInstantiate
      |> FunctionCall.applySubstitution ~sub:functionArgs
    in
    let functionName = functionCall.functionName in
    let call = Call.FunctionCall functionCall in
    let stateAfterCall =
      match cache |> lookupCache ~functionCall with
      | Some stateAfterCall ->
        Stats.logCache ~functionCall ~hit:true ~loc;
        {
          stateAfterCall with
          trace = Trace.Tcall (call, stateAfterCall.progress);
        }
      | None ->
        if FunctionCallSet.mem functionCall madeProgressOn then
          State.init ~progress:Progress ~trace:(Trace.Tcall (call, Progress)) ()
        else if
          hasInfiniteLoop ~callStack ~functionCallToInstantiate ~functionCall
            ~loc ~state
        then {state with trace = Trace.Tcall (call, state.progress)}
        else (
          Stats.logCache ~functionCall ~hit:false ~loc;
          let functionDefinition =
            functionTable |> FunctionTable.getFunctionDefinition ~functionName
          in
          callStack |> CallStack.addFunctionCall ~functionCall ~pos;
          let body =
            match functionDefinition.body with
            | Some body -> body
            | None -> assert false
          in
          let stateAfterCall =
            body
            |> run ~cache ~callStack ~functionArgs:functionCall.functionArgs
                 ~functionTable ~madeProgressOn ~state:(State.init ())
          in
          cache |> updateCache ~functionCall ~loc ~state:stateAfterCall;
          (* Invariant: run should restore the callStack *)
          callStack |> CallStack.removeFunctionCall ~functionCall;
          let trace = Trace.Tcall (call, stateAfterCall.progress) in
          {stateAfterCall with trace})
    in
    State.seq state stateAfterCall

  and run ~(cache : cache) ~callStack ~functionArgs ~functionTable
      ~madeProgressOn ~state (command : Command.t) : State.t =
    match command with
    | Call (FunctionCall functionCall, loc) ->
      functionCall
      |> runFunctionCall ~cache ~callStack ~functionArgs ~functionTable
           ~madeProgressOn ~loc ~state
    | Call ((ProgressFunction _ as call), _pos) ->
      let state1 =
        State.init ~progress:Progress ~trace:(Tcall (call, Progress)) ()
      in
      State.seq state state1
    | ConstrOption r ->
      let state1 =
        match r = Rsome with
        | true -> State.some ~progress:state.progress
        | false -> State.none ~progress:state.progress
      in
      State.seq state state1
    | Nothing ->
      let state1 = State.init () in
      State.seq state state1
    | Sequence commands ->
      (* if one command makes progress, then the sequence makes progress *)
      let rec findFirstProgress ~callStack ~commands ~madeProgressOn ~state =
        match commands with
        | [] -> state
        | c :: nextCommands ->
          let state1 =
            c
            |> run ~cache ~callStack ~functionArgs ~functionTable
                 ~madeProgressOn ~state
          in
          let madeProgressOn, callStack =
            match state1.progress with
            | Progress ->
              (* look for infinite loops in the rest of the sequence, remembering what has made progress *)
              ( FunctionCallSet.union madeProgressOn
                  (callStack |> CallStack.toSet),
                CallStack.create () )
            | NoProgress -> (madeProgressOn, callStack)
          in
          findFirstProgress ~callStack ~commands:nextCommands ~madeProgressOn
            ~state:state1
      in
      findFirstProgress ~callStack ~commands ~madeProgressOn ~state
    | UnorderedSequence commands ->
      let stateNoTrace = {state with trace = Trace.empty} in
      (* the commands could be executed in any order: progess if any one does *)
      let states =
        commands
        |> List.map (fun c ->
               c
               |> run ~cache ~callStack ~functionArgs ~functionTable
                    ~madeProgressOn ~state:stateNoTrace)
      in
      State.seq state (states |> State.unorderedSequence)
    | Nondet commands ->
      let stateNoTrace = {state with trace = Trace.empty} in
      (* the commands could be executed in any order: progess if any one does *)
      let states =
        commands
        |> List.map (fun c ->
               c
               |> run ~cache ~callStack ~functionArgs ~functionTable
                    ~madeProgressOn ~state:stateNoTrace)
      in
      State.seq state (states |> State.nondet)
    | SwitchOption {functionCall; loc; some; none} -> (
      let stateAfterCall =
        functionCall
        |> runFunctionCall ~cache ~callStack ~functionArgs ~functionTable
             ~madeProgressOn ~loc ~state
      in
      match stateAfterCall.valuesOpt with
      | None ->
        Command.nondet [some; none]
        |> run ~cache ~callStack ~functionArgs ~functionTable ~madeProgressOn
             ~state:stateAfterCall
      | Some values ->
        let runOpt c progressOpt =
          match progressOpt with
          | None -> State.init ~progress:Progress ()
          | Some progress ->
            c
            |> run ~cache ~callStack ~functionArgs ~functionTable
                 ~madeProgressOn ~state:(State.init ~progress ())
        in
        let stateNone = values |> Values.getNone |> runOpt none in
        let stateSome = values |> Values.getSome |> runOpt some in
        State.seq stateAfterCall (State.nondet [stateSome; stateNone]))

  let analyzeFunction ~cache ~functionTable ~loc functionName =
    if !Common.Cli.debug then
      Log_.log "@[<v 2>@,@{<warning>Termination Analysis@} for @{<info>%s@}@]@."
        functionName;
    let pos = loc.Location.loc_start in
    let callStack = CallStack.create () in
    let functionArgs = FunctionArgs.empty in
    let functionCall = FunctionCall.noArgs functionName in
    callStack |> CallStack.addFunctionCall ~functionCall ~pos;
    let functionDefinition =
      functionTable |> FunctionTable.getFunctionDefinition ~functionName
    in
    if functionDefinition.kind <> Kind.empty then
      Stats.logHygieneParametric ~functionName ~loc
    else
      let body =
        match functionDefinition.body with
        | Some body -> body
        | None -> assert false
      in
      let state =
        body
        |> run ~cache ~callStack ~functionArgs ~functionTable
             ~madeProgressOn:FunctionCallSet.empty ~state:(State.init ())
      in
      cache |> updateCache ~functionCall ~loc ~state
end

let progressFunctionsFromAttributes attributes =
  let lidToString lid = lid |> Longident.flatten |> String.concat "." in
  let isProgress = ( = ) "progress" in
  if attributes |> Annotation.hasAttribute isProgress then
    Some
      (match attributes |> Annotation.getAttributePayload isProgress with
      | None -> []
      | Some (IdentPayload lid) -> [lidToString lid]
      | Some (TuplePayload l) ->
        l
        |> List.filter_map (function
             | Annotation.IdentPayload lid -> Some (lidToString lid)
             | _ -> None)
      | _ -> [])
  else None

let traverseAst ~valueBindingsTable =
  let super = Tast_mapper.default in
  let value_bindings (self : Tast_mapper.mapper) (recFlag, valueBindings) =
    (* Update the table of value bindings for variables *)
    valueBindings
    |> List.iter (fun (vb : Typedtree.value_binding) ->
           match vb.vb_pat.pat_desc with
           | Tpat_var (id, {loc = {loc_start = pos}}) ->
             let callees = lazy (FindFunctionsCalled.findCallees vb.vb_expr) in
             Hashtbl.replace valueBindingsTable (Ident.name id)
               (pos, vb.vb_expr, callees)
           | _ -> ());
    let progressFunctions, functionsToAnalyze =
      if recFlag = Asttypes.Nonrecursive then (StringSet.empty, [])
      else
        let progressFunctions0, functionsToAnalyze0 =
          valueBindings
          |> List.fold_left
               (fun (progressFunctions, functionsToAnalyze)
                    (valueBinding : Typedtree.value_binding) ->
                 match
                   progressFunctionsFromAttributes valueBinding.vb_attributes
                 with
                 | None -> (progressFunctions, functionsToAnalyze)
                 | Some newProgressFunctions ->
                   ( StringSet.union
                       (StringSet.of_list newProgressFunctions)
                       progressFunctions,
                     match valueBinding.vb_pat.pat_desc with
                     | Tpat_var (id, _) ->
                       (Ident.name id, valueBinding.vb_expr.exp_loc)
                       :: functionsToAnalyze
                     | _ -> functionsToAnalyze ))
               (StringSet.empty, [])
        in
        (progressFunctions0, functionsToAnalyze0 |> List.rev)
    in
    if functionsToAnalyze <> [] then (
      let functionTable = FunctionTable.create () in
      let isProgressFunction path =
        StringSet.mem (Path.name path) progressFunctions
      in
      let recursiveFunctions =
        List.fold_left
          (fun defs (valueBinding : Typedtree.value_binding) ->
            match valueBinding.vb_pat.pat_desc with
            | Tpat_var (id, _) -> Ident.name id :: defs
            | _ -> defs)
          [] valueBindings
        |> List.rev
      in
      let recursiveDefinitions =
        recursiveFunctions
        |> List.fold_left
             (fun acc functionName ->
               match Hashtbl.find_opt valueBindingsTable functionName with
               | Some (_pos, e, _set) -> (functionName, e) :: acc
               | None -> acc)
             []
        |> List.rev
      in
      recursiveDefinitions
      |> List.iter (fun (functionName, _body) ->
             functionTable |> FunctionTable.addFunction ~functionName);
      recursiveDefinitions
      |> List.iter (fun (_, body) ->
             body
             |> ExtendFunctionTable.run ~functionTable ~progressFunctions
                  ~valueBindingsTable);
      recursiveDefinitions
      |> List.iter (fun (_, body) ->
             body
             |> CheckExpressionWellFormed.run ~functionTable ~valueBindingsTable);
      functionTable
      |> Hashtbl.iter
           (fun
             functionName
             (functionDefinition : FunctionTable.functionDefinition)
           ->
             if functionDefinition.body = None then
               match Hashtbl.find_opt valueBindingsTable functionName with
               | None -> ()
               | Some (_pos, body, _) ->
                 functionTable
                 |> FunctionTable.addBody
                      ~body:
                        (Some
                           (body
                           |> Compile.expression
                                ~ctx:
                                  {
                                    currentFunctionName = functionName;
                                    functionTable;
                                    innerRecursiveFunctions = Hashtbl.create 1;
                                    isProgressFunction;
                                  }))
                      ~functionName);
      if !Common.Cli.debug then FunctionTable.dump functionTable;
      let cache = Eval.createCache () in
      functionsToAnalyze
      |> List.iter (fun (functionName, loc) ->
             functionName |> Eval.analyzeFunction ~cache ~functionTable ~loc);
      Stats.newRecursiveFunctions ~numFunctions:(Hashtbl.length functionTable));
    valueBindings
    |> List.iter (fun valueBinding ->
           super.value_binding self valueBinding |> ignore);
    (recFlag, valueBindings)
  in
  {super with Tast_mapper.value_bindings}

let processStructure (structure : Typedtree.structure) =
  Stats.newFile ();
  let valueBindingsTable = Hashtbl.create 1 in
  let traverseAst = traverseAst ~valueBindingsTable in
  structure |> traverseAst.structure traverseAst |> ignore

let processCmt (cmt_infos : Cmt_format.cmt_infos) =
  match cmt_infos.cmt_annots with
  | Interface _ -> ()
  | Implementation structure -> processStructure structure
  | _ -> ()

let reportStats () = Stats.dump ~ppf:Format.std_formatter
