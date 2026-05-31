let print_pos ppf (pos : Lexing.position) =
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
  type arg = {label: string; function_name: FunctionName.t}
  type t = arg list

  let empty = []
  let arg_to_string {label; function_name} = label ^ ":" ^ function_name

  let to_string function_args =
    match function_args = [] with
    | true -> ""
    | false ->
      "<" ^ (function_args |> List.map arg_to_string |> String.concat ",") ^ ">"

  let find (t : t) ~label =
    match t |> List.find_opt (fun arg -> arg.label = label) with
    | Some {function_name} -> Some function_name
    | None -> None

  let compare_arg a1 a2 =
    let n = compare a1.label a2.label in
    if n <> 0 then n else compare a1.function_name a2.function_name

  let rec compare l1 l2 =
    match (l1, l2) with
    | [], [] -> 0
    | [], _ :: _ -> -1
    | _ :: _, [] -> 1
    | x1 :: l1, x2 :: l2 ->
      let n = compare_arg x1 x2 in
      if n <> 0 then n else compare l1 l2
end

module FunctionCall = struct
  type t = {function_name: FunctionName.t; function_args: FunctionArgs.t}

  let substitute_name ~sub name =
    match sub |> FunctionArgs.find ~label:name with
    | Some function_name -> function_name
    | None -> name

  let apply_substitution ~(sub : FunctionArgs.t) (t : t) =
    if sub = [] then t
    else
      {
        function_name = t.function_name |> substitute_name ~sub;
        function_args =
          t.function_args
          |> List.map (fun (arg : FunctionArgs.arg) ->
                 {
                   arg with
                   function_name = arg.function_name |> substitute_name ~sub;
                 });
      }

  let no_args function_name = {function_name; function_args = []}

  let to_string {function_name; function_args} =
    function_name ^ FunctionArgs.to_string function_args

  let compare (x1 : t) x2 =
    let n = compare x1.function_name x2.function_name in
    if n <> 0 then n else FunctionArgs.compare x1.function_args x2.function_args
end

module FunctionCallSet = Set.Make (FunctionCall)

module Stats = struct
  let n_cache_checks = ref 0
  let n_cache_hits = ref 0
  let n_files = ref 0
  let n_functions = ref 0
  let n_hygiene_errors = ref 0
  let n_infinite_loops = ref 0
  let n_recursive_blocks = ref 0

  let print ppf () =
    Format.fprintf ppf "@[<v 2>@,@{<warning>Termination Analysis Stats@}@,";
    Format.fprintf ppf "Files:@{<dim>%d@}@," !n_files;
    Format.fprintf ppf "Recursive Blocks:@{<dim>%d@}@," !n_recursive_blocks;
    Format.fprintf ppf "Functions:@{<dim>%d@}@," !n_functions;
    Format.fprintf ppf "Infinite Loops:@{<dim>%d@}@," !n_infinite_loops;
    Format.fprintf ppf "Hygiene Errors:@{<dim>%d@}@," !n_hygiene_errors;
    Format.fprintf ppf "Cache Hits:@{<dim>%d@}/@{<dim>%d@}@," !n_cache_hits
      !n_cache_checks;
    Format.fprintf ppf "@]"

  let dump ~ppf = Format.fprintf ppf "%a@." print ()
  let new_file () = incr n_files

  let new_recursive_functions ~num_functions =
    incr n_recursive_blocks;
    n_functions := !n_functions + num_functions

  let log_loop () = incr n_infinite_loops

  let log_cache ~config ~function_call ~hit ~loc =
    incr n_cache_checks;
    if hit then incr n_cache_hits;
    if config.DceConfig.cli.debug then
      Log_.warning ~for_stats:false ~loc
        (Termination
           {
             termination = TerminationAnalysisInternal;
             message =
               Format.asprintf "Cache %s for @{<info>%s@}"
                 (match hit with
                 | true -> "hit"
                 | false -> "miss")
                 (FunctionCall.to_string function_call);
           })

  let log_result ~config ~function_call ~loc ~res_string =
    if config.DceConfig.cli.debug then
      Log_.warning ~for_stats:false ~loc
        (Termination
           {
             termination = TerminationAnalysisInternal;
             message =
               Format.asprintf "@{<info>%s@} returns %s"
                 (FunctionCall.to_string function_call)
                 res_string;
           })

  let log_hygiene_parametric ~function_name ~loc =
    incr n_hygiene_errors;
    Log_.error ~loc
      (Termination
         {
           termination = ErrorHygiene;
           message =
             Format.asprintf
               "@{<error>%s@} cannot be analyzed directly as it is parametric"
               function_name;
         })

  let log_hygiene_only_call_directly ~path ~loc =
    incr n_hygiene_errors;
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

  let log_hygiene_must_have_named_argument ~label ~loc =
    incr n_hygiene_errors;
    Log_.error ~loc
      (Termination
         {
           termination = ErrorHygiene;
           message =
             Format.asprintf "Call must have named argument @{<error>%s@}" label;
         })

  let log_hygiene_named_arg_value ~label ~loc =
    incr n_hygiene_errors;
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

  let log_hygiene_no_nested_let_rec ~loc =
    incr n_hygiene_errors;
    Log_.error ~loc
      (Termination
         {
           termination = ErrorHygiene;
           message = Format.asprintf "Nested multiple let rec not supported yet";
         })
end

module Progress = struct
  type t = Progress | NoProgress

  let to_string progress =
    match progress = Progress with
    | true -> "Progress"
    | false -> "NoProgress"
end

module Call = struct
  type progress_function = Path.t

  type t =
    | FunctionCall of FunctionCall.t
    | ProgressFunction of progress_function

  let to_string call =
    match call with
    | ProgressFunction progress_function -> "+" ^ Path.name progress_function
    | FunctionCall function_call -> FunctionCall.to_string function_call
end

module Trace = struct
  type ret_option = Rsome | Rnone

  type t =
    | Tcall of Call.t * Progress.t
    | Tnondet of t list
    | Toption of ret_option
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

  let ret_option_to_string r =
    match r = Rsome with
    | true -> "Some"
    | false -> "None"

  let rec to_string trace =
    match trace with
    | Tcall (ProgressFunction progress_function, progress) ->
      Path.name progress_function ^ ":" ^ Progress.to_string progress
    | Tcall (FunctionCall function_call, progress) ->
      FunctionCall.to_string function_call ^ ":" ^ Progress.to_string progress
    | Tnondet traces ->
      "[" ^ (traces |> List.map to_string |> String.concat " || ") ^ "]"
    | Toption ret_option -> ret_option |> ret_option_to_string
    | Tseq traces -> (
      let traces_not_empty = traces |> List.filter (( <> ) empty) in
      match traces_not_empty with
      | [] -> "_"
      | [t] -> t |> to_string
      | _ :: _ -> traces_not_empty |> List.map to_string |> String.concat "; ")
end

module Values : sig
  type t

  val get_none : t -> Progress.t option
  val get_some : t -> Progress.t option
  val nd : t -> t -> t
  val none : progress:Progress.t -> t
  val some : progress:Progress.t -> t
  val to_string : t -> string
end = struct
  type t = {none: Progress.t option; some: Progress.t option}

  let get_none {none} = none
  let get_some {some} = some

  let to_string x =
    ((match x.some with
     | None -> []
     | Some p -> ["some: " ^ Progress.to_string p])
    @
    match x.none with
    | None -> []
    | Some p -> ["none: " ^ Progress.to_string p])
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
      | None, progress_opt | progress_opt, None -> progress_opt
    in
    let none = combine v1.none v2.none in
    let some = combine v1.some v2.some in
    {none; some}
end

module State = struct
  type t = {progress: Progress.t; trace: Trace.t; values_opt: Values.t option}

  let to_string {progress; trace; values_opt} =
    let progress_str =
      match values_opt with
      | None -> progress |> Progress.to_string
      | Some values -> "{" ^ (values |> Values.to_string) ^ "}"
    in
    progress_str ^ " with trace " ^ Trace.to_string trace

  let init ?(progress = Progress.NoProgress) ?(trace = Trace.empty)
      ?(values_opt = None) () =
    {progress; trace; values_opt}

  let seq s1 s2 =
    let progress =
      match s1.progress = Progress || s2.progress = Progress with
      | true -> Progress.Progress
      | false -> NoProgress
    in
    let trace = Trace.seq s1.trace s2.trace in
    let values_opt = s2.values_opt in
    {progress; trace; values_opt}

  let sequence states =
    match states with
    | [] -> assert false
    | s :: next_states -> List.fold_left seq s next_states

  let nd s1 s2 =
    let progress =
      match s1.progress = Progress && s2.progress = Progress with
      | true -> Progress.Progress
      | false -> NoProgress
    in
    let trace = Trace.nd s1.trace s2.trace in
    let values_opt =
      match (s1.values_opt, s2.values_opt) with
      | None, values_opt -> (
        match s1.progress = Progress with
        | true -> values_opt
        | false -> None)
      | values_opt, None -> (
        match s2.progress = Progress with
        | true -> values_opt
        | false -> None)
      | Some values1, Some values2 -> Some (Values.nd values1 values2)
    in
    {progress; trace; values_opt}

  let nondet states =
    match states with
    | [] -> assert false
    | s :: next_states -> List.fold_left nd s next_states

  let unordered_sequence states = {(states |> sequence) with values_opt = None}

  let none ~progress =
    init ~progress ~trace:Trace.none
      ~values_opt:(Some (Values.none ~progress))
      ()

  let some ~progress =
    init ~progress ~trace:Trace.some
      ~values_opt:(Some (Values.some ~progress))
      ()
end

module Command = struct
  type progress = Progress.t
  type ret_option = Trace.ret_option

  type t =
    | Call of Call.t * Location.t
    | ConstrOption of ret_option
    | Nondet of t list
    | Nothing
    | Sequence of t list
    | SwitchOption of {
        function_call: FunctionCall.t;
        loc: Location.t;
        some: t;
        none: t;
      }
    | UnorderedSequence of t list

  let rec to_string command =
    match command with
    | Call (call, _pos) -> call |> Call.to_string
    | ConstrOption r -> r |> Trace.ret_option_to_string
    | Nondet commands ->
      "[" ^ (commands |> List.map to_string |> String.concat " || ") ^ "]"
    | Nothing -> "_"
    | Sequence commands -> commands |> List.map to_string |> String.concat "; "
    | SwitchOption {function_call; some = c_some; none = c_none} ->
      "switch "
      ^ FunctionCall.to_string function_call
      ^ " {some: " ^ to_string c_some ^ ", none: " ^ to_string c_none ^ "}"
    | UnorderedSequence commands ->
      "{" ^ (commands |> List.map to_string |> String.concat ", ") ^ "}"

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

  let unordered_sequence commands =
    let relevant_commands = commands |> List.filter (fun x -> x <> nothing) in
    match relevant_commands with
    | [] -> nothing
    | [c] -> c
    | _ :: _ :: _ -> UnorderedSequence relevant_commands
end

module Kind = struct
  type t = entry list
  and entry = {label: string; k: t}

  let empty = ([] : t)

  let has_label ~label (k : t) =
    k |> List.exists (fun entry -> entry.label = label)

  let rec entry_to_string {label; k} =
    match k = [] with
    | true -> label
    | false -> label ^ ":" ^ (k |> to_string)

  and to_string (kind : t) =
    match kind = [] with
    | true -> ""
    | false ->
      "<" ^ (kind |> List.map entry_to_string |> String.concat ", ") ^ ">"

  let add_label_with_empty_kind ~label kind =
    if not (kind |> has_label ~label) then
      {label; k = empty} :: kind |> List.sort compare
    else kind
end

module FunctionTable = struct
  type function_definition = {
    mutable body: Command.t option;
    mutable kind: Kind.t;
  }

  type t = (FunctionName.t, function_definition) Hashtbl.t

  let create () : t = Hashtbl.create 1

  let print ppf (tbl : t) =
    Format.fprintf ppf "@[<v 2>@,@{<warning>Function Table@}";
    let definitions =
      Hashtbl.fold
        (fun function_name {kind; body} definitions ->
          (function_name, kind, body) :: definitions)
        tbl []
      |> List.sort (fun (fn1, _, _) (fn2, _, _) -> String.compare fn1 fn2)
    in
    definitions
    |> List.iteri (fun i (function_name, kind, body) ->
           Format.fprintf ppf "@,@{<dim>%d@} @{<info>%s%s@}: %s" (i + 1)
             function_name (Kind.to_string kind)
             (match body with
             | Some command -> Command.to_string command
             | None -> "None"));
    Format.fprintf ppf "@]"

  let dump tbl = Format.fprintf Format.std_formatter "%a@." print tbl
  let initial_function_definition () = {kind = Kind.empty; body = None}

  let get_function_definition ~function_name (tbl : t) =
    try Hashtbl.find tbl function_name with Not_found -> assert false

  let is_in_function_in_table ~function_table path =
    Hashtbl.mem function_table (Path.name path)

  let add_function ~function_name (tbl : t) =
    if Hashtbl.mem tbl function_name then assert false;
    Hashtbl.replace tbl function_name (initial_function_definition ())

  let add_label_to_kind ~function_name ~label (tbl : t) =
    let function_definition = tbl |> get_function_definition ~function_name in
    function_definition.kind <-
      function_definition.kind |> Kind.add_label_with_empty_kind ~label

  let add_body ~body ~function_name (tbl : t) =
    let function_definition = tbl |> get_function_definition ~function_name in
    function_definition.body <- body

  let function_get_kind_of_label ~function_name ~label (tbl : t) =
    match Hashtbl.find tbl function_name with
    | {kind} -> (
      match kind |> Kind.has_label ~label with
      | true -> Some Kind.empty
      | false -> None)
    | exception Not_found -> None
end

module FindFunctionsCalled = struct
  let traverse_expr ~callees =
    let super = Tast_mapper.default in
    let expr (self : Tast_mapper.mapper) (e : Typedtree.expression) =
      (match e.exp_desc with
      | Texp_apply {funct = {exp_desc = Texp_ident (callee, _, _)}} ->
        let function_name = Path.name callee in
        callees := !callees |> StringSet.add function_name
      | _ -> ());
      super.expr self e
    in
    {super with Tast_mapper.expr}

  let find_callees (expression : Typedtree.expression) =
    let is_function =
      match expression.exp_desc with
      | Texp_function {arity = None} -> true
      | _ -> false
    in
    let callees = ref StringSet.empty in
    let traverse_expr = traverse_expr ~callees in
    if is_function then expression |> traverse_expr.expr traverse_expr |> ignore;
    !callees
end

module ExtendFunctionTable = struct
  (* Add functions passed a recursive function via a labeled argument,
     and functions calling progress functions, to the function table. *)
  let extract_labelled_argument ?(kind_opt = None)
      (arg_opt : Typedtree.expression option) =
    match arg_opt with
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
      when kind_opt <> None ->
      let check_arg ((arg_label : Asttypes.arg_label), _argOpt) =
        match (arg_label, kind_opt) with
        | (Labelled {txt = l} | Optional {txt = l}), Some kind ->
          kind |> List.for_all (fun {Kind.label} -> label <> l)
        | _ -> true
      in
      if args |> List.for_all check_arg then Some (path, loc) else None
    | _ -> None

  let traverse_expr ~config ~function_table ~progress_functions ~value_bindings_table
      =
    let super = Tast_mapper.default in
    let expr (self : Tast_mapper.mapper) (e : Typedtree.expression) =
      (match e.exp_desc with
      | Texp_ident (callee, _, _) -> (
        let loc = e.exp_loc in
        match Hashtbl.find_opt value_bindings_table (Path.name callee) with
        | None -> ()
        | Some (id_pos, _, callees) ->
          if
            not
              (StringSet.is_empty
                 (StringSet.inter (Lazy.force callees) progress_functions))
          then
            let function_name = Path.name callee in
            if not (callee |> FunctionTable.is_in_function_in_table ~function_table)
            then (
              function_table |> FunctionTable.add_function ~function_name;
              if config.DceConfig.cli.debug then
                Log_.warning ~for_stats:false ~loc
                  (Termination
                     {
                       termination = TerminationAnalysisInternal;
                       message =
                         Format.asprintf
                           "Extend Function Table with @{<info>%s@} (%a) as it \
                            calls a progress function"
                           function_name print_pos id_pos;
                     })))
      | Texp_apply {funct = {exp_desc = Texp_ident (callee, _, _)}; args}
        when callee |> FunctionTable.is_in_function_in_table ~function_table ->
        let function_name = Path.name callee in
        args
        |> List.iter (fun ((arg_label : Asttypes.arg_label), arg_opt) ->
               match (arg_label, arg_opt |> extract_labelled_argument) with
               | Labelled {txt = label}, Some (path, loc)
                 when path |> FunctionTable.is_in_function_in_table ~function_table
                 ->
                 function_table
                 |> FunctionTable.add_label_to_kind ~function_name ~label;
                 if config.DceConfig.cli.debug then
                   Log_.warning ~for_stats:false ~loc
                     (Termination
                        {
                          termination = TerminationAnalysisInternal;
                          message =
                            Format.asprintf
                              "@{<info>%s@} is parametric \
                               ~@{<info>%s@}=@{<info>%s@}"
                              function_name label (Path.name path);
                        })
               | _ -> ())
      | _ -> ());
      super.expr self e
    in
    {super with Tast_mapper.expr}

  let run ~config ~function_table ~progress_functions ~value_bindings_table
      (expression : Typedtree.expression) =
    let traverse_expr =
      traverse_expr ~config ~function_table ~progress_functions ~value_bindings_table
    in
    expression |> traverse_expr.expr traverse_expr |> ignore
end

module CheckExpressionWellFormed = struct
  let traverse_expr ~config ~function_table ~value_bindings_table =
    let super = Tast_mapper.default in
    let check_ident ~path ~loc =
      if path |> FunctionTable.is_in_function_in_table ~function_table then
        Stats.log_hygiene_only_call_directly ~path ~loc
    in
    let expr (self : Tast_mapper.mapper) (e : Typedtree.expression) =
      match e.exp_desc with
      | Texp_ident (path, {loc}, _) ->
        check_ident ~path ~loc;
        e
      | Texp_apply {funct = {exp_desc = Texp_ident (function_path, _, _)}; args}
        ->
        let function_name = Path.name function_path in
        args
        |> List.iter (fun ((arg_label : Asttypes.arg_label), arg_opt) ->
               match arg_opt |> ExtendFunctionTable.extract_labelled_argument with
               | Some (path, loc) -> (
                 match arg_label with
                 | Labelled {txt = label} -> (
                   if
                     function_table
                     |> FunctionTable.function_get_kind_of_label ~function_name
                          ~label
                     <> None
                   then ()
                   else
                     match Hashtbl.find_opt value_bindings_table function_name with
                     | Some (_pos, (body : Typedtree.expression), _)
                       when path
                            |> FunctionTable.is_in_function_in_table ~function_table
                       ->
                       let in_table =
                         function_path
                         |> FunctionTable.is_in_function_in_table ~function_table
                       in
                       if not in_table then
                         function_table
                         |> FunctionTable.add_function ~function_name;
                       function_table
                       |> FunctionTable.add_label_to_kind ~function_name ~label;
                       if config.DceConfig.cli.debug then
                         Log_.warning ~for_stats:false ~loc:body.exp_loc
                           (Termination
                              {
                                termination = TerminationAnalysisInternal;
                                message =
                                  Format.asprintf
                                    "Extend Function Table with @{<info>%s@} \
                                     as parametric ~@{<info>%s@}=@{<info>%s@}"
                                    function_name label (Path.name path);
                              })
                     | _ -> check_ident ~path ~loc)
                 | Optional _ | Nolabel -> check_ident ~path ~loc)
               | _ -> ());
        e
      | _ -> super.expr self e
    in
    {super with Tast_mapper.expr}

  let run ~config ~function_table ~value_bindings_table
      (expression : Typedtree.expression) =
    let traverse_expr =
      traverse_expr ~config ~function_table ~value_bindings_table
    in
    expression |> traverse_expr.expr traverse_expr |> ignore
end

module Compile = struct
  type ctx = {
    config: DceConfig.t;
    current_function_name: FunctionName.t;
    function_table: FunctionTable.t;
    inner_recursive_functions: (FunctionName.t, FunctionName.t) Hashtbl.t;
    is_progress_function: Path.t -> bool;
  }

  let rec expression ~ctx (expr : Typedtree.expression) =
    let {config; current_function_name; function_table; is_progress_function} =
      ctx
    in
    let loc = expr.exp_loc in
    let not_implemented case =
      Log_.error ~loc
        (Termination
           {termination = ErrorNotImplemented; message = Format.asprintf case})
    in

    match expr.exp_desc with
    | Texp_ident _ -> Command.nothing
    | Texp_apply
        {
          funct = {exp_desc = Texp_ident (callee_to_rename, l, vd)} as expr;
          args = args_to_extend;
        } -> (
      let callee, args =
        match
          Hashtbl.find_opt ctx.inner_recursive_functions
            (Path.name callee_to_rename)
        with
        | Some inner_function_name ->
          let inner_function_definition =
            function_table
            |> FunctionTable.get_function_definition
                 ~function_name:inner_function_name
          in
          let args_from_kind =
            inner_function_definition.kind
            |> List.map (fun (entry : Kind.entry) ->
                   ( Asttypes.Labelled {txt = entry.label; loc = Location.none},
                     Some
                       {
                         expr with
                         exp_desc =
                           Texp_ident
                             (Path.Pident (Ident.create entry.label), l, vd);
                       } ))
          in
          ( Path.Pident (Ident.create inner_function_name),
            args_from_kind @ args_to_extend )
        | None -> (callee_to_rename, args_to_extend)
      in
      if callee |> FunctionTable.is_in_function_in_table ~function_table then
        let function_name = Path.name callee in
        let function_definition =
          function_table |> FunctionTable.get_function_definition ~function_name
        in
        let exception ArgError in
        let get_function_arg {Kind.label} =
          let arg_opt =
            args
            |> List.find_opt (fun arg ->
                   match arg with
                   | Asttypes.Labelled {txt = s}, Some _ -> s = label
                   | _ -> false)
          in
          let arg_opt =
            match arg_opt with
            | Some (_, Some e) -> Some e
            | _ -> None
          in
          let function_arg () =
            match
              arg_opt
              |> ExtendFunctionTable.extract_labelled_argument
                   ~kind_opt:(Some function_definition.kind)
            with
            | None ->
              Stats.log_hygiene_must_have_named_argument ~label ~loc;
              raise ArgError
            | Some (path, _pos)
              when path |> FunctionTable.is_in_function_in_table ~function_table ->
              let function_name = Path.name path in
              {FunctionArgs.label; function_name}
            | Some (path, _pos)
              when function_table
                   |> FunctionTable.function_get_kind_of_label
                        ~function_name:current_function_name
                        ~label:(Path.name path)
                   = Some []
                   (* TODO: when kinds are inferred, support and check non-empty kinds *)
              ->
              let function_name = Path.name path in
              {FunctionArgs.label; function_name}
            | _ ->
              Stats.log_hygiene_named_arg_value ~label ~loc;
              raise ArgError
              [@@raises ArgError]
          in
          function_arg ()
            [@@raises ArgError]
        in
        let function_args_opt =
          try Some (function_definition.kind |> List.map get_function_arg)
          with ArgError -> None
        in
        match function_args_opt with
        | None -> Command.nothing
        | Some function_args ->
          Command.Call (FunctionCall {function_name; function_args}, loc)
          |> eval_args ~args ~ctx
      else if callee |> is_progress_function then
        Command.Call (ProgressFunction callee, loc) |> eval_args ~args ~ctx
      else
        match
          function_table
          |> FunctionTable.function_get_kind_of_label
               ~function_name:current_function_name ~label:(Path.name callee)
        with
        | Some kind when kind = Kind.empty ->
          Command.Call
            (FunctionCall (Path.name callee |> FunctionCall.no_args), loc)
          |> eval_args ~args ~ctx
        | Some _kind ->
          (* TODO when kinds are extended in future: check that args matches with kind
             and create a function call with the appropriate arguments *)
          assert false
        | None -> expr |> expression ~ctx |> eval_args ~args ~ctx)
    | Texp_apply {funct = expr; args} ->
      expr |> expression ~ctx |> eval_args ~args ~ctx
    | Texp_let
        ( Recursive,
          [{vb_pat = {pat_desc = Tpat_var (id, _); pat_loc}; vb_expr}],
          in_expr ) ->
      let old_function_name = Ident.name id in
      let new_function_name = current_function_name ^ "$" ^ old_function_name in
      function_table |> FunctionTable.add_function ~function_name:new_function_name;
      let new_function_definition =
        function_table
        |> FunctionTable.get_function_definition ~function_name:new_function_name
      in
      let current_function_definition =
        function_table
        |> FunctionTable.get_function_definition ~function_name:current_function_name
      in
      new_function_definition.kind <- current_function_definition.kind;
      let new_ctx = {ctx with current_function_name = new_function_name} in
      Hashtbl.replace ctx.inner_recursive_functions old_function_name
        new_function_name;
      new_function_definition.body <- Some (vb_expr |> expression ~ctx:new_ctx);
      if config.DceConfig.cli.debug then
        Log_.warning ~for_stats:false ~loc:pat_loc
          (Termination
             {
               termination = TerminationAnalysisInternal;
               message =
                 Format.asprintf "Adding recursive definition @{<info>%s@}"
                   new_function_name;
             });
      in_expr |> expression ~ctx
    | Texp_let (rec_flag, value_bindings, in_expr) ->
      if rec_flag = Recursive then Stats.log_hygiene_no_nested_let_rec ~loc;
      let commands =
        (value_bindings
        |> List.map (fun (vb : Typedtree.value_binding) ->
               vb.vb_expr |> expression ~ctx))
        @ [in_expr |> expression ~ctx]
      in
      Command.sequence commands
    | Texp_sequence (e1, e2) ->
      let open Command in
      expression ~ctx e1 +++ expression ~ctx e2
    | Texp_ifthenelse (e1, e2, e_opt) ->
      let c1 = e1 |> expression ~ctx in
      let c2 = e2 |> expression ~ctx in
      let c3 = e_opt |> expression_opt ~ctx in
      let open Command in
      c1 +++ nondet [c2; c3]
    | Texp_constant _ -> Command.nothing
    | Texp_construct ({loc = {loc_ghost}}, {cstr_name}, expressions) -> (
      let c =
        expressions
        |> List.map (fun e -> e |> expression ~ctx)
        |> Command.unordered_sequence
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
    | Texp_match (e, cases_ok, cases_exn, _partial)
      when not
             (cases_exn
             |> List.map (fun (case : Typedtree.case) -> case.c_lhs.pat_desc)
             != []) -> (
      (* No exceptions *)
      let cases = cases_ok @ cases_exn in
      let c_e = e |> expression ~ctx in
      let c_cases = cases |> List.map (case ~ctx) in
      let fail () =
        let open Command in
        c_e +++ nondet c_cases
      in
      match (c_e, cases) with
      | ( Call (FunctionCall function_call, loc),
          [{c_lhs = pattern1}; {c_lhs = pattern2}] ) -> (
        match (pattern1.pat_desc, pattern2.pat_desc) with
        | ( Tpat_construct (_, {cstr_name = ("Some" | "None") as name1}, _),
            Tpat_construct (_, {cstr_name = "Some" | "None"}, _) ) ->
          let cases_arr = Array.of_list c_cases in
          let some, none =
            try
              match name1 = "Some" with
              | true -> (cases_arr.(0), cases_arr.(1))
              | false -> (cases_arr.(1), cases_arr.(0))
            with Invalid_argument _ -> (Nothing, Nothing)
          in
          Command.SwitchOption {function_call; loc; some; none}
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
                  (record_label_definition : Typedtree.record_label_definition),
                  _ )
              ->
                match record_label_definition with
                | Kept _typeExpr -> None
                | Overridden (_loc, e) -> Some e))
      |> List.map (expression_opt ~ctx)
      |> Command.unordered_sequence
    | Texp_setfield (e1, _loc, _desc, e2) ->
      [e1; e2] |> List.map (expression ~ctx) |> Command.unordered_sequence
    | Texp_tuple expressions | Texp_array expressions ->
      expressions |> List.map (expression ~ctx) |> Command.unordered_sequence
    | Texp_assert _ -> Command.nothing
    | Texp_try (e, cases) ->
      let c_e = e |> expression ~ctx in
      let c_cases = cases |> List.map (case ~ctx) |> Command.nondet in
      let open Command in
      c_e +++ c_cases
    | Texp_variant (_label, e_opt) -> e_opt |> expression_opt ~ctx
    | Texp_while _ ->
      not_implemented "Texp_while";
      assert false
    | Texp_for (_id, _pat, e1, e2, _dir, e3) ->
      let open Command in
      expression ~ctx e1 +++ expression ~ctx e2 +++ expression ~ctx e3
    | Texp_for_of (_id, _pat, e1, e2) ->
      let open Command in
      expression ~ctx e1 +++ expression ~ctx e2
    | Texp_for_await_of (_id, _pat, e1, e2) ->
      let open Command in
      expression ~ctx e1 +++ expression ~ctx e2
    | Texp_send _ ->
      not_implemented "Texp_send";
      assert false
    | Texp_letmodule _ ->
      not_implemented "Texp_letmodule";
      assert false
    | Texp_letexception _ ->
      not_implemented "Texp_letexception";
      assert false
    | Texp_pack _ ->
      not_implemented "Texp_pack";
      assert false
    | Texp_extension_constructor _ when true ->
      not_implemented "Texp_extension_constructor";
      assert false
    | _ ->
      (* ocaml 4.08: Texp_letop(_) | Texp_open(_) *)
      not_implemented "Texp_letop(_) | Texp_open(_)";
      assert false

  and expression_opt ~ctx e_opt =
    match e_opt with
    | None -> Command.nothing
    | Some e -> e |> expression ~ctx

  and eval_args ~args ~ctx command =
    (* Don't assume any evaluation order on the arguments *)
    let commands =
      args |> List.map (fun (_, e_opt) -> e_opt |> expression_opt ~ctx)
    in
    let open Command in
    unordered_sequence commands +++ command

  and case : ctx:ctx -> Typedtree.case -> _ =
   fun ~ctx {c_guard; c_rhs} ->
    match c_guard with
    | None -> c_rhs |> expression ~ctx
    | Some e ->
      let open Command in
      expression ~ctx e +++ expression ~ctx c_rhs
end

module CallStack = struct
  type frame = {frame_number: int; pos: Lexing.position}
  type t = {tbl: (FunctionCall.t, frame) Hashtbl.t; mutable size: int}

  let create () = {tbl = Hashtbl.create 1; size = 0}

  let to_set {tbl} =
    Hashtbl.fold
      (fun frame _i set -> FunctionCallSet.add frame set)
      tbl FunctionCallSet.empty

  let has_function_call ~function_call (t : t) = Hashtbl.mem t.tbl function_call

  let add_function_call ~function_call ~pos (t : t) =
    t.size <- t.size + 1;
    Hashtbl.replace t.tbl function_call {frame_number = t.size; pos}

  let remove_function_call ~function_call (t : t) =
    t.size <- t.size - 1;
    Hashtbl.remove t.tbl function_call

  let print ppf (t : t) =
    Format.fprintf ppf "  CallStack:";
    let frames =
      Hashtbl.fold
        (fun function_call {frame_number; pos} frames ->
          (function_call, frame_number, pos) :: frames)
        t.tbl []
      |> List.sort (fun (_, i1, _) (_, i2, _) -> i2 - i1)
    in
    frames
    |> List.iter (fun ((function_call : FunctionCall.t), i, pos) ->
           Format.fprintf ppf "\n    @{<dim>%d@} %s (%a)" i
             (FunctionCall.to_string function_call)
             print_pos pos)
end

module Eval = struct
  type progress = Progress.t
  type cache = (FunctionCall.t, State.t) Hashtbl.t

  let create_cache () : cache = Hashtbl.create 1

  let lookup_cache ~function_call (cache : cache) =
    Hashtbl.find_opt cache function_call

  let update_cache ~config ~function_call ~loc ~state (cache : cache) =
    Stats.log_result ~config ~function_call ~res_string:(state |> State.to_string)
      ~loc;
    if not (Hashtbl.mem cache function_call) then
      Hashtbl.replace cache function_call state

  let has_infinite_loop ~call_stack ~function_call_to_instantiate ~function_call ~loc
      ~state =
    if call_stack |> CallStack.has_function_call ~function_call then (
      if state.State.progress = NoProgress then (
        Log_.error ~loc
          (Termination
             {
               termination = ErrorTermination;
               message =
                 Format.asprintf "%a"
                   (fun ppf () ->
                     Format.fprintf ppf "Possible infinite loop when calling ";
                     (match function_call_to_instantiate = function_call with
                     | true ->
                       Format.fprintf ppf "@{<error>%s@}"
                         (function_call_to_instantiate |> FunctionCall.to_string)
                     | false ->
                       Format.fprintf ppf "@{<error>%s@} which is @{<error>%s@}"
                         (function_call_to_instantiate |> FunctionCall.to_string)
                         (function_call |> FunctionCall.to_string));
                     Format.fprintf ppf "@,%a" CallStack.print call_stack)
                   ();
             });
        Stats.log_loop ());
      true)
    else false

  let rec run_function_call ~config ~cache ~call_stack ~function_args ~function_table
      ~made_progress_on ~loc ~state function_call_to_instantiate : State.t =
    let pos = loc.Location.loc_start in
    let function_call =
      function_call_to_instantiate
      |> FunctionCall.apply_substitution ~sub:function_args
    in
    let function_name = function_call.function_name in
    let call = Call.FunctionCall function_call in
    let state_after_call =
      match cache |> lookup_cache ~function_call with
      | Some state_after_call ->
        Stats.log_cache ~config ~function_call ~hit:true ~loc;
        {
          state_after_call with
          trace = Trace.Tcall (call, state_after_call.progress);
        }
      | None ->
        if FunctionCallSet.mem function_call made_progress_on then
          State.init ~progress:Progress ~trace:(Trace.Tcall (call, Progress)) ()
        else if
          has_infinite_loop ~call_stack ~function_call_to_instantiate ~function_call
            ~loc ~state
        then {state with trace = Trace.Tcall (call, state.progress)}
        else (
          Stats.log_cache ~config ~function_call ~hit:false ~loc;
          let function_definition =
            function_table |> FunctionTable.get_function_definition ~function_name
          in
          call_stack |> CallStack.add_function_call ~function_call ~pos;
          let body =
            match function_definition.body with
            | Some body -> body
            | None -> assert false
          in
          let state_after_call =
            body
            |> run ~config ~cache ~call_stack
                 ~function_args:function_call.function_args ~function_table
                 ~made_progress_on ~state:(State.init ())
          in
          cache |> update_cache ~config ~function_call ~loc ~state:state_after_call;
          (* Invariant: run should restore the callStack *)
          call_stack |> CallStack.remove_function_call ~function_call;
          let trace = Trace.Tcall (call, state_after_call.progress) in
          {state_after_call with trace})
    in
    State.seq state state_after_call

  and run ~config ~(cache : cache) ~call_stack ~function_args ~function_table
      ~made_progress_on ~state (command : Command.t) : State.t =
    match command with
    | Call (FunctionCall function_call, loc) ->
      function_call
      |> run_function_call ~config ~cache ~call_stack ~function_args ~function_table
           ~made_progress_on ~loc ~state
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
      let rec find_first_progress ~call_stack ~commands ~made_progress_on ~state =
        match commands with
        | [] -> state
        | c :: next_commands ->
          let state1 =
            c
            |> run ~config ~cache ~call_stack ~function_args ~function_table
                 ~made_progress_on ~state
          in
          let made_progress_on, call_stack =
            match state1.progress with
            | Progress ->
              (* look for infinite loops in the rest of the sequence, remembering what has made progress *)
              ( FunctionCallSet.union made_progress_on
                  (call_stack |> CallStack.to_set),
                CallStack.create () )
            | NoProgress -> (made_progress_on, call_stack)
          in
          find_first_progress ~call_stack ~commands:next_commands ~made_progress_on
            ~state:state1
      in
      find_first_progress ~call_stack ~commands ~made_progress_on ~state
    | UnorderedSequence commands ->
      let state_no_trace = {state with trace = Trace.empty} in
      (* the commands could be executed in any order: progess if any one does *)
      let states =
        commands
        |> List.map (fun c ->
               c
               |> run ~config ~cache ~call_stack ~function_args ~function_table
                    ~made_progress_on ~state:state_no_trace)
      in
      State.seq state (states |> State.unordered_sequence)
    | Nondet commands ->
      let state_no_trace = {state with trace = Trace.empty} in
      (* the commands could be executed in any order: progess if any one does *)
      let states =
        commands
        |> List.map (fun c ->
               c
               |> run ~config ~cache ~call_stack ~function_args ~function_table
                    ~made_progress_on ~state:state_no_trace)
      in
      State.seq state (states |> State.nondet)
    | SwitchOption {function_call; loc; some; none} -> (
      let state_after_call =
        function_call
        |> run_function_call ~config ~cache ~call_stack ~function_args
             ~function_table ~made_progress_on ~loc ~state
      in
      match state_after_call.values_opt with
      | None ->
        Command.nondet [some; none]
        |> run ~config ~cache ~call_stack ~function_args ~function_table
             ~made_progress_on ~state:state_after_call
      | Some values ->
        let run_opt c progress_opt =
          match progress_opt with
          | None -> State.init ~progress:Progress ()
          | Some progress ->
            c
            |> run ~config ~cache ~call_stack ~function_args ~function_table
                 ~made_progress_on ~state:(State.init ~progress ())
        in
        let state_none = values |> Values.get_none |> run_opt none in
        let state_some = values |> Values.get_some |> run_opt some in
        State.seq state_after_call (State.nondet [state_some; state_none]))

  let analyze_function ~config ~cache ~function_table ~loc function_name =
    if config.DceConfig.cli.debug then
      Log_.log "@[<v 2>@,@{<warning>Termination Analysis@} for @{<info>%s@}@]@."
        function_name;
    let pos = loc.Location.loc_start in
    let call_stack = CallStack.create () in
    let function_args = FunctionArgs.empty in
    let function_call = FunctionCall.no_args function_name in
    call_stack |> CallStack.add_function_call ~function_call ~pos;
    let function_definition =
      function_table |> FunctionTable.get_function_definition ~function_name
    in
    if function_definition.kind <> Kind.empty then
      Stats.log_hygiene_parametric ~function_name ~loc
    else
      let body =
        match function_definition.body with
        | Some body -> body
        | None -> assert false
      in
      let state =
        body
        |> run ~config ~cache ~call_stack ~function_args ~function_table
             ~made_progress_on:FunctionCallSet.empty ~state:(State.init ())
      in
      cache |> update_cache ~config ~function_call ~loc ~state
end

let progress_functions_from_attributes attributes =
  let lid_to_string lid = lid |> Longident.flatten |> String.concat "." in
  let is_progress = ( = ) "progress" in
  if attributes |> Annotation.has_attribute is_progress then
    Some
      (match attributes |> Annotation.get_attribute_payload is_progress with
      | None -> []
      | Some (IdentPayload lid) -> [lid_to_string lid]
      | Some (TuplePayload l) ->
        l
        |> List.filter_map (function
             | Annotation.IdentPayload lid -> Some (lid_to_string lid)
             | _ -> None)
      | _ -> [])
  else None

let traverse_ast ~config ~value_bindings_table =
  let super = Tast_mapper.default in
  let value_bindings (self : Tast_mapper.mapper) (rec_flag, value_bindings) =
    (* Update the table of value bindings for variables *)
    value_bindings
    |> List.iter (fun (vb : Typedtree.value_binding) ->
           match vb.vb_pat.pat_desc with
           | Tpat_var (id, {loc = {loc_start = pos}}) ->
             let callees = lazy (FindFunctionsCalled.find_callees vb.vb_expr) in
             Hashtbl.replace value_bindings_table (Ident.name id)
               (pos, vb.vb_expr, callees)
           | _ -> ());
    let progress_functions, functions_to_analyze =
      if rec_flag = Asttypes.Nonrecursive then (StringSet.empty, [])
      else
        let progress_functions0, functions_to_analyze0 =
          value_bindings
          |> List.fold_left
               (fun (progress_functions, functions_to_analyze)
                    (value_binding : Typedtree.value_binding) ->
                 match
                   progress_functions_from_attributes value_binding.vb_attributes
                 with
                 | None -> (progress_functions, functions_to_analyze)
                 | Some new_progress_functions ->
                   ( StringSet.union
                       (StringSet.of_list new_progress_functions)
                       progress_functions,
                     match value_binding.vb_pat.pat_desc with
                     | Tpat_var (id, _) ->
                       (Ident.name id, value_binding.vb_expr.exp_loc)
                       :: functions_to_analyze
                     | _ -> functions_to_analyze ))
               (StringSet.empty, [])
        in
        (progress_functions0, functions_to_analyze0 |> List.rev)
    in
    if functions_to_analyze <> [] then (
      let function_table = FunctionTable.create () in
      let is_progress_function path =
        StringSet.mem (Path.name path) progress_functions
      in
      let recursive_functions =
        List.fold_left
          (fun defs (value_binding : Typedtree.value_binding) ->
            match value_binding.vb_pat.pat_desc with
            | Tpat_var (id, _) -> Ident.name id :: defs
            | _ -> defs)
          [] value_bindings
        |> List.rev
      in
      let recursive_definitions =
        recursive_functions
        |> List.fold_left
             (fun acc function_name ->
               match Hashtbl.find_opt value_bindings_table function_name with
               | Some (_pos, e, _set) -> (function_name, e) :: acc
               | None -> acc)
             []
        |> List.rev
      in
      recursive_definitions
      |> List.iter (fun (function_name, _body) ->
             function_table |> FunctionTable.add_function ~function_name);
      recursive_definitions
      |> List.iter (fun (_, body) ->
             body
             |> ExtendFunctionTable.run ~config ~function_table
                  ~progress_functions ~value_bindings_table);
      recursive_definitions
      |> List.iter (fun (_, body) ->
             body
             |> CheckExpressionWellFormed.run ~config ~function_table
                  ~value_bindings_table);
      function_table
      |> Hashtbl.iter
           (fun
             function_name
             (function_definition : FunctionTable.function_definition)
           ->
             if function_definition.body = None then
               match Hashtbl.find_opt value_bindings_table function_name with
               | None -> ()
               | Some (_pos, body, _) ->
                 function_table
                 |> FunctionTable.add_body
                      ~body:
                        (Some
                           (body
                           |> Compile.expression
                                ~ctx:
                                  {
                                    config;
                                    current_function_name = function_name;
                                    function_table;
                                    inner_recursive_functions = Hashtbl.create 1;
                                    is_progress_function;
                                  }))
                      ~function_name);
      if config.DceConfig.cli.debug then FunctionTable.dump function_table;
      let cache = Eval.create_cache () in
      functions_to_analyze
      |> List.iter (fun (function_name, loc) ->
             function_name
             |> Eval.analyze_function ~config ~cache ~function_table ~loc);
      Stats.new_recursive_functions ~num_functions:(Hashtbl.length function_table));
    value_bindings
    |> List.iter (fun value_binding ->
           super.value_binding self value_binding |> ignore);
    (rec_flag, value_bindings)
  in
  {super with Tast_mapper.value_bindings}

let process_structure ~config (structure : Typedtree.structure) =
  Stats.new_file ();
  let value_bindings_table = Hashtbl.create 1 in
  let traverse_ast = traverse_ast ~config ~value_bindings_table in
  structure |> traverse_ast.structure traverse_ast |> ignore

let process_cmt ~config ~file:_ (cmt_infos : Cmt_format.cmt_infos) =
  match cmt_infos.cmt_annots with
  | Interface _ -> ()
  | Implementation structure -> process_structure ~config structure
  | _ -> ()

let report_stats ~config:_ = Stats.dump ~ppf:Format.std_formatter
