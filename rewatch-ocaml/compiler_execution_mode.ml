let max_domain_count = max 12 Process.default_max_jobs

(* Leave a CPU for dependency scheduling and artifact publication. The cap is
   provisional: gains flattened around eight workers on the Linux fixture. *)
let recommended_domain_count available_cpus = min 8 (max 1 (available_cpus - 1))

(* A count above the machine's limit is lowered to it, so one setting works on
   machines with different CPU counts. *)
let configured_count () =
  match Sys.getenv_opt "REWATCH_COMPILER_DOMAINS" with
  | None -> recommended_domain_count (Domain.recommended_domain_count ())
  | Some value -> (
    match int_of_string_opt (String.trim value) with
    | Some count when count >= 1 -> min count max_domain_count
    | _ ->
      raise
        (Process.Error
           ("REWATCH_COMPILER_DOMAINS must be a positive integer, got " ^ value))
    )
