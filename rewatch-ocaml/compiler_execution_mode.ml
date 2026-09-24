let max_domain_count = max 12 Process.default_max_jobs

(* Leave a CPU for dependency scheduling and artifact publication. The cap is
   provisional: gains flattened around eight workers on the Linux fixture. *)
let recommended_domain_count available_cpus = min 8 (max 1 (available_cpus - 1))

let configured_count () =
  let count =
    match Sys.getenv_opt "REWATCH_COMPILER_DOMAINS" with
    | None -> recommended_domain_count (Domain.recommended_domain_count ())
    | Some value -> (
      match int_of_string_opt value with
      | Some count -> count
      | None ->
        invalid_arg ("REWATCH_COMPILER_DOMAINS must be an integer, got " ^ value)
      )
  in
  if count < 1 || count > max_domain_count then
    invalid_arg
      (Printf.sprintf
         "REWATCH_COMPILER_DOMAINS must be between 1 and %d, got %d"
         max_domain_count count);
  count
