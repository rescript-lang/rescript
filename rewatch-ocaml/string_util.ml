(* OCaml 5.5 provides [String.includes], but Rewatch also supports OCaml 5.0.
   The affixes checked here are short markers, so this allocation-free scan
   keeps the compatibility implementation simpler than a general-purpose
   substring-search algorithm. *)
let contains value substring =
  let substring_length = String.length substring in
  let last_start = String.length value - substring_length in
  let rec matches_at start offset =
    offset = substring_length
    || String.get value (start + offset) = String.get substring offset
       && matches_at start (offset + 1)
  in
  let rec search start =
    start <= last_start && (matches_at start 0 || search (start + 1))
  in
  search 0
