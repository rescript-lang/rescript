let contains_text value text =
  try
    ignore (Str.search_forward (Str.regexp_string text) value 0);
    true
  with Not_found -> false

external unsetenv : string -> unit = "rewatch_test_unsetenv"
