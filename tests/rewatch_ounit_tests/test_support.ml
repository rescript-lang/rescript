let contains_text value text =
  try
    ignore (Str.search_forward (Str.regexp_string text) value 0);
    true
  with Not_found -> false
