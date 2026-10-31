let newline source =
  match String.index_opt source '\n' with
  | Some index when index > 0 && source.[index - 1] = '\r' -> "\r\n"
  | _ -> "\n"
