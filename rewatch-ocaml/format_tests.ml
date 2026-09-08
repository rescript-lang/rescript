let check condition message = if not condition then failwith message

let () =
  check
    (Format.formatting_error "stdin" "invalid source"
    = "Error formatting stdin: invalid source")
    "stdin formatting failures do not expose a temporary filename";
  check
    (Format.formatting_error "src/A.res" "invalid source"
    = "Error formatting src/A.res: invalid source")
    "file formatting failures retain the source path";
  check
    (Format.format_check_summary 1
    = "The file listed above needs formatting")
    "format check uses Rust's singular summary";
  check
    (Format.format_check_summary 2
    = "The 2 files listed above need formatting")
    "format check uses Rust's plural summary"
