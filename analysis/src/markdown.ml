let spacing = "\n```\n \n```\n"
let code_block code = Printf.sprintf "```rescript\n%s\n```" code
let divider = "\n---\n"

type link = {start_pos: Lsp.Types.Position.t; file: string; label: string}

let make_goto_command link =
  Printf.sprintf "[%s](%s#L%d,%d)" link.label link.file (link.start_pos.line + 1)
    (link.start_pos.character + 1)

let go_to_definition_text ~env ~pos =
  let start_line, start_col = Pos.of_lexing pos in
  "\nGo to: "
  ^ make_goto_command
      {
        label = "Type definition";
        file = Uri.to_string env.Shared_types.Query_env.file.uri;
        start_pos = {line = start_line; character = start_col};
      }
