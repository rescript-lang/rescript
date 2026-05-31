let spacing = "\n```\n \n```\n"
let code_block code = Printf.sprintf "```rescript\n%s\n```" code
let divider = "\n---\n"

type link = {start_pos: Lsp.Types.Position.t; file: string; label: string}

let link_to_command_args link =
  Printf.sprintf "[\"%s\",%i,%i]" link.file link.start_pos.line
    link.start_pos.character

let make_goto_command link =
  Printf.sprintf "[%s](command:rescript-vscode.go_to_location?%s)" link.label
    (Uri.encode_u_r_i_component (link_to_command_args link))

let go_to_definition_text ~env ~pos =
  let start_line, start_col = Pos.of_lexing pos in
  "\nGo to: "
  ^ make_goto_command
      {
        label = "Type definition";
        file = Uri.to_string env.SharedTypes.QueryEnv.file.uri;
        start_pos = {line = start_line; character = start_col};
      }
