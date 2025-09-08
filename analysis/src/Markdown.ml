let spacing = "\n```\n \n```\n"
let codeBlock code = Printf.sprintf "```rescript\n%s\n```" code
let divider = "\n---\n"

type link = {startPos: Protocol.position; file: string; label: string}

let linkToCommandArgs link =
  Printf.sprintf "[\"%s\",%i,%i]" link.file link.startPos.line
    link.startPos.character

let makeGotoCommand link =
  Printf.sprintf "[%s](command:rescript-vscode.go_to_location?%s)" link.label
    (Uri.encodeURIComponent (linkToCommandArgs link))

let goToDefinitionText ~env ~pos =
  let startLine, startCol = Pos.ofLexing pos in
  "\nGo to: "
  ^ makeGotoCommand
      {
        label = "Type definition";
        file = Uri.toString env.SharedTypes.QueryEnv.file.uri;
        startPos = {line = startLine; character = startCol};
      }
