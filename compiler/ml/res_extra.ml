let current_outputprefix : string option ref = ref None

let possible_actions : Actions.action list ref = ref []

let set_current_outputprefix v = current_outputprefix := v

let is_interface : bool ref = ref false

let set_is_interface v = is_interface := v

let add_possible_action action = possible_actions := action :: !possible_actions

let () = Actions._add_possible_action := add_possible_action

let save () =
  match !current_outputprefix with
  | None -> ()
  | Some outputprefix ->
    let extras_filename =
      outputprefix ^ if !is_interface then ".resiextra" else ".resextra"
    in
    if List.length !possible_actions > 0 then
      Misc.output_to_bin_file_directly extras_filename (fun _ oc ->
          output_value oc (!possible_actions : Actions.action list));
    possible_actions := []
