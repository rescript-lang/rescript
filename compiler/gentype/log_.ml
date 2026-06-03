module Color = struct
  type color = Red | Yellow | Magenta | Cyan
  type style = FG of color | Bold | Dim

  let code_of_style = function
    | FG Red -> "31"
    | FG Yellow -> "33"
    | FG Magenta -> "35"
    | FG Cyan -> "36"
    | Bold -> "1"
    | Dim -> "2"

  let style_of_stag s =
    match s with
    | Format.String_tag "error" -> [Bold; FG Red]
    | Format.String_tag "warning" -> [Bold; FG Magenta]
    | Format.String_tag "info" -> [Bold; FG Yellow]
    | Format.String_tag "dim" -> [Dim]
    | Format.String_tag "filename" -> [FG Cyan]
    | _ -> []

  let ansi_of_stag s =
    let l = style_of_stag s in
    let s = String.concat ";" (List.map code_of_style l) in
    "\027[" ^ s ^ "m"

  let reset_lit = "\027[0m"

  let color_functions =
    (({
        mark_open_stag = (fun s -> ansi_of_stag s);
        mark_close_stag = (fun _ -> reset_lit);
        print_open_stag = (fun _ -> ());
        print_close_stag = (fun _ -> ());
      }
       : Format.formatter_stag_functions)
      : Format.formatter_stag_functions)

  let setup () =
    Format.pp_set_mark_tags Format.std_formatter true;
    Format.pp_set_formatter_stag_functions Format.std_formatter color_functions

  let error ppf s = Format.fprintf ppf "@{<error>%s@}" s [@@dead "Color.error"]
  let info ppf s = Format.fprintf ppf "@{<info>%s@}" s
end

module Loc = struct
  let print_filename ppf file =
    match file with
    (* modified *)
    | "_none_" | "" -> Format.fprintf ppf "(No file name)"
    | real_file -> Format.fprintf ppf "%s" (Location.show_filename real_file)

  let print_loc ~normalized_range ppf (loc : Location.t) =
    let file, _, _ = Location.get_pos_info loc.loc_start in
    if file = "//toplevel//" then
      Format.fprintf ppf "Characters %i-%i" loc.loc_start.pos_cnum
        loc.loc_end.pos_cnum
    else
      let dim_loc ppf = function
        | None -> ()
        | Some
            ((start_line, start_line_start_char), (end_line, end_line_end_char))
          ->
          if start_line = end_line then
            if start_line_start_char = end_line_end_char then
              Format.fprintf ppf ":@{<dim>%i:%i@}" start_line
                start_line_start_char
            else
              Format.fprintf ppf ":@{<dim>%i:%i-%i@}" start_line
                start_line_start_char end_line_end_char
          else
            Format.fprintf ppf ":@{<dim>%i:%i-%i:%i@}" start_line
              start_line_start_char end_line end_line_end_char
      in
      Format.fprintf ppf "@{<filename>%a@}%a" print_filename file dim_loc
        normalized_range

  let print ppf (loc : Location.t) =
    let _file, start_line, start_char = Location.get_pos_info loc.loc_start in
    let _, end_line, end_char = Location.get_pos_info loc.loc_end in
    let normalized_range =
      if start_char == -1 || end_char == -1 then None
      else if start_line = end_line && start_char >= end_char then
        let same_char = start_char + 1 in
        Some ((start_line, same_char), (end_line, same_char))
      else Some ((start_line, start_char + 1), (end_line, end_char))
    in

    Format.fprintf ppf "@[%a@]" (print_loc ~normalized_range) loc
end

let item x =
  Format.fprintf Format.std_formatter "  ";
  Format.fprintf Format.std_formatter x

let log_kind body ~color ~loc ~name =
  Format.fprintf Format.std_formatter "@[<v 2>@,%a@,%a@,%a@]@." color name
    Loc.print loc body ()

let info body ~loc ~name = log_kind body ~color:Color.info ~loc ~name
