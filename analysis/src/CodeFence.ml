(* Define a type for a range with start and finish indices *)
type range = {start: int; finish: int}

(* --- Helper function to find the 0-based line index containing a given 0-based character index --- *)
let get_line_index_from_char_index code char_index =
  let lines = String.split_on_char '\n' code in
  let rec find_line_idx current_char_idx current_line_num remaining_lines =
    match remaining_lines with
    | [] ->
      max 0 (current_line_num - 1)
      (* If char_index is beyond the end, return last line index *)
    | line :: tl ->
      let line_length = String.length line in
      (* Check if char_index is within the current line (including the newline char) *)
      if
        char_index >= current_char_idx
        && char_index <= current_char_idx + line_length
      then current_line_num
      else
        (* Move to the next line, account for the newline character (+1) *)
        find_line_idx
          (current_char_idx + line_length + 1)
          (current_line_num + 1) tl
  in
  find_line_idx 0 0 lines

(* --- Helper function to calculate the 0-based character index of the start of a given 0-based line index --- *)
let get_char_index_from_line_index code target_line_index =
  let lines = String.split_on_char '\n' code in
  let rec calculate_start_index_impl current_char_idx current_line_num
      lines_to_process =
    if current_line_num >= target_line_index then current_char_idx
    else
      match lines_to_process with
      | [] -> current_char_idx (* Target line index is out of bounds *)
      | line :: tl ->
        (* Move past the current line and its newline character *)
        calculate_start_index_impl
          (current_char_idx + String.length line + 1)
          (current_line_num + 1) tl
  in
  calculate_start_index_impl 0 0 lines

(* --- Main formatting function --- *)
let format_code_snippet_cropped code (underline_range : range option)
    lines_around_annotation =
  let lines = String.split_on_char '\n' code in
  let total_lines = List.length lines in
  let formatted_output = Buffer.create (String.length code) in
  (* Initial capacity *)

  (* Determine the central line index for cropping *)
  let target_line_index =
    match underline_range with
    | Some {start; finish = _} -> get_line_index_from_char_index code start
    | None -> 0 (* Default to first line if no annotations *)
  in

  (* Determine the cropping window (0-based line indices) *)
  let start_line_index = max 0 (target_line_index - lines_around_annotation) in
  let end_line_index =
    min (total_lines - 1) (target_line_index + lines_around_annotation)
  in

  (* Keep track of the global character index corresponding to the start of the *current* line being iterated over *)
  let current_char_index = ref 0 in

  (* Iterate through all original lines to correctly track current_char_index *)
  List.iteri
    (fun original_line_idx line ->
      let line_length = String.length line in
      (* Check if the current original line is within our cropping window *)
      if
        original_line_idx >= start_line_index
        && original_line_idx <= end_line_index
      then (
        let original_line_number = original_line_idx + 1 in
        (* 1-based for display *)
        let line_number_prefix = Printf.sprintf "%d + " original_line_number in
        let prefix_length = String.length line_number_prefix in

        (* Add the code line *)
        Buffer.add_string formatted_output line_number_prefix;
        Buffer.add_string formatted_output line;
        Buffer.add_char formatted_output '\n';

        (* Prepare the annotation line buffer *)
        let annotation_line_buffer =
          Buffer.create (prefix_length + line_length)
        in
        Buffer.add_string annotation_line_buffer (String.make prefix_length ' ');

        (* Initial padding *)
        let has_annotation_on_this_line = ref false in

        (* Check each character position within this line for annotations *)
        for i = 0 to line_length - 1 do
          let global_char_index = !current_char_index + i in
          let annotation_char = ref ' ' in
          (* Default to space *)

          (* Check for underline using Option.iter *)
          Option.iter
            (fun {start; finish} ->
              if global_char_index >= start && global_char_index < finish then (
                annotation_char := '-' (* '¯' *);
                (* Macron symbol *)
                has_annotation_on_this_line := true))
            underline_range;

          Buffer.add_char annotation_line_buffer !annotation_char
        done;

        (* Add the annotation line to the main output if needed *)
        if !has_annotation_on_this_line then (
          Buffer.add_buffer formatted_output annotation_line_buffer;
          Buffer.add_char formatted_output '\n'));

      (* Update the global character index to the start of the next line *)
      (* This happens regardless of whether the line was in the cropped window *)
      current_char_index := !current_char_index + line_length + 1
      (* +1 for the newline *))
    lines;

  Buffer.contents formatted_output
