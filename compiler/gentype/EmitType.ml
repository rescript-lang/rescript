open GenTypeCommon

let file_header ~source_file =
  let make_header ~lines =
    match lines with
    | [line] -> "/* " ^ line ^ " */\n\n"
    | _ ->
      "/** \n"
      ^ (lines |> List.map (fun line -> " * " ^ line) |> String.concat "\n")
      ^ "\n */\n\n"
  in
  make_header
    ~lines:["TypeScript file generated from " ^ source_file ^ " by genType."]
  ^ "/* eslint-disable */\n" ^ "/* tslint:disable */\n"

let interface_name ~(config : Config.t) name =
  match config.export_interfaces with
  | true -> "I" ^ name
  | false -> name

let type_any = ident ~builtin:true "any"

let type_react_component ~props_type =
  "React.ComponentType" |> ident ~builtin:true ~type_args:[props_type]

let type_react_context ~type_ =
  "React.Context" |> ident ~builtin:true ~type_args:[type_]

let type_react_element_type_script = ident ~builtin:true "JSX.Element"
let type_react_child_type_script = ident ~builtin:true "React.ReactNode"
let type_react_element = type_react_element_type_script
let type_react_child = type_react_child_type_script
let is_type_react_element type_ = type_ == type_react_element

let type_react_d_o_m_re_dom_ref =
  "React.Ref" |> ident ~builtin:true ~type_args:[unknown]

let type_react_event_mouse_t = "MouseEvent" |> ident ~builtin:true
let react_ref_current = "current"

let type_react_ref ~type_ =
  Object
    ( Open,
      [
        {
          mutable_ = Mutable;
          name_js = react_ref_current;
          optional = Mandatory;
          type_ = Null type_;
          doc_string = DocString.empty;
        };
      ] )

let is_type_react_ref ~fields =
  match fields with
  | [{mutable_ = Mutable; name_js; optional = Mandatory}] ->
    name_js == react_ref_current
  | _ -> false

let is_type_function_component ~fields type_ =
  type_ |> is_type_react_element && not (is_type_react_ref ~fields)

let rec render_type ~(config : Config.t) ?(indent = None)
    ~type_name_is_interface ~in_fun_type type0 =
  match type0 with
  | Array (t, array_kind) ->
    let type_is_simple =
      match t with
      | Ident _ | TypeVar _ -> true
      | _ -> false
    in
    if type_is_simple && array_kind = Mutable then
      (t |> render_type ~config ~indent ~type_name_is_interface ~in_fun_type)
      ^ "[]"
    else
      let array_name =
        match array_kind = Mutable with
        | true -> "Array"
        | false -> "ReadonlyArray"
      in
      array_name ^ "<"
      ^ (t |> render_type ~config ~indent ~type_name_is_interface ~in_fun_type)
      ^ ">"
  | Dict type_ ->
    "{[id: string]: "
    ^ (type_ |> render_type ~config ~indent ~type_name_is_interface ~in_fun_type)
    ^ "}"
  | Function
      {
        arg_types = [{a_type = Object (closed_flag, fields)}];
        ret_type;
        type_vars;
      }
    when ret_type |> is_type_function_component ~fields ->
    let fields =
      fields
      |> List.map (fun field ->
             {
               field with
               type_ =
                 field.type_
                 |> TypeVars.substitute ~f:(fun s ->
                        if type_vars |> List.mem s then Some type_any else None);
             })
    in
    let component_type =
      type_react_component ~props_type:(Object (closed_flag, fields))
    in
    component_type
    |> render_type ~config ~indent ~type_name_is_interface ~in_fun_type
  | Function {arg_types; ret_type; type_vars} ->
    render_fun_type ~config ~indent ~in_fun_type ~type_name_is_interface
      ~type_vars arg_types ret_type
  | Object (_, fields) ->
    let indent1 = fields |> Indent.heuristic_fields ~indent in
    fields
    |> render_fields ~config ~indent:indent1 ~in_fun_type
         ~type_name_is_interface
  | Ident {builtin; name; type_args} ->
    let name = name |> sanitize_type_name in
    (match
       (not builtin) && config.export_interfaces
       && name |> type_name_is_interface
     with
    | true -> name |> interface_name ~config
    | false -> name)
    ^ EmitText.generics_string
        ~type_vars:
          (type_args
          |> List.map
               (render_type ~config ~indent ~type_name_is_interface ~in_fun_type)
          )
  | Null type_ ->
    "(null | "
    ^ (type_ |> render_type ~config ~indent ~type_name_is_interface ~in_fun_type)
    ^ ")"
  | Nullable type_ ->
    let use_parens x =
      match type_ with
      | Function _ | Variant _ -> EmitText.parens [x]
      | _ -> x
    in
    "(null | undefined | "
    ^ use_parens
        (type_
        |> render_type ~config ~indent ~type_name_is_interface ~in_fun_type)
    ^ ")"
  | Option type_ ->
    let use_parens x =
      match type_ with
      | Function _ | Variant _ -> EmitText.parens [x]
      | _ -> x
    in
    "(undefined | "
    ^ use_parens
        (type_
        |> render_type ~config ~indent ~type_name_is_interface ~in_fun_type)
    ^ ")"
  | Promise type_ ->
    "Promise" ^ "<"
    ^ (type_ |> render_type ~config ~indent ~type_name_is_interface ~in_fun_type)
    ^ ">"
  | Tuple inner_types ->
    "["
    ^ (inner_types
      |> List.map
           (render_type ~config ~indent ~type_name_is_interface ~in_fun_type)
      |> String.concat ", ")
    ^ "]"
  | TypeVar s -> s
  | Variant {inherits; no_payloads; payloads; polymorphic; tag; unboxed} ->
    let inherits_rendered =
      inherits
      |> List.map (fun type_ ->
             type_
             |> render_type ~config ~indent ~type_name_is_interface ~in_fun_type)
    in
    let no_payloads_rendered = no_payloads |> List.map label_js_to_string in
    let field ~name value =
      {
        mutable_ = Mutable;
        name_js = name;
        optional = Mandatory;
        type_ = TypeVar value;
        doc_string = DocString.empty;
      }
    in
    let fields fields =
      fields
      |> render_fields ~config ~indent ~in_fun_type ~type_name_is_interface
    in
    let payloads_rendered =
      payloads
      |> List.map (fun {case; t = type_} ->
             let render t =
               t
               |> render_type ~config ~indent ~type_name_is_interface
                    ~in_fun_type
             in
             let tag_field =
               case |> label_js_to_string
               |> field ~name:(Runtime.js_variant_tag ~polymorphic:false ~tag)
             in
             match (unboxed, type_) with
             | true, type_ ->
               let need_parens =
                 match type_ with
                 | Function _ -> true
                 | _ -> false
               in
               let t = type_ |> render in
               if need_parens then EmitText.parens [t] else t
             | false, type_ when polymorphic ->
               (* poly variant *)
               [
                 case |> label_js_to_string
                 |> field ~name:(Runtime.js_variant_tag ~polymorphic ~tag);
                 type_ |> render
                 |> field ~name:(Runtime.js_variant_value ~polymorphic);
               ]
               |> fields
             | false, Object (Inline, flds) ->
               (* inlined record *)
               tag_field :: flds |> fields
             | false, type_ ->
               (* ordinary variant *)
               let payloads =
                 match type_ with
                 | Tuple ts -> ts
                 | _ -> [type_]
               in
               let flds =
                 tag_field
                 :: Ext_list.mapi payloads (fun n t ->
                        t |> render
                        |> field ~name:(Runtime.js_variant_payload_tag ~n))
               in
               flds |> fields)
    in
    let rendered =
      inherits_rendered @ no_payloads_rendered @ payloads_rendered
    in
    let indent1 = rendered |> Indent.heuristic_variants ~indent in
    (match indent1 = None with
    | true -> ""
    | false -> Indent.break ~indent:indent1 ^ "  ")
    ^ (rendered
      |> String.concat
           ((match indent1 = None with
            | true -> " "
            | false -> Indent.break ~indent:indent1)
           ^ "| "))

and render_field ~config ~indent ~type_name_is_interface ~in_fun_type
    {mutable_; name_js = lbl; optional; type_; doc_string} =
  let opt_marker =
    match optional == Optional with
    | true -> "?"
    | false -> ""
  in
  let mut_marker =
    match mutable_ = Immutable with
    | true -> "readonly "
    | false -> ""
  in
  let lbl =
    match is_js_safe_property_name lbl with
    | true -> lbl
    | false -> EmitText.quotes lbl
  in

  let def_str =
    mut_marker ^ lbl ^ opt_marker ^ ": "
    ^ (type_ |> render_type ~config ~indent ~type_name_is_interface ~in_fun_type)
  in
  if DocString.has_content doc_string then
    (* Always print comments on newline before definition. *)
    let indent_str = indent |> Option.value ~default:"" in
    "\n" ^ indent_str ^ DocString.render doc_string ^ indent_str ^ def_str
  else Indent.break ~indent ^ def_str

and render_fields ~config ~indent ~in_fun_type ~type_name_is_interface fields =
  let indent1 = indent |> Indent.more in
  let space =
    match indent = None && fields <> [] with
    | true -> " "
    | false -> ""
  in
  let rendered_fields =
    fields
    |> List.map
         (render_field ~config ~indent:indent1 ~type_name_is_interface
            ~in_fun_type)
  in
  ("{" ^ space)
  ^ String.concat "; " rendered_fields
  ^ Indent.break ~indent ^ space ^ "}"

and render_fun_type ~config ~indent ~in_fun_type ~type_name_is_interface
    ~type_vars arg_types ret_type =
  (match in_fun_type with
  | true -> "("
  | false -> "")
  ^ EmitText.generics_string ~type_vars
  ^ "("
  ^ String.concat ", "
      (List.mapi
         (fun i {a_name; a_type} ->
           let parameter_name =
             (match a_name = "" with
             | true -> "_" ^ string_of_int (i + 1)
             | false -> a_name)
             ^ ":"
           in
           parameter_name
           ^ (a_type
             |> render_type ~config ~indent ~type_name_is_interface
                  ~in_fun_type:true))
         arg_types)
  ^ ") => "
  ^ (ret_type
    |> render_type ~config ~indent ~type_name_is_interface ~in_fun_type)
  ^
  match in_fun_type with
  | true -> ")"
  | false -> ""

let type_to_string ~config ~type_name_is_interface type_ =
  type_ |> render_type ~config ~type_name_is_interface ~in_fun_type:false

let emit_export_const ~early ?(comment = "") ~config
    ?(doc_string = DocString.empty) ~emitters ~name ~type_
    ~type_name_is_interface line =
  let type_string = type_ |> type_to_string ~config ~type_name_is_interface in
  (match comment = "" with
  | true -> comment
  | false -> "// " ^ comment ^ "\n")
  ^ DocString.render doc_string
  ^ "export const " ^ name ^ ": " ^ type_string ^ " = " ^ line ^ " as any;"
  |> (match early with
     | true -> Emitters.export_early
     | false -> Emitters.export)
       ~emitters

let emit_export_default ~emitters name =
  "export default " ^ name ^ ";" |> Emitters.export ~emitters

let emit_export_type ~(config : Config.t) ~emitters ~name_as ~opaque ~type_
    ~type_name_is_interface ~type_vars ~doc_string resolved_type_name =
  let doc_string = DocString.render doc_string in
  let type_params_string = EmitText.generics_string ~type_vars in
  let is_interface = resolved_type_name |> type_name_is_interface in
  let resolved_type_name =
    match config.export_interfaces && is_interface with
    | true -> resolved_type_name |> interface_name ~config
    | false -> resolved_type_name
  in
  let export_name_as =
    match name_as with
    | None -> ""
    | Some s ->
      "\nexport type " ^ s ^ type_params_string ^ " = " ^ resolved_type_name
      ^ type_params_string ^ ";"
  in
  if opaque then
    (* Represent an opaque type as an absract class with a field called 'opaque'.
       Any type parameters must occur in the type of opaque, so that different
       instantiations are considered different types. *)
    let type_of_opaque_field =
      match type_vars = [] with
      | true -> "any"
      | false -> type_vars |> String.concat " | "
    in
    doc_string ^ "export abstract class " ^ resolved_type_name
    ^ type_params_string ^ " { protected opaque!: " ^ type_of_opaque_field
    ^ " }; /* simulate opaque types */" ^ export_name_as
    |> Emitters.export ~emitters
  else
    (if is_interface && config.export_interfaces then
       doc_string ^ "export interface " ^ resolved_type_name
       ^ type_params_string ^ " "
     else
       doc_string ^ "export type " ^ resolved_type_name ^ type_params_string
       ^ " = ")
    ^ (match type_ with
      | _ -> type_ |> type_to_string ~config ~type_name_is_interface)
    ^ ";" ^ export_name_as
    |> Emitters.export ~emitters

let emit_import_value_as_early ~emitters ~name ~name_as import_path =
  "import "
  ^ (match name_as with
    | Some name_as -> "{" ^ name ^ " as " ^ name_as ^ "}"
    | None -> name)
  ^ " from " ^ "'"
  ^ (import_path |> ImportPath.emit)
  ^ "';"
  |> Emitters.require_early ~emitters

let emit_require ~imported_value_or_component ~early ~emitters
    ~(config : Config.t) ~module_name import_path =
  let module_name_string = ModuleName.to_string module_name in
  let import_path_string = ImportPath.emit import_path in
  let output =
    match config.module_ with
    | ESModule when not imported_value_or_component ->
      "import * as " ^ module_name_string ^ " from '" ^ import_path_string
      ^ "';"
    | _ ->
      "const " ^ module_name_string ^ " = require('" ^ import_path_string
      ^ "');"
  in
  output
  |> (match early with
     | true -> Emitters.require_early
     | false -> Emitters.require)
       ~emitters

let require ~early =
  match early with
  | true -> Emitters.require_early
  | false -> Emitters.require

let emit_import_react ~emitters =
  "import * as React from 'react';" |> require ~early:true ~emitters

let emit_import_type_as ~emitters ~config ~type_name ~as_type_name
    ~type_name_is_interface ~import_path =
  let type_name = sanitize_type_name type_name in
  let as_type_name =
    match as_type_name with
    | None -> as_type_name
    | Some s -> Some (sanitize_type_name s)
  in
  let type_name, as_type_name =
    match as_type_name with
    | Some as_name -> (
      match as_name |> type_name_is_interface with
      | true ->
        ( type_name |> interface_name ~config,
          Some (as_name |> interface_name ~config) )
      | false -> (type_name, as_type_name))
    | None -> (type_name, as_type_name)
  in
  let import_path_string = import_path |> ImportPath.emit in
  let import_prefix = "import type" in
  import_prefix ^ " " ^ "{" ^ type_name
  ^ (match as_type_name with
    | Some as_t -> " as " ^ as_t
    | None -> "")
  ^ "} from '" ^ import_path_string ^ "';"
  |> Emitters.import ~emitters

let emit_type_cast ~config ~type_ ~type_name_is_interface s =
  s ^ " as " ^ (type_ |> type_to_string ~config ~type_name_is_interface)
