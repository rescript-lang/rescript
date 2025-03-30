open Ast_mapper
open Asttypes
open Parsetree
open Longident

let get_payload_fields payload =
  match payload with
  | PStr
      ({
         pstr_desc =
           Pstr_eval ({pexp_desc = Pexp_record (record_fields, None)}, _);
       }
      :: _rest) ->
    record_fields
  | _ -> []

type config_key = Int | String

let get_jsx_config_by_key ~key ~type_ record_fields =
  let values =
    List.filter_map
      (fun ((lid, expr, _) : Longident.t Location.loc * expression * bool) ->
        match (type_, lid, expr) with
        | ( Int,
            {txt = Lident k},
            {pexp_desc = Pexp_constant (Pconst_integer (value, None))} )
          when k = key ->
          Some value
        | ( String,
            {txt = Lident k},
            (* accept both normal strings and "js" strings *)
            {pexp_desc = Pexp_constant (Pconst_string (value, _))} )
          when k = key ->
          Some value
        | _ -> None)
      record_fields
  in
  match values with
  | [] -> None
  | [v] | v :: _ -> Some v

let get_int ~key fields =
  match fields |> get_jsx_config_by_key ~key ~type_:Int with
  | None -> None
  | Some s -> int_of_string_opt s

let get_string ~key fields = fields |> get_jsx_config_by_key ~key ~type_:String

let update_config config payload =
  let fields = get_payload_fields payload in
  let module_raw = get_string ~key:"module_" fields in
  let is_generic =
    match module_raw |> Option.map (fun m -> String.lowercase_ascii m) with
    | Some "react" | None -> false
    | Some _ -> true
  in
  (match (is_generic, get_int ~key:"version" fields) with
  | true, _ -> config.Jsx_common.version <- 4
  | false, Some i -> config.Jsx_common.version <- i
  | _ -> ());
  match module_raw with
  | None -> ()
  | Some s -> config.module_ <- s

let is_jsx_config_attr ((loc, _) : attribute) = loc.txt = "jsxConfig"

let process_config_attribute attribute config =
  if is_jsx_config_attr attribute then update_config config (snd attribute)

let get_mapper ~config =
  let ( expr4,
        module_binding4,
        transform_signature_item4,
        transform_structure_item4 ) =
    Jsx_v4.jsx_mapper ~config
  in

  let expr mapper e =
    match config.version with
    | 4 -> expr4 mapper e
    | _ -> default_mapper.expr mapper e
  in
  let module_binding mapper mb =
    match config.version with
    | 4 -> module_binding4 mapper mb
    | _ -> default_mapper.module_binding mapper mb
  in
  let save_config () =
    {
      config with
      version = config.version;
      module_ = config.module_;
      has_component = config.has_component;
    }
  in
  let restore_config old_config =
    config.version <- old_config.Jsx_common.version;
    config.module_ <- old_config.module_;
    config.has_component <- old_config.has_component
  in
  let signature mapper items =
    let old_config = save_config () in
    config.has_component <- false;
    let result =
      List.map
        (fun item ->
          (match item.psig_desc with
          | Psig_attribute attr -> process_config_attribute attr config
          | _ -> ());
          let item = default_mapper.signature_item mapper item in
          if config.version = 4 then transform_signature_item4 item else [item])
        items
      |> List.flatten
    in
    restore_config old_config;
    result
  in
  let structure mapper items =
    let old_config = save_config () in
    config.has_component <- false;
    let result =
      List.map
        (fun item ->
          (match item.pstr_desc with
          | Pstr_attribute attr -> process_config_attribute attr config
          | _ -> ());
          let item = default_mapper.structure_item mapper item in
          if config.version = 4 then transform_structure_item4 item else [item])
        items
      |> List.flatten
    in
    restore_config old_config;
    result
  in

  {default_mapper with expr; module_binding; signature; structure}

let rewrite_implementation ~jsx_version ~jsx_module (code : Parsetree.structure)
    : Parsetree.structure =
  let config =
    {
      Jsx_common.version = jsx_version;
      module_ = jsx_module;
      nested_modules = [];
      has_component = false;
    }
  in
  let mapper = get_mapper ~config in
  mapper.structure mapper code

let rewrite_signature ~jsx_version ~jsx_module (code : Parsetree.signature) :
    Parsetree.signature =
  let config =
    {
      Jsx_common.version = jsx_version;
      module_ = jsx_module;
      nested_modules = [];
      has_component = false;
    }
  in
  let mapper = get_mapper ~config in
  mapper.signature mapper code
