open Shared_types

(* List and explanations taken from
   https://www.tutorialrepublic.com/html-reference/html5-tags.php. *)
let html_elements =
  [
    ("a", "Defines a hyperlink.", false);
    ("abbr", "Defines an abbreviated form of a longer word or phrase.", false);
    ("acronym", "Defines an acronym. Use <abbr> instead.", true);
    ("address", "Specifies the author's contact information.", false);
    ( "applet",
      "Embeds a Java applet (mini Java applications) on the page. Use <object> \
       instead.",
      true );
    ("area", "Defines a specific area within an image map.", false);
    ("article", "Defines an article.", false);
    ("aside", "Defines some content loosely related to the page content.", false);
    ("audio", "Embeds a sound, or an audio stream in an HTML document.", false);
    ("b", "Displays text in a bold style.", false);
    ("base", "Defines the base URL for all relative URLs in a document.", false);
    ("basefont", "Specifies the base font for a page. Use CSS instead.", true);
    ( "bdi",
      "Represents text that is isolated from its surrounding for the purposes \
       of bidirectional text formatting.",
      false );
    ("bdo", "Overrides the current text direction.", false);
    ("big", "Displays text in a large size. Use CSS instead.", true);
    ( "blockquote",
      "Represents a section that is quoted from another source.",
      false );
    ("body", "Defines the document's body.", false);
    ("br", "Produces a single line break.", false);
    ("button", "Creates a clickable button.", false);
    ( "canvas",
      "Defines a region in the document, which can be used to draw graphics on \
       the fly via scripting (usually JavaScript).",
      false );
    ("caption", "Defines the caption or title of the table.", false);
    ("center", "Align contents in the center. Use CSS instead.", true);
    ("cite", "Indicates a citation or reference to another source.", false);
    ("code", "Specifies text as computer code.", false);
    ( "col",
      "Defines attribute values for one or more columns in a table.",
      false );
    ("colgroup", "Specifies attributes for multiple columns in a table.", false);
    ( "data",
      "Links a piece of content with a machine-readable translation.",
      false );
    ( "datalist",
      "Represents a set of pre-defined options for an <input> element.",
      false );
    ( "dd",
      "Specifies a description, or value for the term (<dt>) in a description \
       list (<dl>).",
      false );
    ("del", "Represents text that has been deleted from the document.", false);
    ( "details",
      "Represents a widget from which the user can obtain additional \
       information or controls on-demand.",
      false );
    ("dfn", "Specifies a definition.", false);
    ("dialog", "Defines a dialog box or subwindow.", false);
    ("dir", "Defines a directory list. Use <ul> instead.", true);
    ("div", "Specifies a division or a section in a document.", false);
    ("dl", "Defines a description list.", false);
    ("dt", "Defines a term (an item) in a description list.", false);
    ("em", "Defines emphasized text.", false);
    ( "embed",
      "Embeds external application, typically multimedia content like audio or \
       video into an HTML document.",
      false );
    ("fieldset", "Specifies a set of related form fields.", false);
    ("figcaption", "Defines a caption or legend for a figure.", false);
    ("figure", "Represents a figure illustrated as part of the document.", false);
    ("font", "Defines font, color, and size for text. Use CSS instead.", true);
    ("footer", "Represents the footer of a document or a section.", false);
    ("form", "Defines an HTML form for user input.", false);
    ("frame", "Defines a single frame within a frameset.", true);
    ("frameset", "Defines a collection of frames or other frameset.", true);
    ( "head",
      "Defines the head portion of the document that contains information \
       about the document such as title.",
      false );
    ("header", "Represents the header of a document or a section.", false);
    ("hgroup", "Defines a group of headings.", false);
    ("h1", "Defines HTML headings.", false);
    ("h2", "Defines HTML headings.", false);
    ("h3", "Defines HTML headings.", false);
    ("h4", "Defines HTML headings.", false);
    ("h5", "Defines HTML headings.", false);
    ("h6", "Defines HTML headings.", false);
    ("hr", "Produce a horizontal line.", false);
    ("html", "Defines the root of an HTML document.", false);
    ("i", "Displays text in an italic style.", false);
    ("iframe", "Displays a URL in an inline frame.", false);
    ("img", "Represents an image.", false);
    ("input", "Defines an input control.", false);
    ( "ins",
      "Defines a block of text that has been inserted into a document.",
      false );
    ("kbd", "Specifies text as keyboard input.", false);
    ( "keygen",
      "Represents a control for generating a public-private key pair.",
      false );
    ("label", "Defines a label for an <input> control.", false);
    ("legend", "Defines a caption for a <fieldset> element.", false);
    ("li", "Defines a list item.", false);
    ( "link",
      "Defines the relationship between the current document and an external \
       resource.",
      false );
    ("main", "Represents the main or dominant content of the document.", false);
    ("map", "Defines a client-side image-map.", false);
    ("mark", "Represents text highlighted for reference purposes.", false);
    ("menu", "Represents a list of commands.", false);
    ( "menuitem",
      "Defines a list (or menuitem) of commands that a user can perform.",
      false );
    ("meta", "Provides structured metadata about the document content.", false);
    ("meter", "Represents a scalar measurement within a known range.", false);
    ("nav", "Defines a section of navigation links.", false);
    ( "noframes",
      "Defines an alternate content that displays in browsers that do not \
       support frames.",
      true );
    ( "noscript",
      "Defines alternative content to display when the browser doesn't support \
       scripting.",
      false );
    ("object", "Defines an embedded object.", false);
    ("ol", "Defines an ordered list.", false);
    ( "optgroup",
      "Defines a group of related options in a selection list.",
      false );
    ("option", "Defines an option in a selection list.", false);
    ("output", "Represents the result of a calculation.", false);
    ("p", "Defines a paragraph.", false);
    ("param", "Defines a parameter for an object or applet element.", false);
    ("picture", "Defines a container for multiple image sources.", false);
    ("pre", "Defines a block of preformatted text.", false);
    ("progress", "Represents the completion progress of a task.", false);
    ("q", "Defines a short inline quotation.", false);
    ( "rp",
      "Provides fall-back parenthesis for browsers that that don't support \
       ruby annotations.",
      false );
    ( "rt",
      "Defines the pronunciation of character presented in a ruby annotations.",
      false );
    ("ruby", "Represents a ruby annotation.", false);
    ( "s",
      "Represents contents that are no longer accurate or no longer relevant.",
      false );
    ("samp", "Specifies text as sample output from a computer program.", false);
    ( "script",
      "Places script in the document for client-side processing.",
      false );
    ( "section",
      "Defines a section of a document, such as header, footer etc.",
      false );
    ("select", "Defines a selection list within a form.", false);
    ("small", "Displays text in a smaller size.", false);
    ( "source",
      "Defines alternative media resources for the media elements like <audio> \
       or <video>.",
      false );
    ("span", "Defines an inline styleless section in a document.", false);
    ("strike", "Displays text in strikethrough style.", true);
    ("strong", "Indicate strongly emphasized text.", false);
    ( "style",
      "Inserts style information (commonly CSS) into the head of a document.",
      false );
    ("sub", "Defines subscripted text.", false);
    ("summary", "Defines a summary for the <details> element.", false);
    ("sup", "Defines superscripted text.", false);
    ( "svg",
      "Embed SVG (Scalable Vector Graphics) content in an HTML document.",
      false );
    ("table", "Defines a data table.", false);
    ( "tbody",
      "Groups a set of rows defining the main body of the table data.",
      false );
    ("td", "Defines a cell in a table.", false);
    ( "template",
      "Defines the fragments of HTML that should be hidden when the page is \
       loaded, but can be cloned and inserted in the document by JavaScript.",
      false );
    ("textarea", "Defines a multi-line text input control (text area).", false);
    ( "tfoot",
      "Groups a set of rows summarizing the columns of the table.",
      false );
    ("th", "Defines a header cell in a table.", false);
    ( "thead",
      "Groups a set of rows that describes the column labels of a table.",
      false );
    ("time", "Represents a time and/or date.", false);
    ("title", "Defines a title for the document.", false);
    ("tr", "Defines a row of cells in a table.", false);
    ( "track",
      "Defines text tracks for the media elements like <audio> or <video>.",
      false );
    ("tt", "Displays text in a teletype style.", true);
    ("u", "Displays text with an underline.", false);
    ("ul", "Defines an unordered list.", false);
    ("var", "Defines a variable.", false);
    ("video", "Embeds video content in an HTML document.", false);
    ("wbr", "Represents a line break opportunity.", false);
  ]

let get_jsx_labels ~component_path ~find_type_of_value ~package =
  match component_path @ ["make"] |> find_type_of_value with
  | Some (typ, make_env) ->
    let get_fields ~path ~type_args =
      match References.dig_constructor ~env:make_env ~package path with
      | Some
          ( env,
            {
              item =
                {
                  decl =
                    {type_kind = Type_record (label_decls, _repr); type_params};
                };
            } ) ->
        label_decls
        |> List.map (fun (ld : Types.label_declaration) ->
               let name = Ident.name ld.ld_id in
               let t =
                 ld.ld_type
                 |> Type_utils.instantiate_type ~type_params ~type_args
               in
               (name, t, env))
      | _ -> []
    in
    let rec get_labels (t : Types.type_expr) =
      match t.desc with
      | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> get_labels t1
      | Tconstr (p, [props_type], _) when Path.name p = "React.component" -> (
        let rec get_props_type (t : Types.type_expr) =
          match t.desc with
          | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> get_props_type t1
          | Tconstr (path, type_args, _) when Path.last path = "props" ->
            Some (path, type_args)
          | _ -> None
        in
        match props_type |> get_props_type with
        | Some (path, type_args) -> get_fields ~path ~type_args
        | None -> [])
      | Tarrow
          ({lbl = Nolabel; typ = {desc = Tconstr (path, type_args, _)}}, _, _, _)
        when Path.last path = "props" ->
        get_fields ~path ~type_args
      | Tconstr (cl_path, [{desc = Tconstr (path, type_args, _)}; _], _)
        when Path.name cl_path = "React.componentLike"
             && Path.last path = "props" ->
        (* JSX V4 external or interface *)
        get_fields ~path ~type_args
      | Tarrow ({lbl = Nolabel; typ}, _, _, _) -> (
        (* Component without the JSX PPX, like a make fn taking a hand-written
           type props. *)
        let rec dig_to_constr typ =
          match typ.Types.desc with
          | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> dig_to_constr t1
          | Tconstr (path, type_args, _) when Path.last path = "props" ->
            Some (path, type_args)
          | _ -> None
        in
        match dig_to_constr typ with
        | None -> []
        | Some (path, type_args) -> get_fields ~path ~type_args)
      | _ -> []
    in
    typ |> get_labels
  | None -> []

type prop = {
  name: string;
  pos_start: int * int;
  pos_end: int * int;
  exp: Parsetree.expression;
}

type jsx_props = {
  comp_name: Longident.t Location.loc;
  props: prop list;
  children_start: (int * int) option;
}

(** 
<div muted= />

This is a special case for JSX props, where the above code is parsed 
as <div muted=//, a regexp literal. We leverage that fact to trigger completion
for the JSX prop value.

This code is safe because we also check that the location of the expression is broken,
which only happens when the expression is a parse error/not complete.
*)
let is_regexp_jsx_heuristic_expr expr =
  match expr.Parsetree.pexp_desc with
  | Pexp_extension
      ( {txt = "re"},
        PStr
          [
            {
              pstr_desc =
                Pstr_eval
                  ({pexp_desc = Pexp_constant (Pconst_string ("//", _))}, _);
            };
          ] )
    when expr.pexp_loc |> Loc.end_ = (Location.none |> Loc.end_) ->
    true
  | _ -> false

let find_jsx_props_completable ~jsx_props ~end_pos ~pos_before_cursor
    ~first_char_before_cursor_no_white ~char_at_cursor ~pos_after_comp_name =
  let all_labels =
    List.fold_right
      (fun prop all_labels -> prop.name :: all_labels)
      jsx_props.props []
  in
  let before_children_start =
    match jsx_props.children_start with
    | Some children_pos -> pos_before_cursor < children_pos
    | None -> pos_before_cursor <= end_pos
  in
  let rec loop props =
    match props with
    | prop :: rest ->
      if prop.pos_start <= pos_before_cursor && pos_before_cursor < prop.pos_end
      then (
        if Debug.verbose () then
          print_endline "[jsx_props_completable]--> Cursor on the prop name";

        Some
          (Completable.Cjsx
             ( Utils.flatten_long_ident ~jsx:true jsx_props.comp_name.txt,
               prop.name,
               all_labels )))
      else if
        prop.pos_end <= pos_before_cursor
        && pos_before_cursor < Loc.start prop.exp.pexp_loc
      then (
        if Debug.verbose () then
          print_endline
            "[jsx_props_completable]--> Cursor between the prop name and expr \
             assigned";
        match (first_char_before_cursor_no_white, prop.exp) with
        | Some '=', {pexp_desc = Pexp_ident {txt = Lident txt}} ->
          if Debug.verbose () then
            Printf.printf
              "[jsx_props_completable]--> Heuristic for empty JSX prop expr \
               completion.\n";
          Some
            (Cexpression
               {
                 context_path =
                   CJsxPropValue
                     {
                       path_to_component =
                         Utils.flatten_long_ident ~jsx:true
                           jsx_props.comp_name.txt;
                       prop_name = prop.name;
                       empty_jsx_prop_name_hint = Some txt;
                     };
                 nested = [];
                 prefix = "";
               })
        | _ -> None)
      else if prop.exp.pexp_loc |> Loc.has_pos ~pos:pos_before_cursor then (
        if Debug.verbose () then
          print_endline "[jsx_props_completable]--> Cursor on expr assigned";
        match
          Completion_expressions.traverse_expr prop.exp ~expr_path:[]
            ~pos:pos_before_cursor ~first_char_before_cursor_no_white
        with
        | Some (prefix, nested) ->
          Some
            (Cexpression
               {
                 context_path =
                   CJsxPropValue
                     {
                       path_to_component =
                         Utils.flatten_long_ident ~jsx:true
                           jsx_props.comp_name.txt;
                       prop_name = prop.name;
                       empty_jsx_prop_name_hint = None;
                     };
                 nested = List.rev nested;
                 prefix;
               })
        | _ -> None)
      else if prop.exp.pexp_loc |> Loc.end_ = (Location.none |> Loc.end_) then (
        if Debug.verbose () then
          print_endline "[jsx_props_completable]--> Loc is broken";
        if
          Completion_expressions.is_expr_hole prop.exp
          || is_regexp_jsx_heuristic_expr prop.exp
        then (
          if Debug.verbose () then
            print_endline
              "[jsx_props_completable]--> Expr was expr hole or regexp literal \
               heuristic";
          Some
            (Cexpression
               {
                 context_path =
                   CJsxPropValue
                     {
                       path_to_component =
                         Utils.flatten_long_ident ~jsx:true
                           jsx_props.comp_name.txt;
                       prop_name = prop.name;
                       empty_jsx_prop_name_hint = None;
                     };
                 prefix = "";
                 nested = [];
               }))
        else None)
      else if
        rest = [] && before_children_start && char_at_cursor = '>'
        && first_char_before_cursor_no_white = Some '='
      then (
        (* This is a special case for: <SomeComponent someProp=> (completing directly after the '=').
           The completion comes at the end of the component, after the equals sign, but before any
           children starts, and '>' marks that it's at the end of the component JSX.
           This little heuristic makes sure we pick up this special case. *)
        if Debug.verbose () then
          print_endline
            "[jsx_props_completable]--> Special case: last prop, '>' after \
             cursor";
        Some
          (Cexpression
             {
               context_path =
                 CJsxPropValue
                   {
                     path_to_component =
                       Utils.flatten_long_ident ~jsx:true
                         jsx_props.comp_name.txt;
                     prop_name = prop.name;
                     empty_jsx_prop_name_hint = None;
                   };
               prefix = "";
               nested = [];
             }))
      else loop rest
    | [] ->
      let after_comp_name = pos_before_cursor >= pos_after_comp_name in
      if after_comp_name && before_children_start then (
        if Debug.verbose () then
          print_endline "[jsx_props_completable]--> Complete for JSX prop name";
        Some
          (Cjsx
             ( Utils.flatten_long_ident ~jsx:true jsx_props.comp_name.txt,
               "",
               all_labels )))
      else None
  in
  loop jsx_props.props

let extract_jsx_props ~(comp_name : Longident.t Location.loc) ~props ~children =
  let open Parsetree in
  let children_start =
    match children with
    | [] -> None
    | child :: _ ->
      if child.pexp_loc.loc_ghost then None else Some (Loc.start child.pexp_loc)
  in
  let props =
    props
    |> List.map (function
         | JSXPropPunning (_, name) ->
           {
             name = name.txt;
             pos_start = Loc.start name.loc;
             pos_end = Loc.end_ name.loc;
             exp =
               Ast_helper.Exp.ident ~loc:name.loc
                 {txt = Longident.Lident name.txt; loc = name.loc};
           }
         | JSXPropValue (name, _, value) ->
           {
             name = name.txt;
             pos_start = Loc.start name.loc;
             pos_end = Loc.end_ name.loc;
             exp = value;
           }
         | JSXPropSpreading (loc, expr) ->
           {
             name = "_spreadProps";
             pos_start = Loc.start loc;
             pos_end = Loc.end_ loc;
             exp = expr;
           })
  in
  {comp_name; props; children_start}
