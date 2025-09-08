(*
  An overview of the implementation of dicts in ReScript:
  ### What is a dict?
  Dicts are effectively an object with unknown fields, but a single known type of the values it holds.

  ### How are they implemented?
  Dicts in ReScript are implemented as predefined record type, with a single (magic) field that holds 
  the type of the dict's values. This field is called `dictValuesType`, and it represent every possible
  key in the dict. It's just an implementation detail - it's never actually exposed to the user, just 
  used internally. 

  The compiler will route any label lookup on the dict record type to the magic field, which creates a 
  record with unknown keys, but of a single type.

  The reason for this seemingly convoluted implementation is that it allows us to piggyback on the 
  existing record pattern matching mechanism, which means we get pattern matching on dicts for free.

  ### Modifications to the type checker
  We've made a few smaller modifications to the type checker to support this implementation:

  - We've added a new predefined type `dict` that is a record with a single field called `dictValuesType`.
    This type is used to represent the type of the values in a dict.
  - We've modified the type checker to recognize `dict` patterns, and route them to the predefined `dict` type.
    This allows us to get full inference for dicts in patterns. 
    
  ### Syntax
  There's first class syntax support for dicts, both as expressions and as patterns.
  A dict pattern is treated as a record pattern in the compiler and syntax, with an attriubute `@res.dictPattern`
  attached to it. This attribute is used to tell the compiler that the pattern is a dict pattern, and is what
  triggers the compiler to treat the dict record type differently to regular record types.
  *)
let dict_magic_field_name = "dictValuesType"

let has_dict_pattern_attribute attrs =
  attrs
  |> List.find_opt (fun (({txt}, _) : Parsetree.attribute) ->
         txt = "res.dictPattern")
  |> Option.is_some

let has_dict_attribute attrs =
  attrs
  |> List.find_opt (fun (({txt}, _) : Parsetree.attribute) -> txt = "res.$dict")
  |> Option.is_some

let dict_attr : Parsetree.attribute =
  (Location.mknoloc "res.$dict", Parsetree.PStr [])

let dict_magic_field_attr : Parsetree.attribute =
  (Location.mknoloc "res.$dictMagicField", Parsetree.PStr [])
