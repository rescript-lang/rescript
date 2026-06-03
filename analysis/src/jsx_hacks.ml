let path_is_fragment path = Path.name path = "ReasonReact.fragment"

let primitive_is_fragment (vd : Typedtree.value_description) =
  vd.val_name.txt = "fragment"
  && vd.val_loc.loc_start.pos_fname |> Filename.basename = "ReasonReact.res"
