let pathIsFragment path = Path.name path = "ReasonReact.fragment"

let primitiveIsFragment (vd : Typedtree.value_description) =
  vd.val_name.txt = "fragment"
  && vd.val_loc.loc_start.pos_fname |> Filename.basename = "ReasonReact.res"
