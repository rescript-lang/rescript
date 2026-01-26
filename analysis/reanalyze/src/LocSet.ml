include Set.Make (struct
  include Location

  let compare = compare
end)
