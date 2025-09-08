module WithVariant = {
  type t = One({miss: bool}) | Two(bool)
}

let x = WithVariant.One()
//                      ^com
