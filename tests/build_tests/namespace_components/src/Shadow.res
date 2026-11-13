// A local module hides the module of the package with the same name
module Price = {
  let make = _ => "local"
}

let label = Price.make(1)
