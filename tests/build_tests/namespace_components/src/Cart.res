// Modules of the package, found through the namespace it opens
let total = Price.add(Price.make(40), Price.make(Tax.rate)).cents

module P = Price
let viaAlias = P.make(1).cents

let viaOpen = {
  open Price
  make(3).cents
}

// The module named like the namespace
let shopName = Shop.name
