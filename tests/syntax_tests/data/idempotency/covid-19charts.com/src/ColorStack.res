module Map = Belt.Map.String
module Set = Belt.Set.String
module Stack = Belt.MutableStack

type color =
  | Color(string, string)
  | Fallback

type t = {
  colors: Stack.t<color>,
  associations: Map.t<color>,
}

let white = "#fff"
let black = "#000"
let fallbackColor = ("#878787", white)
let colors = [
  Color("#a50026", white),
  Color("#fdae61", black),
  Color("#313695", white),
  Color("#d73027", white),
  Color("#fee090", black),
  Color("#abd9e9", black),
  Color("#f46d43", white),
  Color("#74add1", white),
  Color("#4575b4", white),
]

let initialColors = {
  let stack = Stack.make()
  Js.Array.forEach(color => Stack.push(stack, color), colors)
  stack
}

let popColor = colorQueue =>
  switch Stack.pop(colorQueue) {
  | Some(color) => color
  | None => Fallback
  }

let make = (~locations) => {
  let colors = Stack.copy(initialColors)
  {
    associations: Js.Array.reduce((associations, location) => {
      let color = popColor(colors)
      Map.set(associations, location, color)
    }, Map.empty, locations),
    colors: colors,
  }
}

let updateColors = (~locations, {colors, associations}) => {
  let colors = Stack.copy(colors)
  let locations = Set.fromArray(locations)
  let prevLocations = Map.keysToArray(associations)->Set.fromArray
  let removed = Set.diff(prevLocations, locations)

  Set.forEach(removed, location =>
    switch Map.get(associations, location) {
    | Some(Color(_) as color) => Stack.push(colors, color)
    | _ => ()
    }
  )

  {
    associations: Set.reduce(locations, Map.empty, (newAssociations, location) =>
      switch Map.get(associations, location) {
      | Some(x) => Map.set(newAssociations, location, x)
      | None =>
        let color = popColor(colors)
        Map.set(newAssociations, location, color)
      }
    ),
    colors: colors,
  }
}

let getColor = (~location, {associations}) =>
  switch Map.getWithDefault(associations, location, Fallback) {
  | Color(primaryColor, secondaryColor) => (primaryColor, secondaryColor)
  | Fallback => fallbackColor
  }
