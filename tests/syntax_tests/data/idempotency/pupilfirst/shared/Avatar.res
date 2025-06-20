let colors = [
  ("#ff4040", false),
  ("#7f2020", false),
  ("#cc5c33", false),
  ("#734939", false),
  ("#bf9c8f", false),
  ("#995200", false),
  ("#4c2900", false),
  ("#f2a200", false),
  ("#ffd580", true),
  ("#332b1a", false),
  ("#4c3d00", false),
  ("#ffee00", true),
  ("#b0b386", false),
  ("#64664d", false),
  ("#6c8020", false),
  ("#c3d96c", true),
  ("#143300", false),
  ("#19bf00", false),
  ("#53a669", false),
  ("#bfffd9", true),
  ("#40ffbf", true),
  ("#1a332e", false),
  ("#00b3a7", false),
  ("#165955", false),
  ("#00b8e6", false),
  ("#69818c", false),
  ("#005ce6", false),
  ("#6086bf", false),
  ("#000e66", false),
  ("#202440", false),
  ("#393973", false),
  ("#4700b3", false),
  ("#2b0d33", false),
  ("#aa86b3", false),
  ("#ee00ff", false),
  ("#bf60b9", false),
  ("#4d3949", false),
  ("#ff00aa", false),
  ("#7f0044", false),
  ("#f20061", false),
  ("#330007", false),
  ("#d96c7b", false),
]

let initials = name =>
  name
  ->Js.String.split(" ")
  ->Js.Array.slice(~start=0, ~end_=2)
  ->Js.Array.map(word => word->Js.String.slice(~from=0, ~to_=1))
  ->Js.Array.joinWith("")

let stringToInt = name => {
  let rec aux = (sum, remains) =>
    switch remains {
    | "" => sum
    | remains =>
      let firstCharacter = remains->Js.String.slice(~from=0, ~to_=1)
      let remains = remains->Js.String.sliceToEnd(~from=1)
      aux(sum +. (firstCharacter->Js.String.charCodeAt(0)), remains)
    }

  aux(0.0, name)->int_of_float
}

let computeColors = name => {
  let index = mod(name->stringToInt, 42)
  let (backgroundColor, blackText) = colors[index]
  (backgroundColor, blackText ? "#000000" : "#FFFFFF")
}

@react.component
let make = (~colors=?, ~name, ~className=?) => {
  let (bgColor, fgColor) = switch colors {
  | Some((bgColor, fgColor)) => (bgColor, fgColor)
  | None => computeColors(name)
  }

  <svg xmlns="http://www.w3.org/2000/svg" version="1.1" viewBox="0 0 100 100" title=name ?className>
    <circle cx="50" cy="50" r="50" fill=bgColor />
    <text
      fill=fgColor
      fontSize="42"
      fontFamily="sans-serif"
      x="50"
      y="54"
      textAnchor="middle"
      dominantBaseline="middle"
      alignmentBaseline="middle">
      {initials(name)->React.string}
    </text>
  </svg>
}
