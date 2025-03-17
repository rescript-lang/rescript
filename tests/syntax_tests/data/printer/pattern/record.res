let {x} = 1
let {x, _} = 1
let {x, y} = 1
let {x, y, _} = 1
let {x: xCoord} = 1
let {x: xCoord, _} = 1
let {x: xCoord, y: yCoord} = 1
let {x: xCoord, y: yCoord, _} = 1
let {xxxxxxxxxxxxxxxxxxxxxxxxx, yyyyyyyyyyyyyyyyyyyyyyyyy, zzzzzzzzzzzzzzzzzzzzzzzzz } = 1
let {xxxxxxxxxxxxxxxxxxxxxxxxx, yyyyyyyyyyyyyyyyyyyyyyyyy, zzzzzzzzzzzzzzzzzzzzzzzzz, _ } = 1

let {x: y} = z
let {x: x as y} = z

let get_age3 = ({age: (age2: int), name: _}) => age2
let get_age3 = ({age: _, name: _}) => age2
let get_age3 = ({age: age2, name: _}) => age2
let get_age3 = ({age: 3.14, name: _}) => age2
let get_age3 = ({age: 3, name: _}) => age2
let get_age3 = ({age: true, name: _}) => age2
let get_age3 = ({age: false, name: _}) => age2
let get_age3 = ({age: "100", name: _}) => age2
let get_age3 = ({age: 1 .. 2, name: _}) => age2
let get_age3 = ({age: 'a' .. 'z', name: _}) => age2
let get_age3 = ({age: (a, b, c, d), name: _}) => age2
let get_age3 = ({age: (aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbb, cccccccccccccccccccccc, ddddddddddddddddddddddddddddddd), name: _}) => age2
let get_age3 = ({age: Rgb(r, g, b), name: _}) => age2
let get_age3 = ({age: Rgb({r, g, b}), name: _}) => age2
let get_age3 = ({age: Rgb({r: red, g: green, b: black}), name: _}) => age2
let get_age3 = ({age: Rgb(rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr, gggggggggggggggggggggggggggggggg, bbbbbbbbbbbbbbbbbbbbbbbbbbbb), name: _}) => age2
let get_age3 = ({age: Rgb({r: rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr, g: gggggggggggggggggggggggggg, b: bbbbbbbbbbbbbbbb}), name: _}) => age2


let get_age3 = ({age: #red, name: _}) => age2
let get_age3 = ({age: #Blue, name: _}) => age2
let get_age3 = ({age: #Rgb(r, g, b), name: _}) => age2
let get_age3 = ({age: #Rgb(rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr, gggggggggggggggggggggggggggggggg, bbbbbbbbbbbbbbbbbbbbbbbbbbbb), name: _}) => age2
let get_age3 = ({age: #Rgb({r: rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr,g: gggggggggggggggggggggggggggggggg, b: bbbbbbbbbbbbbbbbbbbbbbbbbbbb}), name: _}) => age2
let get_age3 = ({age: {birthDay: day, birthMonth: month}, name: _}) => age2
let get_age3 = ({age: {birthDay: daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaay, birthMonth: moooooooooooooooooooooooooooooooooonth}, name: _}) => age2

let get_age3 = ({age: [], name: _}) => age2
let get_age3 = ({age: [a, b, c, d], name: _}) => age2
let get_age3 = ({age: [aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbb, cccccccccccccccccccccc, ddddddddddddddddddddddddddddddd], name: _}) => age2


let get_age3 = ({age: list{}, name: _}) => age2
let get_age3 = ({age: list{a, b, c, d}, name: _}) => age2
let get_age3 = ({age: list{aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbb, cccccccccccccccccccccc, ddddddddddddddddddddddddddddddd}, name: _}) => age2

let get_age3 = ({age: Red | Blue | Green, name: _}) => age2

let get_age3 = ({age: #...ppatType, name: _}) => age2

let get_age3 = ({age: module(P), name: _}) => age2
let get_age3 = ({age: module(P: S), name: _}) => age2

let get_age3 = ({age: exception Exit, name: _}) => age2

let get_age3 = ({age: %raw("__GC"), name: _}) => age2

let get_age3 = ({age, _}) => age
let get_age3 = ({_}) => ""
let get_age3 = () => 
  switch x {
  | {age, _} => age
  }
let get_age3 = () => 
  switch x {
  | {_} => ""
  }
