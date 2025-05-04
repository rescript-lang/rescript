console.log()
Js.log("arg1", "arg2")
let rbt = make(~compare)
let rbt = make(~compare?)
let rbt = make(~compare=intCompare)
let rbt = make(~compare=?intCompare)
let rbt = make(~compare=?intCompare: (int, int) => int)
let rbt = make((~compare))
let rbt = make((~-compare) ^ ~x)

let () = applyFunctionToArguments(
  superLongIdentifierWooooooowThisIsSuchLong,
  superLongIdentifierWooooooowThisIsSuchLong,
  superLongIdentifierWooooooowThisIsSuchLong,
  superLongIdentifierWooooooowThisIsSuchLong,
)

let cmp = rbt.compare(. Js.Array2.unsafe_get(old, oldIter.contents), node.value)
let cmp = rbt.compare2(. Js.Array2.unsafe_get(old, oldIter.contents), longerNode.longValue)
let uncurriedUnit = apply(.)

let coordinate = make2dCoordinate({x: 1, y: 2})
let coordinate = make3dCoordinate({...base, field1: thisIsAPrettyLongNameHere, field2: thisIsAPrettyLongNameHere, field3: thisIsAPrettyLongNameHere})

let coordinate = make2dCoordinateArray([x, y])
let coordinate = make3dCoordinateArray([thisIsAPrettyLongNameHere, thisIsAPrettyLongNameHere,thisIsAPrettyLongNameHere])

let coordinate = make2dCoordinateTuple((x, y))
let coordinate = make3dCoordinateTuple((thisIsAPrettyLongNameHere, thisIsAPrettyLongNameHere,thisIsAPrettyLongNameHere))

let coordinate = make2dCoordinateList(list{x, y})
let coordinate = make3dCoordinateList(list{thisIsAPrettyLongNameHere, thisIsAPrettyLongNameHere,thisIsAPrettyLongNameHere, ...allCoords})

let coordinate = makeJsCoordinate({"x": 1, "y": 1})
let user = makeJsUser({
  "name": "steve",
  "age":  32
})

let x = @attr callFunction()
let x = @attrWithLongName @attrWithLongName @attrWithLongName @attrWithLongName callFunction()

(a |> f)(b, c)

call(~a: int)
call(~\"let": int)

document.createElementWithOptions(. "div", elementProps(~onClick=_ =>
    Js.log("hello world")
  ))

f(. 1)
f(. [1, 2, 3])
f(. [suuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuperLong, suuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuperLong, suuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuperLong,])
f(. (1, 2, 3))
f(. {"a": 1})
f(. list{1})
f(. `
 template
 string
`)
f(. `single line`)
f(. "
 template
 string
")
f(. {
  expr 
})
f(. {expr})
f(. {
  exception Exit
  raise(Exit)
})

resolve(.)
resolve(. ())

let g =  f(
  a => LongModuleName.functionWithAlongNameThatWrapsTheEditorToTheNextLinexxxxxxxxxxxxxxxxxxxxxx(a),
  b,
  c
)

let g =  f(
  a => LongModuleName.functionWithAlongNameThatWrapsTheEditorToTheNextLinexxxxxxxxxxxxxxxxxxxxxx(a),
  b,
  c, 
  ...
)

let g =  f(
  a => LongModuleName.functionWithAlongNameThatWrapsTheEditorToTheNextLinexxxxxxxxxxxxxxxxxxxxxx(a),
  b,
  c => LongModuleName.functionWithAlongNameThatWrapsTheEditorToTheNextLinexxxxxxxxxxxxxxxxxxxxxx(d, ...),
  ...
)

let g =  f(
  a => LongModuleName.functionWithAlongNameThatWrapsTheEditorToTheNextLinexxxxxxxxxxxxxxxxxxxxxx(a, ...),
  b,
  c => LongModuleName.functionWithAlongNameThatWrapsTheEditorToTheNextLinexxxxxxxxxxxxxxxxxxxxxx(d),
  ...
)


let g =  f(
  a => LongModuleName.functionWithAlongNameThatWrapsTheEditorToTheNextLinexxxxxxxxxxxxxxxxxxxxxx(a),
  b,
  c => LongModuleName.functionWithAlongNameThatWrapsTheEditorToTheNextLinexxxxxxxxxxxxxxxxxxxxxx(d, ...)
)
