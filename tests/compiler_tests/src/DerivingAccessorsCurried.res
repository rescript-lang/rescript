//Assert that deriving accessors does not break
//In curried mode

@deriving(accessors)
type myRecord = {myField: int}

@deriving(accessors)
type variant = NoParam | Num(int) | DoubleNum(int, int)

//Asserts the correct signature for derived accessor
let _myFieldAlias: myRecord => int = myField
let _noParamAlias: variant = noParam
let _numAlias: int => variant = num
let _doubleNumAlias: (int, int) => variant = doubleNum

//Asserts that inference works when composing
//with derived functions
let compose = (a, accessor) => accessor(a)
let _composedMyField = compose({myField: 1}, myField)
let _composedNum = compose(1, num)
