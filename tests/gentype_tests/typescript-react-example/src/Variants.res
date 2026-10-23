@genType
type weekday = [
  | #monday
  | #tuesday
  | #wednesday
  | #thursday
  | #friday
  | #saturday
  | #sunday
]

@genType
let isWeekend = (x: weekday) =>
  switch x {
  | #saturday | #sunday => true
  | _ => false
  }

@genType let monday = #monday
@genType let saturday = #saturday
@genType let sunday = #sunday

@genType let onlySunday = (_: [#sunday]) => ()

@genType
let swap = x =>
  switch x {
  | #sunday => #saturday
  | #saturday => #sunday
  }

@genType
type testGenTypeAs = [
  | #type_
  | #module_
  | #fortytwo
]

@genType let testConvert = (x: testGenTypeAs) => x

@genType let fortytwoOK: testGenTypeAs = #fortytwo

@genType let fortytwoBAD = #fortytwo

@genType
type testGenTypeAs2 = [
  | #type_
  | #"module"
  | #42
]

@genType let testConvert2 = (x: testGenTypeAs2) => x

@genType
type testGenTypeAs3 = [
  | #type_
  | #"module"
  | #42
]

@genType let testConvert3 = (x: testGenTypeAs3) => x

/* This converts between testGenTypeAs2 and testGenTypeAs3 */
@genType let testConvert2to3 = (x: testGenTypeAs2): testGenTypeAs3 => x

@genType type x1 = [#x | #x1]

@genType type x2 = [#x | #x2]

@genType let id1 = (x: x1) => x

@genType let id2 = (x: x2) => x

@genType @genType.as("type")
type type_ = | @as("type") Type

@genType let typeCase = Type

@genType
type rec myList = E | C(int, myList)

@genType
type builtinList = list<int>

@genType
let polyWithOpt = foo =>
  foo === "bar"
    ? None
    : switch foo !== "baz" {
      | true => Some(#One(foo))
      | false => Some(#Two(1))
      }

@genType
type result1<'a, 'b> =
  | Ok('a)
  | Error('b)

@genType type result2<'a, 'b> = result<'a, 'b>

@genType type result3<'a, 'b> = Stdlib.Result.t<'a, 'b>

@genType type result4<'a, 'b> = Belt.Result.t<'a, 'b>

@genType let restResult1 = (x: result1<int, string>) => x

@genType let restResult2 = (x: result2<int, string>) => x

@genType let restResult3 = (x: result3<int, string>) => x
