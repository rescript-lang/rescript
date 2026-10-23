// Implementations whose representation the compiler does not see, an
// abstract type through an abbreviation and a JavaScript object type, agree
// with @notUndefined in the interface
type opaque
type element = opaque
type record = {"name": string}

let makeElement: unit => element = %raw(`() => ({})`)
let makeRecord = () => {"name": "a"}

Mocha.describe(__MODULE__, () => {
  Mocha.test("option of the types is unboxed", () => {
    let element = makeElement()
    Test_utils.ok(__LOC__, Obj.magic(Some(element)) === element)
  })
})
