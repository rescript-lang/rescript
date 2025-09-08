type fakeDict<'t> = {dictValuesType?: 't}

let foo = (fakeDict: fakeDict<'a>) => {
  switch fakeDict {
  | {someUndefinedField: 1} => Js.log("one")
  | _ => Js.log("not one")
  }
}
