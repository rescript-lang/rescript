type fakeDict<'t> = {dictValuesType?: 't}

let foo = (fakeDict: fakeDict<'a>) => {
  switch fakeDict {
  | {someUndefinedField: 1} => Console.log("one")
  | _ => Console.log("not one")
  }
}
