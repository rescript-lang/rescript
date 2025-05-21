type record = {one: string}

external getRecord: unit => promise<record> = "getRecord"

let x = () => {
  let res = Promise.resolve({one: "hi"})
  res.one
}
