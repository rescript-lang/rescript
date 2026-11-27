type a = {"one": string, "two": int}

type b = {
  one: string,
  two: int,
}

type variant = One({...a}) | Two({...b})

let a = One({"one": "1", "two": 2})
let b = Two({one: "1", two: 2})
