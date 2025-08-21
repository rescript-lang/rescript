@@config({flags: ["-enable-experimental", "LetUnwrap"]})

let doStuffWithResult = s =>
  switch s {
  | "s" => Ok("hello")
  | _ => Error(#InvalidString)
  }

let doNextStuffWithResult = s =>
  switch s {
  | "s" => Ok("hello")
  | _ => Error(#InvalidNext)
  }

let getXWithResult = s => {
  let? Ok(y) = doStuffWithResult(s)
  let? Ok(x) = doNextStuffWithResult(y)
  Ok(x ++ y)
}

let someResult = switch getXWithResult("s") {
| Ok(x) => x
| Error(#InvalidString) => "nope"
| Error(#InvalidNext) => "nope!"
}

let doStuffWithOption = s =>
  switch s {
  | "s" => Some("hello")
  | _ => None
  }

let doNextStuffWithOption = s =>
  switch s {
  | "s" => Some("hello")
  | _ => None
  }

let getXWithOption = s => {
  let? Some(y) = doStuffWithOption(s)
  let? Some(x) = doNextStuffWithOption(y)
  Some(x ++ y)
}

let someOption = switch getXWithOption("s") {
| Some(x) => x
| None => "nope"
}

type res = {s: string}

let doStuffResultAsync = async s => {
  switch s {
  | "s" => Ok({s: "hello"})
  | _ => Error(#FetchError)
  }
}

let decodeResAsync = async res => {
  switch res.s {
  | "s" => Ok(res.s)
  | _ => Error(#DecodeError)
  }
}

let getXWithResultAsync = async s => {
  let? Ok({s} as res) = await doStuffResultAsync(s)
  Console.log(s)
  let? Ok(x) = await decodeResAsync(res)
  Ok(x)
}

let returnsAliasOnFirstError = s => {
  let? Ok(_y) = doStuffWithResult(s)
  Ok("ok")
}

let returnsAliasOnSecondError = s => {
  let? Ok(y) = doStuffWithResult(s)
  let? Ok(_x) = doNextStuffWithResult(y)
  Ok("ok")
}

let returnsAliasOnOk = s => {
  let? Error(_e) = doStuffWithResult(s)
  Error(#GotError)
}
