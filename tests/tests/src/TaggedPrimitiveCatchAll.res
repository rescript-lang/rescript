@tag("kind")
type response =
  | @as(202) Ok202({code: int})
  | @as(200) Ok200({code: int})
  | @catch(int) Other({kind: int, body: string})

let asResponse = (x: 'a): response => Obj.magic(x)
let constructedOk202 = Ok202({code: 10})
let constructedOk200 = Ok200({code: 20})

let decode = (x: response) =>
  switch x {
  | Ok202(r) => r.code + 1
  | Other(r) => r.kind + 2
  | Ok200(r) => r.code + 3
  }

let decodePartial = (x: response) =>
  switch x {
  | Other(r) => r.kind + 2
  | Ok202(r) => r.code + 1
  | _ => 999
  }

let ok202 = asResponse(%raw(`({kind: 202, code: 1})`))
let ok200 = asResponse(%raw(`({kind: 200, code: 2})`))
let other404 = asResponse(%raw(`({kind: 404, body: "x"})`))

module NumAndStrCatchAll = {
  @tag("k")
  type t =
    | @as(200) Num200({v: int})
    | @as("ok") StrOk({msg: string})
    | @catch(int) OtherNum({@as("k") tag: int, x: int})
    | @catch(string) OtherStr({@as("k") tag: string, s: string})

  let asT = (x: 'a): t => Obj.magic(x)
  let num200Constructed = Num200({v: 1})
  let strOkConstructed = StrOk({msg: "ok"})

  let classify = (x: t): string =>
    switch x {
    | Num200(_) => "num200"
    | StrOk(_) => "strok"
    | OtherNum(r) => "num:" ++ Int.toString(r.tag)
    | OtherStr(r) => "str:" ++ r.tag
    }

  let classifyPartial = (x: t): string =>
    switch x {
    | OtherNum(r) => "num:" ++ Int.toString(r.tag)
    | OtherStr(r) => "str:" ++ r.tag
    | _ => "lit"
    }

  let num200 = asT(%raw(`({k: 200, v: 1})`))
  let strOk = asT(%raw(`({k: "ok", msg: "m"})`))
  let otherNum = asT(%raw(`({k: 404, x: 1})`))
  let otherStr = asT(%raw(`({k: "else", s: "s"})`))
}

module ManyNumLiteralsWithCatchAll = {
  @tag("k")
  type t =
    | @as(1) One({x: int})
    | @as(2) Two({x: int})
    | @as(3) Three({x: int})
    | @catch(int) Other({k: int, y: int})

  let asT = (x: 'a): t => Obj.magic(x)
  let oneConstructed = One({x: 1})
  let twoConstructed = Two({x: 2})
  let threeConstructed = Three({x: 3})

  let sum = (x: t): int =>
    switch x {
    | One(r) => r.x + 10
    | Two(r) => r.x + 20
    | Three(r) => r.x + 30
    | Other(r) => r.k + r.y
    }

  let sumPartial = (x: t): int =>
    switch x {
    | Other(r) => r.k + r.y
    | _ => 0
    }

  let one = asT(%raw(`({k: 1, x: 1})`))
  let two = asT(%raw(`({k: 2, x: 2})`))
  let three = asT(%raw(`({k: 3, x: 3})`))
  let other = asT(%raw(`({k: 42, y: 1})`))
}

module NullaryStringAndCatchAll = {
  @tag("kind")
  type t =
    | @as("ok") Ok
    | @catch(string) Other({kind: string, value: int})

  let asT = (x: 'a): t => Obj.magic(x)
  let okConstructed = Ok

  let classify = (x: t): int =>
    switch x {
    | Ok => 1
    | Other(_) => 2
    }

  let okResult = classify(asT(%raw(`"ok"`)))
  let otherOkResult = classify(asT(%raw(`({kind: "ok", value: 1})`)))
}

module NoExposedDiscriminantField = {
  @tag("kind")
  type t =
    | @as("ok") Ok({value: int})
    | @catch(string) Other({value: int})

  let asT = (x: 'a): t => Obj.magic(x)
  let okConstructed = Ok({value: 1})

  let classify = (x: t): int =>
    switch x {
    | Ok(r) => r.value
    | Other(r) => r.value + 10
    }

  let okResult = classify(asT(%raw(`({kind: "ok", value: 1})`)))
  let otherResult = classify(asT(%raw(`({kind: "else", value: 2})`)))
}

module NullaryNullAndCatchAll = {
  @tag("kind")
  type t =
    | @as(null) Nil
    | @catch(string) Other({kind: string, value: int})

  let asT = (x: 'a): t => Obj.magic(x)
  let nilConstructed = Nil

  let classify = (x: t): int =>
    switch x {
    | Nil => 10
    | Other(_) => 20
    }

  let nilResult = classify(asT(%raw(`null`)))
  let otherResult = classify(asT(%raw(`({kind: "else", value: 1})`)))
}
