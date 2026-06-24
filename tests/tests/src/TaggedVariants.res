@tag("kind")
type response =
  | @as(202) Ok202({code: int})
  | @as(200) Ok200({code: int})
  | @as(int) Other({@as("kind") kind: int, body: string})

let test = (x: response) =>
  switch x {
  | Ok202(r) => r.code + 1
  | Other(r) => r.kind + 2
  | Ok200(r) => r.code + 3
  }

/* Mixed number + string catch-alls */
module NumAndStrCatchAll = {
  @tag("k")
  type t =
    | @as(200) Num200({v: int})
    | @as("ok") StrOk({msg: string})
    | @as(int) OtherNum({@as("k") k: int, x: int})
    | @as(string) OtherStr({@as("k") k: string, s: string})

  let classify = (x: t): string =>
    switch x {
    | Num200(_) => "num200"
    | StrOk(_) => "strok"
    | OtherNum(r) => "num:" ++ string_of_int(r.k)
    | OtherStr(r) => "str:" ++ r.k
    }

  /* Intentionally not enumerating literals here. We want to ensure
   literals still win before the primitive catch-alls. */
  let classifyPartial = (x: t): string =>
    switch x {
    | OtherNum(r) => "num:" ++ string_of_int(r.k)
    | OtherStr(r) => "str:" ++ r.k
    | _ => "lit"
    }

  let a = Num200({v: 1})
  let b = StrOk({msg: "m"})
  let c = OtherNum({k: 404, x: 1})
  let d = OtherStr({k: "else", s: "s"})
}

/* Number + string literals and both catch-alls */
module NumAndStrLitsAndCatchAlls = {
  @tag("k")
  type t =
    | @as(201) Num201({v: int})
    | @as("warn") Warn({level: int})
    | @as(int) OtherNum({@as("k") k: int, payload: int})
    | @as(string) OtherStr({@as("k") k: string, payload: string})

  let route = (x: t): string =>
    switch x {
    | Num201(_) => "n201"
    | Warn(_) => "warn"
    | OtherNum(r) => "n:" ++ string_of_int(r.k)
    | OtherStr(r) => "s:" ++ r.k
    }

  /* Only match catch-alls; literals should still not be misrouted */
  let routePartial = (x: t): string =>
    switch x {
    | OtherNum(r) => "n:" ++ string_of_int(r.k)
    | OtherStr(r) => "s:" ++ r.k
    | _ => "lit"
    }

  let n = Num201({v: 1})
  let w = Warn({level: 2})
  let on = OtherNum({k: 451, payload: 0})
  let os = OtherStr({k: "other", payload: ""})
}

/* Three+ same-kind literals to exercise switch(x.k) shape */
module ManyNumLiteralsWithCatchAll = {
  @tag("k")
  type t =
    | @as(1) One({x: int})
    | @as(2) Two({x: int})
    | @as(3) Three({x: int})
    | @as(int) Other({@as("k") k: int, y: int})

  let sum = (x: t): int =>
    switch x {
    | One(r) => r.x + 10
    | Two(r) => r.x + 20
    | Three(r) => r.x + 30
    | Other(r) => r.k + r.y
    }

  let a = One({x: 1})
  let b = Two({x: 2})
  let c = Three({x: 3})
  let d = Other({k: 42, y: 1})
}
