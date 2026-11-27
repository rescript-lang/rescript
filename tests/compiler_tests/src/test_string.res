let f = x =>
  switch x {
  | "aaaabb" => 0
  | "bbbb" => 1
  | _ => /* 2 */ assert(false)
  }

let a = x => "hello" ++ ("world" ++ ("hello" ++ x))

let b = (y, x) => y ++ ("hello" ++ ("world" ++ ("hello" ++ x)))

let c = (x, y) => x ++ "hello" ++ ("hi" ++ ("u" ++ ("hi" ++ y)))

let v = String.length("xx")

let h = (s: string) => String.codePointAt(s, 0) === Some(97)
