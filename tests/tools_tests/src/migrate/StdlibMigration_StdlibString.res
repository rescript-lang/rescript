let s1 = "abcde"->String.sliceToEnd(~start=1)
let s2 = "abcde"->String.substringToEnd(~start=1)

let r0 =
  "vowels"->String.unsafeReplaceRegExpBy0(/a|e|i|o|u/g, (~match, ~offset as _, ~input as _) =>
    match
  )
let r1 =
  "Jony is 40"->String.unsafeReplaceRegExpBy1(/(Jony is )\d+/g, (
    ~match as _,
    ~group1,
    ~offset as _,
    ~input as _,
  ) => group1)

let r2 = "7 times 6"->String.unsafeReplaceRegExpBy2(/(\d+) times (\d+)/, (
  ~match as _,
  ~group1,
  ~group2,
  ~offset as _,
  ~input as _,
) =>
  switch (Int.fromString(group1), Int.fromString(group2)) {
  | (Some(x), Some(y)) => Int.toString(x * y)
  | _ => "???"
  }
)

let r3 =
  "abc"->String.unsafeReplaceRegExpBy3(/(a)(b)(c)/, (
    ~match as _,
    ~group1,
    ~group2,
    ~group3,
    ~offset as _,
    ~input as _,
  ) => group1 ++ group2 ++ group3)
