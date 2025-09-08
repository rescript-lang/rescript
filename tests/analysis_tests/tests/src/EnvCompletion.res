type things = One | Two
type things2 = Four | Five

let res: EnvCompletionOtherFile.someResult<things, string> = Okay(One)

let use = (): EnvCompletionOtherFile.response => {
  stuff: First,
  res: Failure(""),
}

// switch res { | }
//               ^com

// switch res { | Okay() }
//                     ^com

// switch res { | Failure() }
//                        ^com

// switch use() { | }
//                 ^com

// switch use() { | {} }
//                   ^com

// switch use() { | {stuff: } }
//                         ^com

// switch use() { | {stuff: Second() } }
//                                 ^com

// switch use() { | {stuff: Second({}) } }
//                                  ^com

// switch use() { | {res: } }
//                       ^com

// switch use() { | {res: Okay() } }
//                             ^com

// switch use() { | {res: Okay(Second()) } }
//                                    ^com

// switch use() { | {res: Okay(Second({})) } }
//                                     ^com

let res2: EnvCompletionOtherFile.someRecord<things2> = {
  name: "string",
  theThing: Four,
  theVariant: First,
}

// switch res2 { | }
//                ^com

// switch res2 { | {} }
//                  ^com

// switch res2 { | {theThing: } }
//                           ^com

// switch res2 { | {theVariant: } }
//                             ^com
