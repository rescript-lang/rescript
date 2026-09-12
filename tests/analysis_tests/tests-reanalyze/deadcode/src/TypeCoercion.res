// Coercing a record to a narrower one reads the source labels through the
// target's: `name` is read by TypeCoercionPerson.greet, while `age` is dead.
type user = {name: string, email: string, age: int}

// Same file, same shape: `a` is read through `narrow`, `b` is dead.
type wide = {a: string, b: string}
type narrow = {a: string}

// Nothing reads `unreadTarget.u`, so the coercion leaves `unreadSource.u` dead.
type unreadSource = {u: string}
type unreadTarget = {u: string}

// Through an alias on either side: `sa` and `ta` are read, `sb` and `tb` are not.
type aliasedSource = {sa: string, sb: string}
type aliasedSourceName = aliasedSource
type aliasedNarrow = {sa: string}

type aliasedTarget = {ta: string, tb: string}
type aliasedTargetNarrow = {ta: string}
type aliasedTargetName = aliasedTargetNarrow

let readNarrow = (n: narrow) => Console.log(n.a)
let takesUnread = (_: unreadTarget) => ()
let readAliasedNarrow = (n: aliasedNarrow) => Console.log(n.sa)
let readAliasedTarget = (n: aliasedTargetName) => Console.log(n.ta)

let main = () => {
  let john = {name: "John", email: "john@example.com", age: 42}
  Console.log(john.email)
  TypeCoercionPerson.greet((john :> TypeCoercionPerson.t))
  readNarrow(({a: "x", b: "y"} :> narrow))
  let source: unreadSource = {u: "u"}
  takesUnread((source :> unreadTarget))
  let aliasedSource: aliasedSourceName = {sa: "x", sb: "y"}
  readAliasedNarrow((aliasedSource :> aliasedNarrow))
  readAliasedTarget(({ta: "x", tb: "y"}: aliasedTarget :> aliasedTargetName))
}

main()
