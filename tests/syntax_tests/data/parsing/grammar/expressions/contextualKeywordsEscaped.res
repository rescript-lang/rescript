// An escaped contextual keyword is always a name, also where the keyword
// would go.
\"open"
Belt.x
let f = () => {
  ignore()
  \"exception"
  Foo
}
\"external"
foo()
let f = x =>
  switch x {
  | \"exception" => 1
  | exception \"as" => 2
  | exception \"when" => 3
  }
let \"rec" = 1
type \"rec" = int
type t = \"private"
