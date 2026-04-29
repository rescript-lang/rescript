module AliasGreeting = ShowFixture.Nested

type itemAlias = ShowFixture.item

let len = String.length

let run = () => {
  ignore(AliasGreeting.makeGreeting("abc"))
  let _item: itemAlias = {label: "abc"}
  len("abc")
}
