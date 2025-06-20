module Component = {
  @module("emoji-mart") @react.component
  external make: (~emoji: string, ~size: int) => React.element = "Emoji"
}

@module("./assets/nmt.png") external nmtPng: string = "default"
@module("./assets/bell.png") external bellPng: string = "default"

module Styles = {
  open Css
  let nmt = style(list{
    backgroundImage(url(nmtPng)),
    width(px(16)),
    height(px(16)),
    display(inlineBlock),
    backgroundSize(cover),
    verticalAlign(#bottom),
    position(relative),
    top(px(-2)),
  })
  let bell = style(list{
    backgroundImage(url(bellPng)),
    width(px(16)),
    height(px(16)),
    display(inlineBlock),
    backgroundSize(cover),
    verticalAlign(#bottom),
    position(relative),
    top(px(-2)),
  })
  let emoji = style(list{verticalAlign(#bottom), position(relative), top(px(-2))})
}

let emojiRegex = /(^|\s)(\:[a-zA-Z0-9-_+]+\:)/g

let parseText = (text: string): React.element => {
  let children = []
  let iter = ref(0)

  let resultRef = ref(text->Js.Re.exec_(emojiRegex))
  while resultRef.contents != None {
    let result = Belt.Option.getExn(resultRef.contents)
    let matches = Js.Re.captures(result)
    let emojiColons = Belt.Option.getExn(Js.Nullable.toOption(matches[2]))
    let offset =
      Js.Re.index(result) + Belt.Option.getExn(Js.Nullable.toOption(matches[1]))->Js.String.length
    if iter.contents < offset {
      children
      ->Js.Array.push(
        <span key={string_of_int(Js.Array.length(children))}>
          {React.string(text->Js.String.substring(~from=iter.contents, ~to_=offset))}
        </span>,
      )
      ->ignore
    }
    children
    ->Js.Array.push(
      switch emojiColons {
      | ":nmt:" => <span className=Styles.nmt key={string_of_int(Js.Array.length(children))} />
      | ":bell:" => <span className=Styles.bell key={string_of_int(Js.Array.length(children))} />
      | _ =>
        <span className=Styles.emoji key={string_of_int(Js.Array.length(children))}>
          <Component emoji=emojiColons size=16 />
        </span>
      },
    )
    ->ignore

    resultRef := text->Js.Re.exec_(emojiRegex)
    iter := offset + Js.String.length(emojiColons)
  }
  if iter.contents < Js.String.length(text) {
    children
    ->Js.Array.push(
      <span key={string_of_int(Js.Array.length(children))}>
        {React.string(text->Js.String.substringToEnd(~from=iter.contents))}
      </span>,
    )
    ->ignore
  }
  React.array(children)
}
