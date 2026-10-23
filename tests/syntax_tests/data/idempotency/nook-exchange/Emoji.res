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

  let resultRef = ref(text->RegExp.exec(emojiRegex))
  while resultRef.contents != None {
    let result = Belt.Option.getExn(resultRef.contents)
    let matches = RegExp.Result.matches(result)
    let emojiColons = Belt.Option.getExn(Nullable.toOption(matches[2]))
    let offset =
      RegExp.Result.index(result) + Belt.Option.getExn(Nullable.toOption(matches[1]))->String.length
    if iter.contents < offset {
      children
      ->Array.push(<span key={string_of_int(Array.length(children))}>
          {React.string(text->String.substring(~start=iter.contents, ~end=offset))}
        </span>)
      ->ignore
    }
    children
    ->Array.push(switch emojiColons {
      | ":nmt:" => <span className=Styles.nmt key={string_of_int(Array.length(children))} />
      | ":bell:" => <span className=Styles.bell key={string_of_int(Array.length(children))} />
      | _ =>
        <span className=Styles.emoji key={string_of_int(Array.length(children))}>
          <Component emoji=emojiColons size=16 />
        </span>
      })
    ->ignore

    resultRef := text->RegExp.exec(emojiRegex)
    iter := offset + String.length(emojiColons)
  }
  if iter.contents < String.length(text) {
    children
    ->Array.push(<span key={string_of_int(Array.length(children))}>
        {React.string(text->String.substring(~start=iter.contents))}
      </span>)
    ->ignore
  }
  React.array(children)
}
