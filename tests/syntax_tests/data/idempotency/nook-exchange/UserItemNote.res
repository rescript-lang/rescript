module Styles = {
  open Css
  let root = style(list{unsafe("alignSelf", "stretch")})
  let textarea = style(list{
    backgroundColor(rgba(0, 0, 0, 0.03)),
    border(px(1), solid, transparent),
    borderRadius(px(4)),
    padding(px(8)),
    height(px(36)),
    boxSizing(borderBox),
    outlineStyle(none),
    fontSize(px(14)),
    width(pct(100.)),
    transition(~duration=200, "all"),
    width(pct(100.)),
    placeholder(list{opacity(0.5)}),
    focus(list{backgroundColor(Colors.white), borderColor(rgba(0, 0, 0, 0.1))}),
  })
  let updateBar = style(list{display(flexBox), justifyContent(spaceBetween), alignItems(center)})
  let cancelLink = style(list{
    opacity(0.8),
    textDecoration(none),
    marginLeft(px(8)),
    hover(list{opacity(1.), textDecoration(underline)}),
  })
}

@react.component
let make = (~itemId, ~variation, ~userItem: User.item, ~className=?, ()) => {
  let (userItemNote, setUserItemNote) = React.useState(() => userItem.note)

  React.useEffect1(() => {
    setUserItemNote(_ => userItem.note)
    None
  }, [userItem.note])

  <div className={Cn.make(list{Styles.root, Cn.unpack(className)})}>
    <textarea
      value=userItemNote
      placeholder="Add a note"
      className=Styles.textarea
      onChange={e => {
        let value = ReactEvent.Form.target(e)["value"]
        setUserItemNote(_ => value)
      }}
    />
    {userItem.note != userItemNote
      ? <div className=Styles.updateBar>
          <Button
            small=true
            onClick={_ => UserStore.setItemNote(~itemId, ~variation, ~note=userItemNote)}>
            {React.string("Save")}
          </Button>
          <a
            href="#"
            onClick={e => {
              setUserItemNote(_ => userItem.note)
              ReactEvent.Mouse.preventDefault(e)
            }}
            className=Styles.cancelLink>
            {React.string("Cancel")}
          </a>
        </div>
      : React.null}
  </div>
}
