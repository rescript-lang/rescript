module ErrorPopup = {
  module Styles = {
    open Css
    let overlay = style(list{
      position(fixed),
      top(zero),
      bottom(zero),
      left(zero),
      right(zero),
      display(flexBox),
      alignItems(center),
      justifyContent(center),
    })
    let backdrop = style(list{
      position(absolute),
      top(zero),
      bottom(zero),
      left(zero),
      right(zero),
      backgroundColor(hex("ffffffc0")),
    })
    let root = style(list{
      padding2(~v=px(32), ~h=px(32)),
      backgroundColor(Colors.red),
      color(Colors.white),
      borderRadius(px(4)),
      position(relative),
      maxWidth(px(448)),
      width(pct(90.)),
      fontSize(px(16)),
      whiteSpace(#preLine),
      boxShadow(Shadow.box(~spread=px(12), rgba(0, 0, 0, 0.1))),
    })
  }

  @react.component
  let make = (~message) =>
    <div className=Styles.overlay>
      <div className=Styles.backdrop /> <div className=Styles.root> {React.string(message)} </div>
    </div>
}

let showPopup = (~message) =>
  ReactAtmosphere.API.pushLayer(~render=_ => <ErrorPopup message />)->ignore
