module Styles = {
  open Css
  let button = style(list{
    backgroundColor(hex("3aa563e0")),
    borderWidth(zero),
    borderRadius(px(4)),
    color(Colors.white),
    cursor(pointer),
    padding2(~v=px(10), ~h=px(14)),
    fontSize(px(14)),
    transition(~duration=200, "all"),
    hover(list{backgroundColor(Colors.green)}),
    disabled(list{opacity(0.5)}),
  })
  let buttonSmall = style(list{padding2(~v=px(6), ~h=px(10))})
}

@react.component
let make = (~onClick=?, ~small=false, ~children, ~disabled=?, ~className=?, ()) =>
  <button
    className={Cn.make(list{
      Styles.button,
      Cn.ifTrue(Styles.buttonSmall, small),
      Cn.unpack(className),
    })}
    ?disabled
    ?onClick>
    children
  </button>
