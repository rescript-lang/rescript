module Styles = {
  open Css
  let root = style(list{
    backgroundColor(hex("ffffffc0")),
    boxSizing(borderBox),
    lineHeight(px(20)),
    maxWidth(px(512)),
    marginBottom(px(32)),
    marginLeft(auto),
    marginRight(auto),
    padding2(~v=px(16), ~h=px(24)),
    borderRadius(px(8)),
    selector(
      "& > p",
      list{marginTop(zero), marginBottom(px(8)), lastChild(list{marginBottom(zero)})},
    ),
    media("(max-width: 512px)", list{padding(px(16)), borderRadius(zero)}),
  })
}

@react.component
let make = (~children, ~className=?, ()) =>
  <div className={Cn.make(list{Styles.root, Cn.unpack(className)})}> children </div>
