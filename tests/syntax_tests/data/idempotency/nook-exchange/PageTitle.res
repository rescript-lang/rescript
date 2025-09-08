module Styles = {
  open Css
  let pageTitle = style(list{fontSize(px(32)), textAlign(center), marginBottom(px(16))})
}

@react.component
let make = (~title) => <div className=Styles.pageTitle> {React.string(title)} </div>
