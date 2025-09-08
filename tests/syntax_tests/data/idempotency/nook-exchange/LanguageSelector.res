module Modal = {
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
      backgroundColor(hex("6f8477d0")),
    })
    let root = style(list{
      backgroundColor(hex("ffffff")),
      padding2(~v=px(32), ~h=px(32)),
      borderRadius(px(4)),
      position(relative),
      maxWidth(px(448)),
      boxSizing(borderBox),
      width(pct(90.)),
      boxShadow(Shadow.box(~spread=px(12), rgba(0, 0, 0, 0.1))),
      overflow(auto),
      maxHeight(vh(100.)),
      media("(max-width: 540px)", list{paddingTop(px(24)), paddingBottom(px(24))}),
    })
    let body = style(list{maxWidth(px(320)), margin2(~v=zero, ~h=auto)})
    let label = style(list{marginBottom(px(12))})
    let row = style(list{marginBottom(px(6))})
    let languageLink = style(list{
      opacity(0.8),
      textDecoration(none),
      hover(list{textDecoration(underline)}),
    })
    let languageLinkSelected = style(list{opacity(1.), textDecoration(underline)})
  }

  @react.component
  let make = (~onClose) => {
    let selectedLanguage = SettingsStore.useLanguage()
    <div className=Styles.overlay>
      <div className=Styles.backdrop onClick={_ => onClose()} />
      <div className=Styles.root>
        <div className=Styles.label> {React.string("Select a language:")} </div>
        <div>
          {SettingsStore.languages
          ->Belt.Array.map(language =>
            <div className=Styles.row key={SettingsStore.languageToJs(language)}>
              <a
                href="#"
                onClick={e => {
                  ReactEvent.Mouse.preventDefault(e)
                  SettingsStore.setLanguage(~language)
                  onClose()
                }}
                className={Cn.make(list{
                  Styles.languageLink,
                  Cn.ifTrue(Styles.languageLinkSelected, selectedLanguage == language),
                })}>
                {React.string(SettingsStore.languageToString(language))}
              </a>
            </div>
          )
          ->React.array}
        </div>
        <Modal.CloseButton onClose />
      </div>
    </div>
  }
}

let show = () => {
  let modalKey = ref(None)
  modalKey :=
    Some(
      ReactAtmosphere.API.pushLayer(~render=_ =>
        <Modal
          onClose={() =>
            ReactAtmosphere.API.removeLayer(~key=Belt.Option.getExn(modalKey.contents))}
        />
      ),
    )
}
