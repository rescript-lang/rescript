module Styles = {
  open Css
  let footer = style(list{
    display(flexBox),
    flexDirection(column),
    fontSize(px(12)),
    alignItems(center),
    textAlign(center),
    margin3(~top=px(96), ~bottom=px(48), ~h=zero),
  })
  let contents = style(list{
    color(Colors.gray),
    lineHeight(px(16)),
    maxWidth(px(512)),
    padding2(~v=zero, ~h=px(8)),
  })
  let disclaimer = style(list{marginTop(px(8))})
}

@react.component
let make = () => {
  let language = SettingsStore.useLanguage()
  <div className=Styles.footer>
    <div className=Styles.contents>
      <div>
        <a href="https://www.buymeacoffee.com/nookexchange" target="_blank">
          {React.string("Support us")}
        </a>
        {React.string(" | ")}
        <a href="https://discord.gg/9sh66CX" target="_blank"> {React.string("Discord")} </a>
        {React.string(" | ")}
        <a href="https://twitter.com/nookexchange" target="_blank"> {React.string("Twitter")} </a>
        {React.string(" | ")}
        <a
          href="https://docs.google.com/spreadsheets/d/13d_LAJPlxMa_DubPTuirkIV4DERBMXbrWQsmSh8ReK4/edit?usp=sharing"
          target="_blank">
          {React.string("Data source")}
        </a>
        {React.string(" | ")}
        <a href="https://tinyurl.com/acnh-translations" target="_blank">
          {React.string("Translations")}
        </a>
        {React.string(" | ")}
        <span>
          {React.string("Language: ")}
          <a
            href="#"
            onClick={e => {
              ReactEvent.Mouse.preventDefault(e)
              LanguageSelector.show()
            }}>
            {React.string(SettingsStore.languageToString(language))}
          </a>
        </span>
      </div>
      <div>
        <a href="/terms"> {React.string("Terms of Service")} </a>
        {React.string(" | ")}
        <a href="/privacy"> {React.string("Privacy Policy")} </a>
        {React.string(" | ")}
        <a href="mailto:hi@nook.exchange"> {React.string("Contact us")} </a>
        {React.string(" | Thanks for visiting!")}
      </div>
      <div className=Styles.disclaimer>
        {React.string(
          "Animal Crossing is a registered trademark of Nintendo. Nook Exchange in no way claims ownership of any intellectual property associated with Animal Crossing.",
        )}
      </div>
    </div>
  </div>
}
