module PersistConfig = {
  let key = "dismiss_discord_bot_notice"
  let value = ref(Dom.Storage.localStorage->Dom.Storage.getItem(key))
  let dismiss = () => {
    let nowString = Js.Date.now()->Js.Float.toString
    value := Some(nowString)
    Dom.Storage.localStorage->Dom.Storage.setItem(key, nowString)
  }
}

module Component = {
  module Styles = {
    open Css
    let root = style(list{
      position(fixed),
      right(px(32)),
      bottom(px(32)),
      backgroundColor(hex("f9fdf9")),
      padding(px(24)),
      borderRadius(px(16)),
      boxShadow(Shadow.box(~blur=px(32), hex("4a846080"))),
      maxWidth(px(360)),
      media("(max-width: 540px)", list{bottom(px(8)), right(px(8)), left(px(8)), padding(px(16))}),
    })
    let bodyText = style(list{lineHeight(px(18))})
    let buttonRow = style(list{
      display(flexBox),
      alignItems(center),
      marginTop(px(24)),
      justifyContent(flexEnd),
    })
    let registerButton = style(list{marginLeft(px(16))})
    let notNowLink = style(list{
      opacity(0.8),
      transition(~duration=200, "all"),
      textDecoration(none),
      hover(list{opacity(1.), textDecoration(underline)}),
    })
  }

  @react.component
  let make = (~onDismiss) => {
    React.useEffect0(() => {
      Analytics.Amplitude.logEvent(~eventName="Discord Bot Notice Shown")
      None
    })
    <div className=Styles.root>
      <div className=Styles.bodyText>
        {React.string("Use our ")}
        <a
          href="#"
          onClick={e => {
            ReactEvent.Mouse.preventDefault(e)
            Analytics.Amplitude.logEventWithProperties(
              ~eventName="Discord Bot Notice Accepted",
              ~eventProperties={"source": "link"},
            )
            open Webapi.Dom
            window
            ->Window.open_(~url="https://discord.gg/9sh66CX", ~name="", ~features=?None)
            ->ignore
          }}>
          {React.string("Discord bot")}
        </a>
        {React.string(" to find DIY swaps and bulk catalog trades with other Nook Exchange users!")}
      </div>
      <div className=Styles.buttonRow>
        <a
          href="#"
          onClick={e => {
            ReactEvent.Mouse.preventDefault(e)
            onDismiss()
            Analytics.Amplitude.logEvent(~eventName="Discord Bot Notice Dismissed")
          }}
          className=Styles.notNowLink>
          {React.string("Not now")}
        </a>
        <Button
          onClick={_ => {
            open Webapi.Dom
            window
            ->Window.open_(~url="https://discord.gg/9sh66CX", ~name="", ~features=?None)
            ->ignore
            onDismiss()
            Analytics.Amplitude.logEventWithProperties(
              ~eventName="Discord Bot Notice Accepted",
              ~eventProperties={"source": "button"},
            )
          }}
          className=Styles.registerButton>
          {React.string("Try Discord bot")}
        </Button>
      </div>
    </div>
  }
}

@react.component
let make = () => {
  let me = UserStore.useMe()
  let (dismissed, setDismissed) = React.useState(() => PersistConfig.value.contents !== None)
  let discordId = me->Belt.Option.map(me => me.discordId)
  switch (dismissed, discordId) {
  | (false, Some(None)) =>
    <Component
      onDismiss={() => {
        setDismissed(_ => true)
        PersistConfig.dismiss()
      }}
    />
  | _ => React.null
  }
}
