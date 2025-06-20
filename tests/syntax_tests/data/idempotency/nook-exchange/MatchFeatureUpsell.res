module PersistConfig = {
  let key = "dismiss_match_list_notice"
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
      media("(max-width: 540px)", list{bottom(px(8)), right(px(8)), left(px(8)), padding(px(16))}),
    })
    let buttonRow = style(list{
      display(flexBox),
      alignItems(center),
      marginTop(px(16)),
      justifyContent(flexEnd),
    })
    let registerButton = style(list{marginLeft(px(16))})
    let notNowLink = style(list{
      opacity(0.8),
      transition(~duration=200, "all"),
      hover(list{opacity(1.)}),
    })
  }

  @react.component
  let make = (~username, ~showLogin, ~onDismiss) => {
    React.useEffect0(() => {
      Analytics.Amplitude.logEventWithProperties(
        ~eventName="Match List Notice Shown",
        ~eventProperties={"username": username},
      )
      None
    })
    <div className=Styles.root>
      <div>
        {React.string(
          "Make your own list and see what matches you have with " ++ (username ++ "!"),
        )}
      </div>
      <div className=Styles.buttonRow>
        <a
          href="#"
          onClick={e => {
            ReactEvent.Mouse.preventDefault(e)
            onDismiss()
            Analytics.Amplitude.logEventWithProperties(
              ~eventName="Match List Notice Dismissed",
              ~eventProperties={"username": username},
            )
          }}
          className=Styles.notNowLink>
          {React.string("Not now")}
        </a>
        <Button
          onClick={_ => {
            showLogin()
            onDismiss()
            Analytics.Amplitude.logEventWithProperties(
              ~eventName="Match List Notice Accepted",
              ~eventProperties={"username": username},
            )
          }}
          className=Styles.registerButton>
          {React.string("Sign me up!")}
        </Button>
      </div>
    </div>
  }
}

module WrappedComponent = {
  @react.component
  let make = (~username, ~showLogin, ~onDismiss) => {
    let bucketId = Experiment.getBucketIdForExperiment(
      ~experimentId=Experiment.ExperimentIds.matchListNotice,
    )
    React.useEffect0(() => {
      Experiment.trigger(~experimentId=Experiment.ExperimentIds.matchListNotice, ~bucketId)
      None
    })
    if bucketId === "1" {
      <Component username showLogin onDismiss />
    } else {
      React.null
    }
  }
}

@react.component
let make = (~username, ~showLogin) => {
  let userState = UserStore.useStore()
  let (dismissed, setDismissed) = React.useState(() => PersistConfig.value.contents !== None)
  switch (dismissed, userState) {
  | (false, NotLoggedIn) =>
    <WrappedComponent
      username
      showLogin
      onDismiss={() => {
        setDismissed(_ => true)
        PersistConfig.dismiss()
      }}
    />
  | _ => React.null
  }
}
