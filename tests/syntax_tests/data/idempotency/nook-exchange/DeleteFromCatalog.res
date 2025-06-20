module PersistConfig = {
  let key = "confirm_catalog_delete"
  let value = ref(Dom.Storage.localStorage->Dom.Storage.getItem(key))
  let confirm = () => {
    let nowString = Js.Date.now()->Js.Float.toString
    value := Some(nowString)
    Dom.Storage.localStorage->Dom.Storage.setItem(key, nowString)
  }
}

let confirm = (~onConfirm) =>
  if Belt.Option.isSome(PersistConfig.value.contents) {
    onConfirm()
  } else {
    Analytics.Amplitude.logEventWithProperties(
      ~eventName="Confirm Dialog Shown",
      ~eventProperties={"id": "deleteFromCatalog"},
    )
    ConfirmDialog.confirm(
      ~bodyText="Removing an item from your catalog will also remove it from your For Trade and Can Craft lists. Do you want to continue?",
      ~confirmLabel="Remove from catalog",
      ~cancelLabel="Not now",
      ~onConfirm=() => {
        PersistConfig.confirm()
        onConfirm()
        Analytics.Amplitude.logEventWithProperties(
          ~eventName="Confirm Dialog Confirmed",
          ~eventProperties={"id": "deleteFromCatalog"},
        )
      },
      (),
    )
  }
