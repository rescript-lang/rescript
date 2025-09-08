module PersistConfig = {
  let key = "confirm_wishlist_to_catalog"
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
      ~eventProperties={"id": "wishlistToCatalog"},
    )
    ConfirmDialog.confirm(
      ~bodyText="Adding an item to your catalog will remove it from your Wishlist. Do you want to continue?",
      ~confirmLabel="Move to catalog",
      ~cancelLabel="Not now",
      ~onConfirm=() => {
        PersistConfig.confirm()
        onConfirm()
        Analytics.Amplitude.logEventWithProperties(
          ~eventName="Confirm Dialog Confirmed",
          ~eventProperties={"id": "wishlistToCatalog"},
        )
      },
      (),
    )
  }
