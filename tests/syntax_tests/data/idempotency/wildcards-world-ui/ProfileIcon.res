open Globals

@react.component
let make = (~clickAction=() => (), ~isMobile=false) => {
  let currentUser = RootProvider.useCurrentUser()
  let networkIdOpt = RootProvider.useNetworkId()

  let displayName = UserProvider.useDisplayName(
    currentUser->Option.mapWithDefault("loading", a => a),
  )
  let displayNameStr = UserProvider.displayNameToString(displayName)

  let userAddressLowerCase = switch currentUser {
  | Some(currentUser) => currentUser->Js.String.toLowerCase //TODO - check with zuck this cant be a 3box name name
  | _ => CONSTANTS.nullEthAddress
  }

  let optThreeBoxData = UserProvider.use3BoxUserData(userAddressLowerCase)
  let optProfile = \">>="(optThreeBoxData, a => a.profile)
  let profileImage: string =
    \">>="(
      \"<$>"(\">>="(\">>="(optProfile, a => a.image), img => img->Array.get(0)), a => a.contentUrl),
      content => Js.Dict.get(content, "/"),
    )->Option.mapWithDefault(Blockie.makeBlockie(. userAddressLowerCase), hash =>
      "https://ipfs.infura.io/ipfs/" ++ hash
    )

  let message = switch (networkIdOpt, currentUser) {
  | (None, _) => "Connect to network"
  | (Some(_), None) => "Loading user"
  | (Some(_), Some(_)) => displayNameStr
  }

  let profileIcon =
    <img
      className={
        open Css
        style(list{
          borderRadius(#percent(50.)),
          width(#px(40)),
          height(#px(40)),
          marginLeft(#px(10)),
        })
      }
      src=profileImage
    />

  let clearAndPush = RootProvider.useClearNonUrlStateAndPushRoute()

  switch networkIdOpt {
  | None => React.null
  | Some(_) =>
    isMobile
      ? <div
          onClick={_ => {
            clickAction()
            clearAndPush("#user/" ++ userAddressLowerCase)
          }}
          className={
            open Css
            style(list{display(#flex), flexDirection(#row)})
          }>
          <div>
            <p> <strong> {"View Your Profile:"->restr} </strong> </p> <p> {message->restr} </p>
          </div>
          profileIcon
        </div>
      : <Rimble.Tooltip message placement="bottom">
          <div
            onClick={_ => {
              clickAction()
              clearAndPush("#user/" ++ userAddressLowerCase)
            }}>
            profileIcon
          </div>
        </Rimble.Tooltip>
  }
}
