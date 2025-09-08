@module("./UserProvider") @react.component
external make: (~children: React.element) => React.element = "UserInfoProvider"

open Globals

type threeBoxImageData = {
  @as("@type")
  imageType: string,
  contentUrl: dict<string>,
}
type threeBoxImage = array<threeBoxImageData>
type threeBoxTwitterVerification = {
  username: string,
  proof: string,
  verifiedBy: string,
}
type threeBoxProfile = {
  coverPhoto: option<threeBoxImage>,
  description: option<string>,
  image: option<threeBoxImage>,
  name: option<string>,
}
type threeBoxVerifications = {
  did: string,
  twitter: option<threeBoxTwitterVerification>,
}
type threeBoxUserInfo = {
  profile: option<threeBoxProfile>,
  wildcardsSpace: option<string>,
  verifications: option<threeBoxVerifications>,
}
type userVerification = {threeBox: threeBoxUserInfo}

type userInfo = {
  userInfo: dict<userVerification>,
  update: (string, bool) => unit,
}

@module("./UserProvider")
external useUserInfoContext: unit => userInfo = "useUserInfoContext"

type displayName =
  | TwitterHandle(string)
  | ThreeBoxName(string)
  | EthAddress(string)

let useDisplayName: Web3.ethAddress => displayName = ethAddress => {
  let userContext = useUserInfoContext()
  let ethAddressLower = Js.String.toLowerCase(ethAddress)

  let opt3box = \"<$>"(Js.Dict.get(userContext.userInfo, ethAddressLower), a => a.threeBox)

  let opt3boxName = \">>="(\">>="(opt3box, a => a.profile), a => a.name)

  let name =
    opt3boxName->Option.mapWithDefault(
      \"<$>"(\">>="(\">>="(opt3box, a => a.verifications), a => a.twitter), a =>
        a.username
      )->Option.mapWithDefault(EthAddress(ethAddress), a => TwitterHandle(a)),
      a => ThreeBoxName(a),
    )
  name
}

let use3BoxUserData: string => option<threeBoxUserInfo> = ethAddress => {
  let userContext = useUserInfoContext()
  let ethAddressLower = Js.String.toLowerCase(ethAddress)

  switch Js.Dict.get(userContext.userInfo, ethAddressLower) {
  | None => None
  | Some(userInfo) => Some(userInfo.threeBox)
  }
}

let useIsUserValidated: string => bool = ethAddress =>
  switch useDisplayName(ethAddress) {
  | TwitterHandle(_) => true
  | ThreeBoxName(_)
  | EthAddress(_) => false
  }

let displayNameToString = displayName =>
  switch displayName {
  | EthAddress(addr) => addr->Helper.elipsifyMiddle(8, 2)
  | TwitterHandle(handle) => handle
  | ThreeBoxName(name) => name
  }
