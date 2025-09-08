open Globals

let infoModalStyle = {
  open Css
  style(list{padding(rem(3.)), borderRadius(px(5)), width(px(640)), maxWidth(#vw(100.))})
}

@react.component
let make = (~clickAction=() => ()) => {
  let connectWeb3 = RootProvider.useConnectWeb3()
  let deactivateWeb3 = RootProvider.useDeactivateWeb3()
  let networkIdOpt = RootProvider.useNetworkId()

  let connectedNetworkName = networkId =>
    switch networkId {
    | 1 => `MAINNET\\xa0`->React.string
    | 3 => `ROPSTEN\\xa0`->React.string
    | 4 => `RINKEBY\\xa0`->React.string
    | 5 => `GOERLI\\xa0`->React.string
    | 42 => `KOVAN\\xa0`->React.string
    | _ => `Unknown\\xa0`->React.string
    }

  let web3Button = switch networkIdOpt {
  | Some(networkId) =>
    <Rimble.Button
      mainColor="#72C7D7"
      onClick={_e => {
        clickAction()
        deactivateWeb3()
      }}>
      {connectedNetworkName(networkId)} <Rimble.Icon name="ExitToApp" size="16px" />
    </Rimble.Button>
  | None =>
    <Rimble.Button
      mainColor="#72C7D7"
      onClick={_e => {
        clickAction()
        ReasonReactRouter.push("#")
        connectWeb3(RootProviderTypes.NoAction)
      }}>
      {"Connect"->React.string}
    </Rimble.Button>
  }

  <div className=Styles.loginButton> web3Button </div>
}

module Modal = {
  @react.component
  let make = () => {
    let showLogin = RootProvider.useShowLogin()
    let closeLogin = RootProvider.useCloseWeb3Login()

    <Rimble.Modal isOpen=showLogin>
      <Rimble.Card className=infoModalStyle>
        <Rimble.Button.Text
          icononly=true
          icon="Close"
          color="moon-gray"
          position="absolute"
          top=0
          right=0
          m=3
          onClick={_ => closeLogin()}
        />
        <Rimble.Heading className=Styles.centerText> {"Login"->restr} </Rimble.Heading>
        <Login />
      </Rimble.Card>
    </Rimble.Modal>
  }
}
