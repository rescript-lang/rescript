open RootProviderTypes

type web3reactContext = {
  active: bool,
  activate: (Web3Connectors.injectedType, unit => unit, bool) => Promise.promise<unit>,
  account: option<Web3.ethAddress>,
  library: option<Web3.web3Library>,
  chainId: option<int>,
  deactivate: unit => unit,
}
@module("@web3-react/core")
external useWeb3React: unit => web3reactContext = "useWeb3React"
@module("@web3-react/core")
external useWeb3ReactId: string => web3reactContext = "useWeb3React"

module Web3ReactProvider = {
  @module("@web3-react/core") @react.component
  external make: (
    ~getLibrary: Web3.rawProvider => Web3.web3Library,
    ~children: React.element,
  ) => React.element = "Web3ReactProvider"
}

@module("ethers") @scope("providers") @new
external createWeb3Provider: Web3.rawProvider => Web3.web3Library = "Web3Provider"

let getLibrary = provider => {
  let library = createWeb3Provider(provider)

  let setPollingInterval: Web3.web3Library => Web3.web3Library = %raw(
    "lib => {lib.pollingInterval = 8000; return lib; }"
  )
  setPollingInterval(library)
}

let initialState = {
  nonUrlState: NoExtraState,
  ethState: Disconnected,
  config: {
    stewardContractAddress: None,
    stewardAbi: None,
  },
}

let rec reducer = (prevState, action) =>
  switch action {
  | ClearNonUrlState => {...prevState, nonUrlState: NoExtraState}
  | LoadAddress(address, optBalance) =>
    let newState = {...prevState, ethState: Connected(address, optBalance)}
    switch prevState.nonUrlState {
    | LoginScreen(followOnAction) => reducer(newState, followOnAction)
    | UserVerificationScreen
    | UpdateDepositScreen
    | UpdatePriceScreen(_)
    | BuyScreen(_)
    | AuctionScreen(_)
    | NoExtraState => newState
    }
  | GoToWeb3Connect(action) =>
    switch prevState.ethState {
    | Connected(_, _) => reducer(prevState, action)
    | Disconnected => {...prevState, nonUrlState: LoginScreen(action)}
    }
  | GoToBuy(animal) =>
    switch prevState.ethState {
    | Connected(_, _) => {...prevState, nonUrlState: BuyScreen(animal)}
    | Disconnected => {...prevState, nonUrlState: LoginScreen(action)}
    }
  | GoToAuction(animal) =>
    switch prevState.ethState {
    | Connected(_, _) => {...prevState, nonUrlState: AuctionScreen(animal)}
    | Disconnected => {...prevState, nonUrlState: LoginScreen(action)}
    }
  | GoToDepositUpdate =>
    switch prevState.ethState {
    | Connected(_, _) => {...prevState, nonUrlState: UpdateDepositScreen}
    | Disconnected => {...prevState, nonUrlState: LoginScreen(action)}
    }
  | GoToPriceUpdate(animal) =>
    switch prevState.ethState {
    | Connected(_, _) => {
        ...prevState,
        nonUrlState: UpdatePriceScreen(animal),
      }
    | Disconnected => {...prevState, nonUrlState: LoginScreen(action)}
    }
  | GoToUserVerification =>
    switch prevState.ethState {
    | Connected(_, _) => {...prevState, nonUrlState: UserVerificationScreen}
    | Disconnected => {...prevState, nonUrlState: LoginScreen(action)}
    }
  | Logout => {
      ...prevState,
      ethState: Disconnected,
      nonUrlState: NoExtraState,
    }
  | NoAction => {...prevState, nonUrlState: NoExtraState}
  // | _ => prevState
  }
module RootContext = {
  let context = React.createContext((initialState, _ => ()))
  // Create a provider component
  let make = React.Context.provider(context)
}

module RootWithWeb3 = {
  @react.component
  let make = (
    ~children,
    ~stewardContractAddress: option<Web3.ethAddress>,
    ~stewardAbi: option<Web3.abi>,
  ) => {
    let (rootState, dispatch) = React.useReducer(
      reducer,
      {
        ...initialState,
        config: {
          stewardContractAddress: stewardContractAddress,
          stewardAbi: stewardAbi,
        },
      },
    )
    let context = useWeb3React()

    // This prevents repeated tries at logging in (or re-login after logout)
    let (triedLoginAlready, setTriedLoginAlready) = React.useState(() => false)
    React.useEffect5(() => {
      Web3Connectors.injected.isAuthorized()->Promise.get(authorised =>
        if authorised && !triedLoginAlready {
          ignore(
            context.activate(Web3Connectors.injected, () => (), true)->Promise.Js.catch(_ => {
              setTriedLoginAlready(_ => true)
              Promise.resolved()
            }),
          )
          ()
        } else {
          setTriedLoginAlready(_ => true)
        }
      )
      switch context.chainId {
      | None => dispatch(Logout)
      | _ => ()
      }
      None
    }, (context.activate, context.chainId, dispatch, setTriedLoginAlready, triedLoginAlready))

    // React.useEffect3(
    //   () => {
    //     let maticChainId =
    //       switch (context.chainId) {
    //       | Some(4)
    //       | Some(5) => 80001
    //       | _ => 137
    //       };
    //     contextMatic.activate(
    //       // Web3Connectors.injected,
    //       Web3Connectors.sideChainNetwork(maticChainId),
    //       () => (),
    //       true,
    //     )
    //     ->Promise.Js.catch(e => {
    //         Js.log("ERROR ACTIVATING MATIC CONNECTION");
    //         Js.log(e);
    //         Promise.resolved();
    //       })
    //     ->ignore;
    //     None;
    //   },
    //   // intentionally only running on mount (make sure it's only mounted once :))
    //   (contextMatic.activate, context.chainId, contextMatic.chainId),
    // );

    //// This will never fire when metamask logs out unfortunately https://stackoverflow.com/a/59215775/3103033
    // React.useEffect1(
    //   () => {
    //     if (context.active) {
    //       ();
    //     } else {
    //       dispatch(Logout);
    //     };
    //     None;
    //   },
    //   // run this if the status of "active" ever changes
    //   [|rootState.ethState|],
    // );

    // if the connection worked, wait until we get confirmation of that to flip the flag
    React.useEffect3(() => {
      !triedLoginAlready && context.active ? setTriedLoginAlready(_ => true) : ()

      None
    }, (triedLoginAlready, context.active, setTriedLoginAlready))

    React.useEffect4(() =>
      switch (context.library, context.account) {
      | (Some(library), Some(account)) =>
        library.getBalance(. account)
        ->Promise.Js.catch(_ => Promise.resolved(None))
        ->Promise.get(newBalance =>
          dispatch(
            LoadAddress(
              account,
              newBalance->Belt.Option.flatMap(balance => Eth.make(balance.toString(.))),
            ),
          )
        )

        None
      | _ => None
      }
    , (context.library, context.account, context.chainId, dispatch))

    <RootContext value=(rootState, dispatch)> children </RootContext>
  }
}
let useRootContext: unit => RootProviderTypes.state = () => {
  let (state, _) = React.useContext(RootContext.context)
  state
}
let useStewardContractAddress: unit => option<Web3.ethAddress> = () => {
  let (state, _) = React.useContext(RootContext.context)
  state.config.stewardContractAddress
}

let useStewardAbi: unit => option<Web3.abi> = () => {
  let (state, _) = React.useContext(RootContext.context)
  state.config.stewardAbi
}

let useCurrentUser: unit => option<Web3.ethAddress> = () => {
  let (state, _) = React.useContext(RootContext.context)
  switch state.ethState {
  | Connected(address, _balance) => Some(address)
  | Disconnected => None
  }
}

let useIsAddressCurrentUser: Web3.ethAddress => bool = address => {
  let currentUser = useCurrentUser()
  switch currentUser {
  | Some(currentUserAddress) =>
    address->Js.String.toLowerCase == currentUserAddress->Js.String.toLowerCase
  | None => false
  }
}

let useIsProviderSelected: unit => bool = () => {
  let (state, _) = React.useContext(RootContext.context)
  switch state.ethState {
  | Connected(_address, _balance) => true
  | Disconnected => false
  }
}
let useEthBalance: unit => option<Eth.t> = () => {
  let (state, _) = React.useContext(RootContext.context)
  switch state.ethState {
  | Connected(_address, balance) => balance
  | Disconnected => None
  }
}
let useNonUrlState: unit => nonUrlState = () => {
  let (state, _) = React.useContext(RootContext.context)
  state.nonUrlState
}
let useShowLogin: unit => bool = () => {
  let nonUrlRouting = useNonUrlState()
  switch nonUrlRouting {
  | LoginScreen(_) => true
  | UserVerificationScreen
  | UpdateDepositScreen
  | UpdatePriceScreen(_)
  | BuyScreen(_)
  | AuctionScreen(_)
  | NoExtraState => false
  }
}
let useNetworkId: unit => option<int> = () => {
  let context = useWeb3React()

  context.chainId
}
let useEtherscanUrl: unit => string = () => {
  let networkId = useNetworkId()

  switch networkId {
  | Some(5) => "goerli.etherscan.io"
  | Some(4) => "rinkeby.etherscan.io"
  | _ => "etherscan.io"
  }
}
let useSidechainEtherscanUrl: unit => string = () => {
  let networkId = useNetworkId()

  switch networkId {
  | Some(5) => "mumbai-explorer.matic.today"
  | Some(4) => "goerli.etherscan.io"
  | _ => "explorer.matic.network"
  }
}
let useDeactivateWeb3: (unit, unit) => unit = () => {
  let context = useWeb3React()

  context.deactivate
}
let useWeb3: unit => option<Web3.web3Library> = () => {
  let context = useWeb3React()

  context.library
}

// TODO:: refactor `useGoToBuy`, `useGoToDepositUpdate`, `useGoToDepositUpdate` to come from a single function (basically do the same thing)
let useGoToBuy: (unit, TokenId.t) => unit = () => {
  let (_, dispatch) = React.useContext(RootContext.context)
  animal => dispatch(GoToBuy(animal))
}
let useGoToAuction: (unit, TokenId.t) => unit = () => {
  let (_, dispatch) = React.useContext(RootContext.context)
  animal => dispatch(GoToAuction(animal))
}
let useGoToDepositUpdate: (unit, unit) => unit = () => {
  let (_, dispatch) = React.useContext(RootContext.context)
  () => dispatch(GoToDepositUpdate)
}
let useGoToPriceUpdate: (unit, TokenId.t) => unit = () => {
  let (_, dispatch) = React.useContext(RootContext.context)
  animal => dispatch(GoToPriceUpdate(animal))
}
let useVerifyUser: (unit, unit) => unit = () => {
  let (_, dispatch) = React.useContext(RootContext.context)
  () => dispatch(GoToUserVerification)
}
let useClearNonUrlState: (unit, unit) => unit = () => {
  let (_, dispatch) = React.useContext(RootContext.context)
  () => dispatch(ClearNonUrlState)
}
let useConnectWeb3: (unit, RootProviderTypes.rootActions) => unit = () => {
  let (_, dispatch) = React.useContext(RootContext.context)
  action => dispatch(GoToWeb3Connect(action))
}
let useCloseWeb3Login: (unit, unit) => unit = () => {
  let (_, dispatch) = React.useContext(RootContext.context)

  () => dispatch(ClearNonUrlState)
}

let useClearNonUrlStateAndPushRoute: (unit, string) => unit = () => {
  let clearNonUrlState = useClearNonUrlState()
  url => {
    clearNonUrlState()
    ReasonReactRouter.push(url)
  }
}

type connection =
  | Standby
  | Connected
  | Connecting
  | ErrorConnecting

let useActivateConnector: unit => (connection, Web3Connectors.injectedType => unit) = () => {
  let context = useWeb3React()
  let (connectionStatus, setConnectionStatus) = React.useState(() => Standby)
  (
    connectionStatus,
    provider => {
      context.activate(provider, () => (), true)
      ->Promise.Js.catch(error => {
        Js.log("Error connecting to network:")
        Js.log(error)
        setConnectionStatus(_ => ErrorConnecting)
        Promise.resolved()
      })
      ->Promise.get(() => setConnectionStatus(_ => Connected))
      setConnectionStatus(_ => Connecting)
    },
  )
}

@react.component
let make = (
  ~children,
  ~stewardContractAddress: option<Web3.ethAddress>,
  ~stewardAbi: option<Web3.abi>,
) =>
  <Web3ReactProvider getLibrary>
    <RootWithWeb3 stewardContractAddress stewardAbi>
      <UserProvider> <ThemeProvider> children </ThemeProvider> </UserProvider>
    </RootWithWeb3>
  </Web3ReactProvider>

// Used to create matic provider
// </Web3Connectors.Custom>
// <Web3Connectors.Custom id="matic" getLibrary>
