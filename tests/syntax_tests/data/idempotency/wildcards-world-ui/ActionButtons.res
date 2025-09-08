module Buy = {
  @react.component
  let make = (~chain, ~animal: TokenId.t, ~isExplorer: bool=false) => {
    let currentPriceWei = QlHooks.usePrice(~chain, animal)

    let goToBuy = RootProvider.useGoToBuy()
    let clearAndPush = RootProvider.useClearNonUrlStateAndPushRoute()

    let buttonText = switch currentPriceWei {
    | Price(_price) => "Buy"
    | Foreclosed(_) => "Adopt"
    | Loading => "loading"
    }

    <React.Fragment>
      <Rimble.Button
        onClick={_e => {
          clearAndPush(
            "#" ++
            (InputHelp.getPagePrefix(isExplorer) ++
            ("details/" ++ animal->TokenId.toString)),
          )
          goToBuy(animal)
        }}>
        {React.string(buttonText)}
      </Rimble.Button>
    </React.Fragment>
  }
}

module Auction = {
  @react.component
  let make = (~animal: TokenId.t, ~isExplorer: bool=false) => {
    let goToBuy = RootProvider.useGoToBuy()
    let clearAndPush = RootProvider.useClearNonUrlStateAndPushRoute()

    <React.Fragment>
      <Rimble.Button
        onClick={_e => {
          clearAndPush(
            "#" ++
            (InputHelp.getPagePrefix(isExplorer) ++
            ("details/" ++ animal->TokenId.toString)),
          )
          goToBuy(animal)
        }}>
        {React.string("Auction")}
      </Rimble.Button>
    </React.Fragment>
  }
}

module UpdateDeposit = {
  @react.component
  let make = () => {
    // TODO: if the token is foreclosed handle that logic... (say something like -- "add deposit quick! to keep your token")
    let goToDepositUpdate = RootProvider.useGoToDepositUpdate()

    <React.Fragment>
      <Rimble.Button onClick={_e => goToDepositUpdate()}> {"Deposit"->React.string} </Rimble.Button>
    </React.Fragment>
  }
}
module UpdatePrice = {
  @react.component
  let make = (~animal: TokenId.t) => {
    // TODO: if the token is foreclosed handle that logic... (say something like -- "add deposit quick! to keep your token")
    let goToPriceUpdate = RootProvider.useGoToPriceUpdate()

    <React.Fragment>
      <Rimble.Box p=1>
        <Rimble.Button onClick={_e => goToPriceUpdate(animal)}>
          {"Price"->React.string}
        </Rimble.Button>
      </Rimble.Box>
    </React.Fragment>
  }
}
