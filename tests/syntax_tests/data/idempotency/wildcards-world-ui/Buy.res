open Globals
open Accounting

let calcPricePerSecond = (price, numerator, denominator) => {
  let priceBn = BN.new_(price)
  let numeratorBn = BN.new_(numerator)
  let denominatorBn = BN.new_(denominator)
  let fullYearSeconds = BN.new_("31536000")

  priceBn->BN.mul(numeratorBn)->BN.div(denominatorBn)->BN.div(fullYearSeconds)
}

// TODO: Could cached and stored so that all values don't need to be culculated each time!
// this calculates pricePerSecondEach time.
let calculateDepositDuration = (deposit, price, numerator, denominator) => {
  let depositBn = BN.new_(deposit)
  let pricePerSecond = calcPricePerSecond(price, numerator, denominator)

  depositBn
  ->BN.div(
    if pricePerSecond->BN.gt(BN.new_("0")) {
      pricePerSecond
    } else {
      BN.new_("1")
    },
  )
  ->BN.toString
  ->Int.fromString
  ->defaultZeroI
  // Check, 9007199254740992 is the largest integer available to javascript.
}

let calcRequiredDepositForTime = (time, price, numerator, denominator) => {
  let timeBn = BN.new_(string_of_int(time))
  let pricePerSecond = calcPricePerSecond(price, numerator, denominator)

  let requiredDeposit = timeBn->BN.mul(pricePerSecond)->BN.toString

  requiredDeposit->Web3Utils.fromWeiToEth
}

module Buy = {
  @react.component
  let make = (~chain, ~tokenId: TokenId.t, ~availableBalance: option<Eth.t>=?) => {
    let web3Context = RootProvider.useWeb3React()
    let (buyFunc, txBuyState) = ContractActions.useBuy(
      ~chain,
      tokenId,
      web3Context.library,
      web3Context.account,
      web3Context.chainId->Option.getWithDefault(1),
    )
    let (buyFuncAuction, txBuyAuctionState) = ContractActions.useBuyAuction(
      ~chain,
      tokenId,
      web3Context.library,
      web3Context.account,
      web3Context.chainId->Option.getWithDefault(1),
    )

    let userBalance = Belt.Option.mapWithDefault(RootProvider.useEthBalance(), BN.new_("0"), a => a)

    let (numerator, denominator, ratio, _ratioInverse) = QlHooks.usePledgeRateDetailed(
      ~chain,
      tokenId,
    )
    let priceStatus: QlHooks.animalPrice = QlHooks.usePrice(~chain, tokenId)
    let isOnAuction = Animal.useIsOnAuction(~chain, tokenId)
    let launchTimeOpt = QlHooks.useLaunchTimeBN(~chain, tokenId)
    let currentPriceWei = Animal.useAuctionPriceWei(
      ~chain,
      tokenId,
      launchTimeOpt->Option.getWithDefault(BN.new_("5000")),
    )

    let currentPriceWei = isOnAuction
      ? currentPriceWei->Option.getWithDefault(BN.new_("0"))
      : switch priceStatus {
        | Price(price) => price
        | Loading
        | Foreclosed(_) =>
          BN.new_("0")
        }

    let tokenIdName = "token#" ++ tokenId->TokenId.toString

    let paymentTokenBalance =
      availableBalance
      ->Option.getWithDefault(userBalance)
      ->Web3Utils.fromWeiBNToEthPrecision(~digits=4)

    let maxAvailableDepositBN =
      availableBalance->Option.getWithDefault(
        userBalance->BN.sub(BN.new_("3000000000000000"))->BN.sub(currentPriceWei), // 0.003 eth as gas
      )
    let maxAvailableDeposit = maxAvailableDepositBN->BN.toString->Web3Utils.fromWeiToEth

    let isAbleToBuy = maxAvailableDepositBN->BN.gt(BN.new_("0"))

    let currentPriceEth = Web3Utils.fromWeiBNToEth(currentPriceWei)
    let currentPriceFloat = Float.fromString(currentPriceEth)->defaultZeroF
    let currentPriceFloatWithMinimum = Js.Math.max_float(currentPriceFloat, 0.005)
    let defaultPriceValue = toFixedWithPrecisionNoTrailingZeros(
      currentPriceFloatWithMinimum *. 1.5,
      ~digits=2,
    )
    let defaultMonthlyPatronage = toFixedWithPrecisionNoTrailingZeros(
      currentPriceFloatWithMinimum *. 1.5 *. ratio,
      ~digits=3,
    )
    let defaultPriceWei = defaultPriceValue->Web3Utils.toWeiFromEth
    let depositForAYear = calcRequiredDepositForTime(
      31536000,
      defaultPriceWei,
      numerator,
      denominator,
    )
    let (
      defaultDepositTime,
      defaultDeposit,
    ) = // TODO: these 'float_of_string' s can throw errors, rather use the Belt library.
    if depositForAYear->float_of_string < maxAvailableDeposit->float_of_string {
      (31536000, depositForAYear)
    } else {
      (
        calculateDepositDuration(
          maxAvailableDeposit->Web3Utils.toWeiFromEth,
          defaultPriceWei,
          numerator,
          denominator,
        ),
        Js.Math.max_float(0., maxAvailableDeposit->float_of_string)->Js.Float.toString,
      )
    }

    let (newPrice, setInitialPrice) = React.useState(() => defaultPriceValue)
    let (patronage, setPatronage) = React.useState(() => defaultMonthlyPatronage)

    let (deposit, setInitialDeposit) = React.useState(() => defaultDeposit)
    let (depositTimeInSeconds, setDepositTimeInSeconds) = React.useState(() => defaultDepositTime)

    let onSubmitBuy = () => {
      let amountToSend = currentPriceWei->BN.add(BN.new_(Web3Utils.toWei(deposit, "ether")))
      switch priceStatus {
      | Foreclosed(_)
      | Loading =>
        buyFuncAuction(
          newPrice,
          "150000",
          amountToSend
          // Add 0.001 ETH as a buffer...
          ->BN.add(BN.new_("1000000000000000"))
          ->BN.toString,
        )
      | Price(price) =>
        if price->BN.gt(BN.new_("0")) {
          buyFunc(newPrice, currentPriceWei->BN.toString, "150000", amountToSend->BN.toString)
        } else {
          buyFuncAuction(
            newPrice,
            "150000",
            amountToSend
            // Add 0.001 ETH as a buffer...
            ->BN.add(BN.new_("1000000000000000"))
            ->BN.toString,
          )
        }
      }
    }

    let setNewPrice = value => {
      let (value, didUpdate) = InputHelp.onlyUpdateValueIfPositiveFloat(
        newPrice,
        setInitialPrice,
        value,
      )
      if didUpdate {
        let patronage = Js.Float.toString(Float.fromString(value)->defaultZeroF *. ratio)
        setPatronage(_ => patronage)
        let timeInSeconds = calculateDepositDuration(
          deposit->Web3Utils.toWeiFromEth,
          value->Web3Utils.toWeiFromEth,
          numerator,
          denominator,
        )
        setDepositTimeInSeconds(_ => timeInSeconds)
      } else {
        ()
      }
    }

    // let updatePatronage = value => {
    //   let (value, didUpdate) =
    //     InputHelp.onlyUpdateValueIfPositiveFloat(
    //       patronage,
    //       setPatronage,
    //       value,
    //     );
    //   if (didUpdate) {
    //     let price =
    //       Js.Float.toString(
    //         Float.fromString(value)->defaultZeroF *. ratioInverse,
    //       );
    //     setInitialPrice(_ => price);

    //     let timeInSeconds =
    //       calculateDepositDuration(
    //         deposit->Web3Utils.toWeiFromEth,
    //         price->Web3Utils.toWeiFromEth,
    //         numerator,
    //         denominator,
    //       );
    //     setDepositTimeInSeconds(_ => timeInSeconds);
    //   } else {
    //     ();
    //   };
    // };
    let setDeposit = value => {
      let (value, didUpdate) = InputHelp.onlyUpdateValueIfInRangeFloat(
        0.,
        float_of_string(maxAvailableDeposit),
        deposit,
        setInitialDeposit,
        value,
      )
      if didUpdate {
        let timeInSeconds = calculateDepositDuration(
          value->Web3Utils.toWeiFromEth,
          newPrice->Web3Utils.toWeiFromEth,
          numerator,
          denominator,
        )

        setDepositTimeInSeconds(_ => timeInSeconds)
      } else {
        ()
      }
    }
    let currency = switch chain {
    | Client.Neither
    | Client.MainnetQuery => "ether"
    | Client.MaticQuery => "DAI"
    }

    let openTransak = _ => {
      Js.log(Config.Transak.getConfig(~chain, web3Context))
      let transak = Transak.new_(Config.Transak.getConfig(~chain, web3Context))
      transak->Transak.init()
    }

    let (showTransakWarning, setShowTransakWarning) = React.useState(_ => false)

    let transakSteps = defaultView =>
      showTransakWarning
        ? <>
            <p> {"This service currently only works for pure ethereum wallets."->React.string} </p>
            <p>
              {"The following wallets are safe: Metamask, Portis, Torus, any wallet that uses words as a passphrase."->React.string}
            </p>
            <p>
              {"The following wallets aren't safe: Argent, Authereum, gnosis safe."->React.string}
            </p>
            <p> {"If you are unsure, please contact us."->React.string} </p>
            <button onClick=openTransak> {"Continue"->React.string} </button>
            <button onClick={_ => setShowTransakWarning(_ => false)}>
              {"Cancel"->React.string}
            </button>
          </>
        : defaultView

    <TxTemplate chain txState=txBuyAuctionState closeButtonText="Back to view Animal">
      <TxTemplate chain txState=txBuyState closeButtonText="Back to view Animal">
        <p> {("This wildcard uses " ++ currency)->React.string} </p>
        {
          // TODO: add link to an explainer of what "ether" or "DAI" is.
          isAbleToBuy
            ? <>
                <p>
                  {("Your available balance is: " ++ (paymentTokenBalance ++ (" " ++ currency)))
                    ->React.string}
                </p>
                {transakSteps(
                  <Rimble.Button onClick={_ => setShowTransakWarning(_ => true)}>
                    {("Buy More " ++ currency)->React.string}
                  </Rimble.Button>,
                )}
                <BuyInput
                  onSubmitBuy
                  setNewPrice
                  newPrice
                  deposit
                  depositTimeInSeconds
                  setDeposit
                  patronage
                  tokenIdName
                  maxAvailableDeposit
                />
              </>
            : <Rimble.Box>
                <p className=Styles.textOnlyModalText>
                  {React.string(
                    "You do not have enough " ++ (currency ++ (" to buy " ++ (tokenIdName ++ "."))),
                  )}
                </p>
                <p>
                  {("Your current balance is: " ++ (paymentTokenBalance ++ (" " ++ currency)))
                    ->React.string}
                </p>
                {transakSteps(
                  <Rimble.Button onClick={_ => setShowTransakWarning(_ => true)}>
                    {("Buy " ++ currency)->React.string}
                  </Rimble.Button>,
                )}
              </Rimble.Box>
        }
      </TxTemplate>
    </TxTemplate>
  }
}

@react.component
let make = (~chain, ~tokenId) => {
  let web3Context = RootProvider.useWeb3React()
  let optMaticState =
    web3Context.account
    ->Option.getWithDefault(CONSTANTS.nullEthAddress)
    ->QlHooks.useMaticState(
      ~forceRefetch=false,
      web3Context.chainId
      ->Option.getWithDefault(1)
      ->ContractActions.getChildChainId
      ->ContractActions.getMaticNetworkName,
    )

  switch chain {
  | Client.Neither
  | Client.MainnetQuery =>
    <Buy chain tokenId />
  | Client.MaticQuery =>
    switch optMaticState {
    | Some(maticState) =>
      switch maticState.error {
      | Some(error) =>
        Js.log2("matic state fetch error", error)
        <p>
          {"Error: Unable to get matic state - please try again or contact the Wildcards Team."->React.string}
        </p>
      | None => <Buy chain tokenId availableBalance={maticState.balance->BN.new_} />
      }
    | None => <p> {"Updating latest state."->React.string} </p>
    }
  }
}
