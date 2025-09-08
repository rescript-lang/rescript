open Globals

let priceWeiToTuple = (wei, optCurrentUsdEthPrice) => {
  let totalPatronageEth = wei->Eth.toFixedWithPrecisionNoTrailingZeros

  let optTotaPatronageUsd =
    optCurrentUsdEthPrice->Option.map(currentUsdEthPrice =>
      toFixedWithPrecisionNoTrailingZeros(
        Float.fromString(totalPatronageEth)->mapd(0., a => a) *. currentUsdEthPrice,
        ~digits=2,
      )
    )

  (totalPatronageEth, optTotaPatronageUsd)
}

let usePrice = (~chain, animal) => {
  let optPriceWei = QlHooks.usePrice(~chain, animal)
  let optCurrentUsdEthPrice = UsdPriceProvider.useUsdPrice()

  switch optPriceWei {
  | Price(totalPatronageWei) => priceWeiToTuple(totalPatronageWei, optCurrentUsdEthPrice)->Some
  | Foreclosed(_) => Some(("0", Some("0")))
  | Loading => None
  }
}

module PurePriceDisplay = {
  @react.component
  let make = (~priceEth, ~optPriceUsd) => <>
    <p className={Styles.noMarginTop ++ (" " ++ Styles.noMarginBottom)}>
      {(priceEth ++ " ETH")->restr}
    </p>
    {switch optPriceUsd {
    | Some(priceUsd) =>
      <p className=Styles.noMarginTop> <small> {("(" ++ (priceUsd ++ " USD)"))->restr} </small> </p>
    | None => React.null
    }}
  </>
}

module InUSD = {
  @react.component
  let make = (~chain, ~animal: TokenId.t) => {
    let optPriceWei = QlHooks.usePrice(~chain, animal)

    switch optPriceWei {
    | Price(totalPatronageWei) =>
      <p className={Styles.noMarginTop ++ (" " ++ Styles.noMarginBottom)}>
        {(totalPatronageWei->Eth.toFixedWithPrecisionNoTrailingZeros ++ " USD")->restr}
      </p>
    | Foreclosed(_) =>
      <p className={Styles.noMarginTop ++ (" " ++ Styles.noMarginBottom)}> {"0 USD"->restr} </p>

    | Loading => <Rimble.Loader />
    }
  }
}

module InEth = {
  @react.component
  let make = (~chain, ~animal: TokenId.t) => {
    let optCurrentPrice = usePrice(~chain, animal)

    switch optCurrentPrice {
    | Some((priceEth, optPriceUsd)) => <PurePriceDisplay priceEth optPriceUsd />
    | None => <Rimble.Loader />
    }
  }
}
@react.component
let make = (~chain, ~animal: TokenId.t) =>
  switch chain {
  | Client.MainnetQuery => <InEth chain animal />
  | Client.Neither
  | Client.MaticQuery =>
    <InUSD chain animal />
  }
