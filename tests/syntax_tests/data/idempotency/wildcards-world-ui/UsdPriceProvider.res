open Globals

type usdPrice = {usdPrice: option<float>}

let initialState = {usdPrice: None}

// TODO: this should move to using a graphql based price feed (preferably from thegraph (via uniswap or makers oracles?))
module PriceProvider = {
  let context: React.Context.t<option<float>> = React.createContext(None)
  let make = React.Context.provider(context)
}

@decco @deriving(accessors)
type price = {c: array<string>}
@decco @deriving(accessors)
type ethUsdPrice = {
  @decco.key("XETHZUSD")
  ethUsd: option<price>,
}
@decco @deriving(accessors)
type krakenPriceResponse = {result: option<ethUsdPrice>}

let getPrice = () => {
  open Fetch
  %Async({
    let result = fetch("https://api.kraken.com/0/public/Ticker?pair=ETHUSD")
    %Async({
      let response = result->Response.json
      let krakenPriceObj = response->krakenPriceResponse_decode

      let usdEthPrice = switch krakenPriceObj {
      | Ok(priceObj) =>
        let getPriceFromArray = a => a->Array.get(0)
        \">>="(\"<$>"(\">>="(priceObj.result, ethUsd), c), getPriceFromArray)
      | Result.Error(error) =>
        Js.log(error)
        None
      }

      usdEthPrice->async
    })
  })
}

@react.component
let make = (~children) => {
  let (etherUsdPrice, setEtherUsdPrice) = React.useState(_ => None)

  React.useEffect2(() => {
    if etherUsdPrice == None {
      let _ =
        getPrice()->mapAsync(newPrice => setEtherUsdPrice(_ => \">>="(newPrice, Float.fromString)))
    } else {
      ()
    }
    None
  }, (setEtherUsdPrice, etherUsdPrice))

  <PriceProvider value=etherUsdPrice> children </PriceProvider>
}

let useUsdPrice = () => React.useContext(PriceProvider.context)
