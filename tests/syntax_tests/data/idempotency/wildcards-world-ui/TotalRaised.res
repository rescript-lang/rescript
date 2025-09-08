open UsdPriceProvider
open Globals

module TotalRaisedEtherCountup = {
  @module("./TotalRaisedEtherCountup.js") @react.component
  external make: (~totalRaised: string) => React.element = "default"
}

type patronageRaised =
  | Loaded(string, option<string>)
  | Loading

let uesTotalPatronage = () => {
  let optTotalPatronageWei = QlHooks.useAmountRaised() //->Web3Utils.fromWeiBNToEth;
  let optCurrentUsdEthPrice = useUsdPrice() //->mapd(0., a => a);
  // let optCurrentUsdEthPrice = Some(0.5); //->mapd(0., a => a);

  switch optTotalPatronageWei {
  | Some(totalPatronageWei) =>
    let totalPatronageEth = totalPatronageWei->BN.toString->Web3Utils.fromWeiToEth

    let optTotaPatronageUsd =
      optCurrentUsdEthPrice->Belt.Option.flatMap(currentUsdEthPrice => Some(
        Js.Float.toFixedWithPrecision(
          Float.fromString(totalPatronageEth)->mapd(0., a => a) *. currentUsdEthPrice,
          ~digits=2,
        ),
      ))

    Loaded(totalPatronageEth, optTotaPatronageUsd)
  | _ => Loading
  }
}

@react.component
let make = () => {
  let totalPatronageRaised = uesTotalPatronage()

  switch totalPatronageRaised {
  | Loaded(totalRaised, optTotaPatronageUsd) =>
    <div
      className={
        open Css
        style(list{
          // These styles make the total raised counter always display in the centre.
          display(#flex),
          alignItems(#center),
          justifyContent(#center),
          flexDirection(#column),
        })
      }>
      <p
        className={
          open Css
          style(list{display(#table)})
        }>
        <small>
          <span className={Styles.totalRaisedText(1.5)}>
            {React.string("Wildcards has currently raised ")}
          </span>
          <br />
          <span className={Styles.totalRaisedText(4.)}>
            <TotalRaisedEtherCountup totalRaised /> <strong> {React.string(" ETH ")} </strong>
          </span>
          <br />
          {switch optTotaPatronageUsd {
          | Some(totalPatronageUsd) =>
            <React.Fragment>
              <span className={Styles.totalRaisedText(2.5)}>
                {React.string("(")}
                {React.string(totalPatronageUsd)}
                <strong> {React.string(" USD")} </strong>
                {React.string(")")}
              </span>
              <br />
              <span className={Styles.totalRaisedText(1.5)}>
                {React.string(" for conservation.")}
              </span>
            </React.Fragment>
          | None => React.null
          }}
        </small>
      </p>
    </div>
  | Loading => <Rimble.Loader />
  }
}
