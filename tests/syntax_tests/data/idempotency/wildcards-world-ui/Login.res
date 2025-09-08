type connectorObj = {
  name: string,
  connector: Web3Connectors.injectedType,
  img: string,
  connectionPhrase: string,
}

@module("./bindings/web3-react/connectors")
external connectors: Js.Array.t<connectorObj> = "default"

@react.component
let make = () => {
  let (_connectionStatus, activateConnector) = RootProvider.useActivateConnector()

  <div>
    <p>
      {"Use one of the wallet providers below. "->React.string}
      <small>
        {"(Not sure where to go from here? "->React.string}
        <a
          href="https://blog.wildcards.world/how-to-buy-a-wildcard-web3-ethereum/"
          target=// Decided to leave this wildcard braning in here ;)
          "_blank"
          rel="noopener noreferrer">
          <span className=Styles.colorGreen> {"Read this guide"->React.string} </span>
        </a>
        {")"->React.string}
      </small>
    </p>
    <div
      className={
        open Css
        style(list{
          display(#grid),
          gridTemplateColumns(list{#repeat(#autoFit, #minmax(px(176), fr(0.6)))}),
          maxWidth(px(800)),
        })
      }>
      {connectors
      ->Belt.Array.mapWithIndex((index, connector) =>
        <div
          key={index->string_of_int}
          onClick={_e => activateConnector(connector.connector)}
          className={
            open Css
            style(list{border(px(1), #solid, rgba(195, 195, 195, 0.14)), hover(list{})})
          }>
          <div
            className={
              open Css
              style(list{
                margin(px(8)),
                display(#flex),
                justifyContent(#center),
                alignItems(#center),
                flexDirection(column),
                cursor(#pointer),
                borderRadius(px(12)),
                hover(list{backgroundColor(rgba(195, 195, 195, 0.14))}),
                transition(~duration=200, ~delay=0, ~timingFunction=easeInOut, "background-color"),
              })
            }>
            <div
              className={
                open Css
                style(list{width(px(45)), height(px(45))})
              }>
              <img
                src=connector.img
                alt="MetaMask"
                className={
                  open Css
                  style(list{width(#percent(100.)), height(#percent(100.))})
                }
              />
            </div>
            <div
              className={
                open Css
                style(list{fontSize(px(24)), fontWeight(#num(700)), marginTop(em(0.5))})
              }>
              {connector.name->React.string}
            </div>
            <div
              className={
                open Css
                style(list{fontSize(px(15)), marginTop(em(0.35)), color(rgb(169, 169, 188))})
              }>
              {connector.connectionPhrase->React.string}
            </div>
          </div>
        </div>
      )
      ->React.array}
    </div>
  </div>
}
