// [%raw {|require("./custom.css")|}];
open Globals

module Router = {
  @react.component
  let make = () => {
    let url = ReasonReactRouter.useUrl()
    switch url.path {
    // | ["new-data"] => <QlHooks />
    | list{_} => <p> {React.string("Unknown page")} </p>
    | _ => <Layout />
    }
  }
}

module ApolloProvider = {
  @react.component
  let make = (~children, ~client) =>
    <ApolloClient.React.ApolloProvider client> children </ApolloClient.React.ApolloProvider>
}

@val
external mainnetApi: option<string> = "process.env.REACT_APP_MAINNET_BE"
@val
external goerliApi: option<string> = "process.env.REACT_APP_GOERLI_BE"

// TODO: SSR doesn't work correctly here, need to use the external apollo client
@react.component
let make = () =>
  <WildcardsProvider
    getGraphEndpoints={(networkId, ()) => {
      open Client
      let endpoints = switch networkId {
      | 5 => {
          mainnet: \"||||"(goerliApi, "https://goerli.api.wildcards.world/v1/graphq"),
          matic: "https://mumbai.graph.wildcards.world/subgraphs/name/wildcards-world/wildcards-matic/graphql",
          ws: "wss://api.thegraph.com/subgraphs/name/wildcards-world/wildcards-goerli",
        }
      | _ => {
          mainnet: \"||||"(mainnetApi, "https://api.wildcards.world/v1/graphql"),
          matic: "https://mumbai.graph.wildcards.world/subgraphs/name/wildcards-world/wildcards-matic/graphql",
          ws: "wss://api.thegraph.com/subgraphs/name/wildcards-world/wildcards",
        }
      }
      endpoints
    }}>
    <Router />
  </WildcardsProvider>
