module GraphQl = {
  @react.component
  let make = (~getGraphEndpoints: (int, unit) => Client.qlEndpoints, ~children) => {
    let networkId = RootProvider.useNetworkId()->Belt.Option.mapWithDefault(1, a => a)
    let client = React.useMemo2(
      () => Client.instance(~getGraphEndpoints=getGraphEndpoints(networkId)),
      (getGraphEndpoints, networkId),
    )

    <ApolloClient.React.ApolloProvider client> children </ApolloClient.React.ApolloProvider>
  }
}

@react.component
let make = (
  ~getGraphEndpoints: (int, unit) => Client.qlEndpoints,
  ~children,
  ~stewardContractAddress: option<Web3.ethAddress>=?,
  ~stewardAbi: option<Web3.abi>=?,
) =>
  <RootProvider stewardContractAddress stewardAbi>
    <GraphQl getGraphEndpoints> children </GraphQl>
  </RootProvider>
