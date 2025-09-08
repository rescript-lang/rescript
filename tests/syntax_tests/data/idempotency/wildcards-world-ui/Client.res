type dataObject = {"__typename": string, "id": string}
// createInMemoryCache(~dataIdFromObject=(obj: dataObject) => obj##id, ());

/* Create an InMemoryCache */
// let inMemoryCache = () => ApolloInMemoryCache.createInMemoryCache();

/* Create an HTTP Link */
let httpLink = (~uri) => ApolloClient.Link.HttpLink.make(~uri=_ => uri, ())
/* Create an WS Link */
let wsLink = (~uri) => {
  open ApolloClient.Link.WebSocketLink
  make(~uri, ~options=ClientOptions.make(~reconnect=true, ()), ())
}

type context =
  | Neither
  | MaticQuery
  | MainnetQuery
let chainContextToStr = chain =>
  switch chain {
  | Neither => "neither"
  | MaticQuery => "matic"
  | MainnetQuery => "mainnet"
  }
type queryContext = {context: context}

@send
external getContext: ApolloClient__Link_Core_ApolloLink.Operation.Js_.t => option<queryContext> =
  "getContext"

/* based on test, execute left or right */
let webSocketHttpLink = (~uri, ~matic, ~subscriptions) =>
  ApolloClient.Link.split(
    ~test=({query, _}) => {
      let definition = ApolloClient.Utilities.getOperationDefinition(query)
      switch definition {
      | Some({kind, operation, _}) => kind === "OperationDefinition" && operation === "subscription"
      | None => false
      }
    },
    ~whenTrue=wsLink(~uri=subscriptions),
    ~whenFalse=ApolloClient.Link.split(~test=operation => {
      let context = operation->getContext

      let usingMatic = switch context {
      | Some({context}) =>
        switch context {
        | MaticQuery => true
        | Neither => false
        | MainnetQuery => false
        }
      | None => false
      }
      usingMatic
    }, ~whenTrue=httpLink(~uri=matic), ~whenFalse=httpLink(~uri)),
  )

type qlEndpoints = {
  mainnet: string,
  matic: string,
  ws: string,
}

let instance = (~getGraphEndpoints: unit => qlEndpoints) => {
  let {mainnet, matic, ws} = getGraphEndpoints()

  open ApolloClient
  make(
    ~cache=Cache.InMemoryCache.make(),
    ~connectToDevTools=true,
    ~defaultOptions=DefaultOptions.make(
      ~mutate=DefaultMutateOptions.make(~awaitRefetchQueries=true, ~errorPolicy=All, ()),
      ~query=DefaultQueryOptions.make(~fetchPolicy=NetworkOnly, ~errorPolicy=All, ()),
      ~watchQuery=DefaultWatchQueryOptions.make(~fetchPolicy=NetworkOnly, ~errorPolicy=All, ()),
      (),
    ),
    ~link=webSocketHttpLink(~uri=mainnet, ~matic, ~subscriptions=ws),
    (),
  )
}
