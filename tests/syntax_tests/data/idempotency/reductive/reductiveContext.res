@@config({jsx: 3})
module type Config = {
  type state
  type action
}

module Make = (Config: Config) => {
  open Subscription
  let storeContext = React.createContext(None)

  module ContextProvider = {
    let make = React.Context.provider(storeContext)
  }

  module Provider = {
    @react.component
    let make = (~children, ~store) =>
      <ContextProvider value=Some(store)> children </ContextProvider>
  }

  let useStore = () => {
    let storeFromContext = React.useContext(storeContext)
    switch storeFromContext {
    | None =>
      failwith(
        "Could not find reductive context value; please ensure the component is wrapped in a <Provider>",
      )
    | Some(store) => store
    }
  }

  let useSelector = selector => {
    let store = useStore()

    let source = React.useMemo2(() => {
      subscribe: Reductive.Store.subscribe(store),
      getCurrentValue: () => selector(Reductive.Store.getState(store)),
    }, (selector, store))

    let selectedState = useSubscription(source)

    selectedState
  }

  let useDispatch = () => {
    let store = useStore()
    React.useCallback1(Reductive.Store.dispatch(store), [store])
  }
}
