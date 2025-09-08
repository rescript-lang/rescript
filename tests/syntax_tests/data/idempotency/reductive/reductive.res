module Store = {
  type rec t<'action, 'state> = {
    mutable state: 'state,
    mutable reducer: ('state, 'action) => 'state,
    mutable listeners: list<unit => unit>,
    customDispatcher: option<(t<'action, 'state>, 'action => unit, 'action) => unit>,
  }
  let create = (~reducer, ~preloadedState, ~enhancer=?, ()) =>
    switch (preloadedState, enhancer, reducer) {
    | (preloadedState, None, reducer) => {
        state: preloadedState,
        listeners: list{},
        reducer: reducer,
        customDispatcher: None,
      }
    | (preloadedState, Some(enhancer), reducer) => {
        state: preloadedState,
        listeners: list{},
        reducer: reducer,
        customDispatcher: Some(enhancer),
      }
    }
  let unsubscribe = (store, listener, ()) =>
    store.listeners = List.filter(l => listener !== l, store.listeners)
  let subscribe = (store, listener) => {
    store.listeners = list{listener, ...store.listeners}
    unsubscribe(store, listener)
  }
  let nativeDispatch = (store, action) => {
    store.state = store.reducer(store.state, action)
    List.iter(listener => listener(), store.listeners)
  }
  let dispatch = (store, action) =>
    switch store.customDispatcher {
    | Some(customDispatcher) => customDispatcher(store, nativeDispatch(store), action)
    | None => nativeDispatch(store, action)
    }
  let getState = store => store.state
  let replaceReducer = (store, reducer) => store.reducer = reducer
}

/* deprecated, use hooks API instead */
module Lense = {
  type state<'reductiveState> = {
    reductiveState: option<'reductiveState>,
    unsubscribe: option<unit => unit>,
  }
  type rec action =
    | UpdateState
    | AddListener(action => unit)
  let createMake = (~name="Lense", ~lense: 'state => 'lense, store: Store.t<'action, 'state>) => {
    let innerComponent = ReasonReact.reducerComponent(name)
    let make = (
      ~component: (
        ~state: 'lense,
        ~dispatch: 'action => unit,
        array<ReasonReact.reactElement>,
      ) => ReasonReact.component<'a, 'b, 'c>,
      _children: array<ReasonReact.reactElement>,
    ): ReasonReact.component<state<'lense>, ReasonReact.noRetainedProps, action> => {
      ...innerComponent,
      initialState: () => {
        reductiveState: Some(lense(Store.getState(store))),
        unsubscribe: None,
      },
      reducer: (action, state) =>
        switch action {
        | AddListener(send) =>
          ReasonReact.Update({
            unsubscribe: Some(Store.subscribe(store, _ => send(UpdateState))),
            reductiveState: Some(lense(Store.getState(store))),
          })
        | UpdateState =>
          lense(Store.getState(store))
          ->Some
          ->Belt.Option.eq(state.reductiveState, (a, b) => a === b)
            ? ReasonReact.NoUpdate
            : ReasonReact.Update({
                ...state,
                reductiveState: Some(lense(Store.getState(store))),
              })
        },
      didMount: ({send}) => send(AddListener(send)),
      willUnmount: ({state}) =>
        switch state.unsubscribe {
        | Some(unsubscribe) => unsubscribe()
        | None => ()
        },
      render: ({state}) =>
        switch state.reductiveState {
        | None => ReasonReact.null
        | Some(state) => ReasonReact.element(component(~state, ~dispatch=Store.dispatch(store), []))
        },
    }
    make
  }
}

/* deprecated, use provider from reductiveContext.re */
module Provider = {
  type state<'reductiveState> = Lense.state<'reductiveState>
  type action = Lense.action
  let createMake = (~name="Provider", store: Store.t<'action, 'state>) =>
    Lense.createMake(~name, ~lense=s => s, store)
}

/* ** These are all visible apis of Redux that aren't needed in Reason.
 * When used, build tools will provide explanation of alternatives.
 * (see .rei for those)
 */
let compose = _ => ()

let combineReducers = _ => ()

let applyMiddleware = _ => ()

let bindActionCreators = (actions, dispatch) => List.map((action, ()) => dispatch(action), actions)

type store<'action, 'state> = Store.t<'action, 'state>
type reducer<'action, 'state> = ('state, 'action) => 'state

type middleware<'action, 'state> = (store<'action, 'state>, 'action => unit, 'action) => unit

type storeCreator<'action, 'state> = (
  ~reducer: reducer<'action, 'state>,
  ~preloadedState: 'state,
  ~enhancer: middleware<'action, 'state>=?,
  unit,
) => store<'action, 'state>

type storeEnhancer<'action, 'state> = storeCreator<'action, 'state> => storeCreator<'action, 'state>

type liftedStoreEnhancer<'action, 'state, 'enhancedAction, 'enhancedState> = (
  ~reducer: reducer<'action, 'state>,
  ~preloadedState: 'enhancedState,
  ~enhancer: middleware<'enhancedAction, 'enhancedState>=?,
  unit,
) => store<'enhancedAction, 'enhancedState>
