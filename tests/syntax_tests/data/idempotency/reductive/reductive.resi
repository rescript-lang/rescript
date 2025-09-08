module Store: {
  type t<'action, 'state>
  let create: (
    ~reducer: ('state, 'action) => 'state,
    ~preloadedState: 'state,
    ~enhancer: (t<'action, 'state>, 'action => unit, 'action) => unit=?,
    unit,
  ) => t<'action, 'state>
  let unsubscribe: (t<'action, 'state>, unit => unit, unit) => unit
  let subscribe: (t<'action, 'state>, unit => unit, unit) => unit
  /* skips all middleware and applies an update directly to the store */
  let nativeDispatch: (t<'action, 'state>, 'action) => unit
  let dispatch: (t<'action, 'state>, 'action) => unit
  let getState: t<'action, 'state> => 'state
  let replaceReducer: (t<'action, 'state>, ('state, 'action) => 'state) => unit
}

module Lense: {
  type state<'reductiveState>
  type rec action =
    | UpdateState
    | AddListener(action => unit)

  @deprecated("Legacy API, prefer the new hooks API with jsx 3")
  let createMake: (
    ~name: string=?,
    ~lense: 'state => 'lense,
    Store.t<'action, 'state>,
    ~component: (
      ~state: 'lense,
      ~dispatch: 'action => unit,
      array<ReasonReact.reactElement>,
    ) => ReasonReact.component<'a, 'b, 'c>,
    array<ReasonReact.reactElement>,
  ) => ReasonReact.component<state<'lense>, ReasonReact.noRetainedProps, action>
}

module Provider: {
  type state<'reductiveState> = Lense.state<'reductiveState>
  type action = Lense.action

  @deprecated("Legacy API, prefer the new hooks API with jsx 3")
  let createMake: (
    ~name: string=?,
    Store.t<'action, 'state>,
    ~component: (
      ~state: 'state,
      ~dispatch: 'action => unit,
      array<ReasonReact.reactElement>,
    ) => ReasonReact.component<'a, 'b, 'c>,
    array<ReasonReact.reactElement>,
  ) => ReasonReact.component<state<'state>, ReasonReact.noRetainedProps, action>
}

/* ** These are all visible apis of Redux that aren't needed in Reason.
 * When used, build tools will provide explanation of alternatives.
 */
@ocaml.deprecated(`
Use the |> as an infix operator to chain the
result of one function into another:

\`compose(f, g, h)(x)\`
in JS goes to
\`x |> h |> g |> f\`
in Reason.
`)
let compose: _ => unit

@ocaml.deprecated(`
combineReducers uses some introspection to determine
the shape of your state. Instead, consider a declarative pattern like:

type counterAction =
| Increment
| Decrement;
type stringAction =
| A
| B;
type action =
| StringAction stringAction
| CounterAction counterAction;
type state = {string, counter};

let combinedReducer state action => {
| StringAction action => {...state, string: stringReducer state action}
| CounterAction action => {...state, counter: counterReducer state action}
};

this pattern gives you full control over the shape of your state.
`)
let combineReducers: _ => unit

@ocaml.deprecated(`
The enhancer attribute in Redux allows you
to provide a custom dispatch method (to perform more
actions before or after the dispatch function). You can simply pass in
a function directly which handles the exact actions you're looking for.

To chain middlewares you can do something like:

let thunkedLoggedTimeTravelLogger store next =>
  Middleware.thunk store @@
  Middleware.logger store @@
  Middleware.timeTravel store @@
  next;
`)
let applyMiddleware: _ => unit

@ocaml.deprecated(`
bindActionCreators is not as useful in Reason,
since action creators are types, not functions.
The code is implemented as:

let bindActionCreators actions dispatch =>
List.map (fun action () => dispatch action) actions;

Instead - you are free to build the action data type at dispatch time.
`)
let bindActionCreators: (list<'a>, 'a => 'b) => list<unit => 'b>

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
