type element = Jsx.element

@val external null: element = "null"

external float: float => element = "%identity"
external int: int => element = "%identity"
external string: string => element = "%identity"
external promise: promise<element> => element = "%identity"

external array: array<element> => element = "%identity"

type componentLike<'props, 'return> = Jsx.componentLike<'props, 'return>

type component<'props> = Jsx.component<'props>

external component: componentLike<'props, element> => component<'props> = "%identity"

@module("react")
external createElement: (component<'props>, 'props) => element = "createElement"

@module("react")
external cloneElement: (element, 'props) => element = "cloneElement"

@module("react")
external isValidElement: 'a => bool = "isValidElement"

@variadic @module("react")
external createElementVariadic: (component<'props>, 'props, array<element>) => element =
  "createElement"

@module("react/jsx-runtime")
external jsx: (component<'props>, 'props) => element = "jsx"

@module("react/jsx-runtime")
external jsxKeyed: (component<'props>, 'props, ~key: string=?, @ignore unit) => element = "jsx"

@module("react/jsx-runtime")
external jsxs: (component<'props>, 'props) => element = "jsxs"

@module("react/jsx-runtime")
external jsxsKeyed: (component<'props>, 'props, ~key: string=?, @ignore unit) => element = "jsxs"

type fragmentProps = {children?: element}

@module("react/jsx-runtime") external jsxFragment: component<fragmentProps> = "Fragment"

type ref<'value> = {mutable current: 'value}

@module("react")
external createRef: unit => ref<Js.nullable<'a>> = "createRef"

module Children = {
  @module("react") @scope("Children")
  external map: (element, element => element) => element = "map"
  @module("react") @scope("Children")
  external mapWithIndex: (element, (element, int) => element) => element = "map"
  @module("react") @scope("Children")
  external forEach: (element, element => unit) => unit = "forEach"
  @module("react") @scope("Children")
  external forEachWithIndex: (element, (element, int) => unit) => unit = "forEach"
  @module("react") @scope("Children")
  external count: element => int = "count"
  @module("react") @scope("Children")
  external only: element => element = "only"
  @module("react") @scope("Children")
  external toArray: element => array<element> = "toArray"
}

module Context = {
  type t<'context>

  type props<'context> = {
    value: 'context,
    children: element,
  }

  @get
  external provider: t<'context> => component<props<'context>> = "Provider"
}

@module("react")
external createContext: 'a => Context.t<'a> = "createContext"

@module("react")
external forwardRef: (('props, Js.Nullable.t<ref<'a>>) => element) => component<'props> =
  "forwardRef"

@module("react")
external memo: component<'props> => component<'props> = "memo"

@module("react")
external memoCustomCompareProps: (
  component<'props>,
  ('props, 'props) => bool,
) => component<'props> = "memo"

@module("react") external fragment: component<fragmentProps> = "Fragment"

module Fragment = {
  type props = {key?: string, children: element}

  @module("react")
  external make: component<props> = "Fragment"
}

module StrictMode = {
  type props = {key?: string, children: element}

  @module("react")
  external make: component<props> = "StrictMode"
}

module Suspense = {
  type props = {key?: string, children?: element, fallback?: element}

  @module("react")
  external make: component<props> = "Suspense"
}

type dynamicallyImportedModule<'a> = {default: component<'a>}

@module("react")
external lazy_: (unit => promise<dynamicallyImportedModule<'a>>) => component<'a> = "lazy"

let lazy_ = load => lazy_(async () => {default: await load()})

/* HOOKS */

/*
 * Yeah, we know this api isn't great. tl;dr: useReducer instead.
 * It's because useState can take functions or non-function values and treats
 * them differently. Lazy initializer + callback which returns state is the
 * only way to safely have any type of state and be able to update it correctly.
 */
@module("react")
external useState: (unit => 'state) => ('state, ('state => 'state) => unit) = "useState"

@module("react")
external useReducer: (('state, 'action) => 'state, 'state) => ('state, 'action => unit) =
  "useReducer"

@module("react")
external useReducerWithMapState: (
  ('state, 'action) => 'state,
  'initialState,
  'initialState => 'state,
) => ('state, 'action => unit) = "useReducer"

@module("react")
external useEffectOnEveryRender: (unit => option<unit => unit>) => unit = "useEffect"
@module("react")
external useEffect: (unit => option<unit => unit>, 'deps) => unit = "useEffect"
@module("react")
external useEffect0: (unit => option<unit => unit>, @as(json`[]`) _) => unit = "useEffect"
@module("react")
external useEffect1: (unit => option<unit => unit>, array<'a>) => unit = "useEffect"
@module("react")
external useEffect2: (unit => option<unit => unit>, ('a, 'b)) => unit = "useEffect"
@module("react")
external useEffect3: (unit => option<unit => unit>, ('a, 'b, 'c)) => unit = "useEffect"
@module("react")
external useEffect4: (unit => option<unit => unit>, ('a, 'b, 'c, 'd)) => unit = "useEffect"
@module("react")
external useEffect5: (unit => option<unit => unit>, ('a, 'b, 'c, 'd, 'e)) => unit = "useEffect"
@module("react")
external useEffect6: (unit => option<unit => unit>, ('a, 'b, 'c, 'd, 'e, 'f)) => unit = "useEffect"
@module("react")
external useEffect7: (unit => option<unit => unit>, ('a, 'b, 'c, 'd, 'e, 'f, 'g)) => unit =
  "useEffect"

@module("react")
external useLayoutEffectOnEveryRender: (unit => option<unit => unit>) => unit = "useLayoutEffect"
@module("react")
external useLayoutEffect: (unit => option<unit => unit>, 'deps) => unit = "useLayoutEffect"
@module("react")
external useLayoutEffect0: (unit => option<unit => unit>, @as(json`[]`) _) => unit =
  "useLayoutEffect"
@module("react")
external useLayoutEffect1: (unit => option<unit => unit>, array<'a>) => unit = "useLayoutEffect"
@module("react")
external useLayoutEffect2: (unit => option<unit => unit>, ('a, 'b)) => unit = "useLayoutEffect"
@module("react")
external useLayoutEffect3: (unit => option<unit => unit>, ('a, 'b, 'c)) => unit = "useLayoutEffect"
@module("react")
external useLayoutEffect4: (unit => option<unit => unit>, ('a, 'b, 'c, 'd)) => unit =
  "useLayoutEffect"
@module("react")
external useLayoutEffect5: (unit => option<unit => unit>, ('a, 'b, 'c, 'd, 'e)) => unit =
  "useLayoutEffect"
@module("react")
external useLayoutEffect6: (unit => option<unit => unit>, ('a, 'b, 'c, 'd, 'e, 'f)) => unit =
  "useLayoutEffect"
@module("react")
external useLayoutEffect7: (unit => option<unit => unit>, ('a, 'b, 'c, 'd, 'e, 'f, 'g)) => unit =
  "useLayoutEffect"

@module("react")
external useMemo: (unit => 'any, 'deps) => 'any = "useMemo"

@module("react")
external useMemo0: (unit => 'any, @as(json`[]`) _) => 'any = "useMemo"

@module("react")
external useMemo1: (unit => 'any, array<'a>) => 'any = "useMemo"

@module("react")
external useMemo2: (unit => 'any, ('a, 'b)) => 'any = "useMemo"

@module("react")
external useMemo3: (unit => 'any, ('a, 'b, 'c)) => 'any = "useMemo"

@module("react")
external useMemo4: (unit => 'any, ('a, 'b, 'c, 'd)) => 'any = "useMemo"

@module("react")
external useMemo5: (unit => 'any, ('a, 'b, 'c, 'd, 'e)) => 'any = "useMemo"

@module("react")
external useMemo6: (unit => 'any, ('a, 'b, 'c, 'd, 'e, 'f)) => 'any = "useMemo"

@module("react")
external useMemo7: (unit => 'any, ('a, 'b, 'c, 'd, 'e, 'f, 'g)) => 'any = "useMemo"

@module("react")
external useCallback: ('f, 'deps) => 'f = "useCallback"

@module("react")
external useCallback0: ('f, @as(json`[]`) _) => 'f = "useCallback"

@module("react")
external useCallback1: ('f, array<'a>) => 'f = "useCallback"

@module("react")
external useCallback2: ('f, ('a, 'b)) => 'f = "useCallback"

@module("react")
external useCallback3: ('f, ('a, 'b, 'c)) => 'f = "useCallback"

@module("react")
external useCallback4: ('f, ('a, 'b, 'c, 'd)) => 'f = "useCallback"

@module("react")
external useCallback5: ('f, ('a, 'b, 'c, 'd, 'e)) => 'f = "useCallback"

@module("react")
external useCallback6: ('callback, ('a, 'b, 'c, 'd, 'e, 'f)) => 'callback = "useCallback"

@module("react")
external useCallback7: ('callback, ('a, 'b, 'c, 'd, 'e, 'f, 'g)) => 'callback = "useCallback"

@module("react")
external useContext: Context.t<'any> => 'any = "useContext"

@module("react")
external usePromise: promise<'a> => 'a = "use"

@module("react") external useRef: 'value => ref<'value> = "useRef"

@module("react")
external useImperativeHandleOnEveryRender: (Js.Nullable.t<ref<'value>>, unit => 'value) => unit =
  "useImperativeHandle"

@module("react")
external useImperativeHandle: (Js.Nullable.t<ref<'value>>, unit => 'value, 'deps) => unit =
  "useImperativeHandle"

@module("react")
external useImperativeHandle0: (
  Js.Nullable.t<ref<'value>>,
  unit => 'value,
  @as(json`[]`) _,
) => unit = "useImperativeHandle"

@module("react")
external useImperativeHandle1: (Js.Nullable.t<ref<'value>>, unit => 'value, array<'a>) => unit =
  "useImperativeHandle"

@module("react")
external useImperativeHandle2: (Js.Nullable.t<ref<'value>>, unit => 'value, ('a, 'b)) => unit =
  "useImperativeHandle"

@module("react")
external useImperativeHandle3: (Js.Nullable.t<ref<'value>>, unit => 'value, ('a, 'b, 'c)) => unit =
  "useImperativeHandle"

@module("react")
external useImperativeHandle4: (
  Js.Nullable.t<ref<'value>>,
  unit => 'value,
  ('a, 'b, 'c, 'd),
) => unit = "useImperativeHandle"

@module("react")
external useImperativeHandle5: (
  Js.Nullable.t<ref<'value>>,
  unit => 'value,
  ('a, 'b, 'c, 'd, 'e),
) => unit = "useImperativeHandle"

@module("react")
external useImperativeHandle6: (
  Js.Nullable.t<ref<'value>>,
  unit => 'value,
  ('a, 'b, 'c, 'd, 'e, 'f),
) => unit = "useImperativeHandle"

@module("react")
external useImperativeHandle7: (
  Js.Nullable.t<ref<'value>>,
  unit => 'value,
  ('a, 'b, 'c, 'd, 'e, 'f, 'g),
) => unit = "useImperativeHandle"

@module("react") external useId: unit => string = "useId"

/** `useDeferredValue` is a React Hook that lets you defer updating a part of the UI. */
@module("react")
external useDeferredValue: ('value, ~initialValue: 'value=?) => 'value = "useDeferredValue"

@module("react")
external useInsertionEffectOnEveryRender: (unit => option<unit => unit>) => unit =
  "useInsertionEffect"
@module("react")
external useInsertionEffect: (unit => option<unit => unit>, 'deps) => unit = "useInsertionEffect"
@module("react")
external useInsertionEffect0: (unit => option<unit => unit>, @as(json`[]`) _) => unit =
  "useInsertionEffect"
@module("react")
external useInsertionEffect1: (unit => option<unit => unit>, array<'a>) => unit =
  "useInsertionEffect"
@module("react")
external useInsertionEffect2: (unit => option<unit => unit>, ('a, 'b)) => unit =
  "useInsertionEffect"
@module("react")
external useInsertionEffect3: (unit => option<unit => unit>, ('a, 'b, 'c)) => unit =
  "useInsertionEffect"
@module("react")
external useInsertionEffect4: (unit => option<unit => unit>, ('a, 'b, 'c, 'd)) => unit =
  "useInsertionEffect"
@module("react")
external useInsertionEffect5: (unit => option<unit => unit>, ('a, 'b, 'c, 'd, 'e)) => unit =
  "useInsertionEffect"
@module("react")
external useInsertionEffect6: (unit => option<unit => unit>, ('a, 'b, 'c, 'd, 'e, 'f)) => unit =
  "useInsertionEffect"
@module("react")
external useInsertionEffect7: (unit => option<unit => unit>, ('a, 'b, 'c, 'd, 'e, 'f, 'g)) => unit =
  "useInsertionEffect"

@module("react")
external useSyncExternalStore: (
  ~subscribe: (unit => unit) => unit => unit,
  ~getSnapshot: unit => 'state,
) => 'state = "useSyncExternalStore"

@module("react")
external useSyncExternalStoreWithServerSnapshot: (
  ~subscribe: (unit => unit) => unit => unit,
  ~getSnapshot: unit => 'state,
  ~getServerSnapshot: unit => 'state,
) => 'state = "useSyncExternalStore"

module Uncurried = {
  @module("react")
  external useState: (unit => 'state) => ('state, ('state => 'state) => unit) = "useState"

  @module("react")
  external useReducer: (('state, 'action) => 'state, 'state) => ('state, 'action => unit) =
    "useReducer"

  @module("react")
  external useReducerWithMapState: (
    ('state, 'action) => 'state,
    'initialState,
    'initialState => 'state,
  ) => ('state, 'action => unit) = "useReducer"

  @module("react")
  external useCallback: ('f, 'deps) => 'f = "useCallback"

  @module("react")
  external useCallback0: ('f, @as(json`[]`) _) => 'f = "useCallback"

  @module("react")
  external useCallback1: ('f, array<'a>) => 'f = "useCallback"

  @module("react")
  external useCallback2: ('f, ('a, 'b)) => 'f = "useCallback"

  @module("react")
  external useCallback3: ('f, ('a, 'b, 'c)) => 'f = "useCallback"

  @module("react")
  external useCallback4: ('f, ('a, 'b, 'c, 'd)) => 'f = "useCallback"

  @module("react")
  external useCallback5: ('f, ('a, 'b, 'c, 'd, 'e)) => 'f = "useCallback"

  @module("react")
  external useCallback6: ('callback, ('a, 'b, 'c, 'd, 'e, 'f)) => 'callback = "useCallback"

  @module("react")
  external useCallback7: ('callback, ('a, 'b, 'c, 'd, 'e, 'f, 'g)) => 'callback = "useCallback"
}

@set
external setDisplayName: (component<'props>, string) => unit = "displayName"

@get @return(nullable)
external displayName: component<'props> => option<string> = "displayName"

// Actions

type transitionFunction = unit => promise<unit>

type transitionStartFunction = transitionFunction => unit

/** `useTransition` is a React Hook that lets you render a part of the UI in the background. */
@module("react")
external useTransition: unit => (bool, transitionStartFunction) = "useTransition"

type action<'state, 'payload> = ('state, 'payload) => promise<'state>

type formAction<'formData> = 'formData => promise<unit>

/** `useActionState` is a Hook that allows you to update state based on the result of a form action. */
@module("react")
external useActionState: (
  action<'state, 'payload>,
  'state,
  ~permalink: string=?,
) => ('state, formAction<'payload>, bool) = "useActionState"

/** `useOptimistic` is a React Hook that lets you optimistically update the UI. */
@module("react")
external useOptimistic: (
  'state,
  ~updateFn: ('state, 'action) => 'state=?,
) => ('state, 'action => unit) = "useOptimistic"

/** `act` is a test helper to apply pending React updates before making assertions. */
@module("react")
external act: (unit => promise<unit>) => promise<unit> = "act"
