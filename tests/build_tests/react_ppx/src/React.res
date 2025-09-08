type element

@val external null: element = "null"

external string: string => element = "%identity"

external array: array<element> => element = "%identity"

type componentLike<'props, 'return> = 'props => 'return

type component<'props> = componentLike<'props, element>

@module("react")
external createElement: (component<'props>, 'props) => element = "createElement"

@module("react")
external cloneElement: (component<'props>, 'props) => element = "cloneElement"

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

type ref<'value> = {mutable current: 'value}

module Ref = {
  type t<'value>

  @get external current: t<'value> => 'value = "current"
  @set external setCurrent: (t<'value>, 'value) => unit = "current"
}

@module("react")
external createRef: unit => Ref.t<Js.nullable<'a>> = "createRef"

module Children = {
  @module("react") @scope("Children") @val
  external map: (element, element => element) => element = "map"
  @module("react") @scope("Children") @val
  external forEach: (element, element => unit) => unit = "forEach"
  @module("react") @scope("Children") @val
  external count: element => int = "count"
  @module("react") @scope("Children") @val
  external only: element => element = "only"
  @module("react") @scope("Children") @val
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
external forwardRef: (('props, Js.Nullable.t<Ref.t<'a>>) => element) => component<'props> =
  "forwardRef"

@module("react")
external memo: component<'props> => component<'props> = "memo"

@module("react")
external memoCustomCompareProps: (
  component<'props>,
  ('props, 'props) => bool,
) => component<'props> = "memo"

module Fragment = {
  type props = {key?: string, children: element}

  @module("react")
  external make: component<props> = "Fragment"
}

module Suspense = {
  type props = {key?: string, children?: element, fallback?: element}

  @module("react")
  external make: component<props> = "Suspense"
}

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
external useEffect: (unit => option<unit => unit>) => unit = "useEffect"
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
external useLayoutEffect: (unit => option<unit => unit>) => unit = "useLayoutEffect"
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
external useMemo: (unit => 'any) => 'any = "useMemo"
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

/* This is used as return values */
type callback<'input, 'output> = 'input => 'output

@module("react")
external useCallback: ('input => 'output) => callback<'input, 'output> = "useCallback"
@module("react")
external useCallback0: ('input => 'output, @as(json`[]`) _) => callback<'input, 'output> =
  "useCallback"
@module("react")
external useCallback1: ('input => 'output, array<'a>) => callback<'input, 'output> = "useCallback"
@module("react")
external useCallback2: ('input => 'output, ('a, 'b)) => callback<'input, 'output> = "useCallback"
@module("react")
external useCallback3: ('input => 'output, ('a, 'b, 'c)) => callback<'input, 'output> =
  "useCallback"
@module("react")
external useCallback4: ('input => 'output, ('a, 'b, 'c, 'd)) => callback<'input, 'output> =
  "useCallback"
@module("react")
external useCallback5: ('input => 'output, ('a, 'b, 'c, 'd, 'e)) => callback<'input, 'output> =
  "useCallback"
@module("react")
external useCallback6: ('input => 'output, ('a, 'b, 'c, 'd, 'e, 'f)) => callback<'input, 'output> =
  "useCallback"
@module("react")
external useCallback7: (
  'input => 'output,
  ('a, 'b, 'c, 'd, 'e, 'f, 'g),
) => callback<'input, 'output> = "useCallback"

@module("react")
external useContext: Context.t<'any> => 'any = "useContext"

@module("react") external useRef: 'value => Ref.t<'value> = "useRef"

@module("react")
external useImperativeHandle0: (
  Js.Nullable.t<Ref.t<'value>>,
  unit => 'value,
  @as(json`[]`) _,
) => unit = "useImperativeHandle"

@module("react")
external useImperativeHandle1: (Js.Nullable.t<Ref.t<'value>>, unit => 'value, array<'a>) => unit =
  "useImperativeHandle"

@module("react")
external useImperativeHandle2: (Js.Nullable.t<Ref.t<'value>>, unit => 'value, ('a, 'b)) => unit =
  "useImperativeHandle"

@module("react")
external useImperativeHandle3: (
  Js.Nullable.t<Ref.t<'value>>,
  unit => 'value,
  ('a, 'b, 'c),
) => unit = "useImperativeHandle"

@module("react")
external useImperativeHandle4: (
  Js.Nullable.t<Ref.t<'value>>,
  unit => 'value,
  ('a, 'b, 'c, 'd),
) => unit = "useImperativeHandle"

@module("react")
external useImperativeHandle5: (
  Js.Nullable.t<Ref.t<'value>>,
  unit => 'value,
  ('a, 'b, 'c, 'd, 'e),
) => unit = "useImperativeHandle"

@module("react")
external useImperativeHandle6: (
  Js.Nullable.t<Ref.t<'value>>,
  unit => 'value,
  ('a, 'b, 'c, 'd, 'e, 'f),
) => unit = "useImperativeHandle"

@module("react")
external useImperativeHandle7: (
  Js.Nullable.t<Ref.t<'value>>,
  unit => 'value,
  ('a, 'b, 'c, 'd, 'e, 'f, 'g),
) => unit = "useImperativeHandle"

@set
external setDisplayName: (component<'props>, string) => unit = "displayName"
