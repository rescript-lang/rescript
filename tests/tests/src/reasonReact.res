// Prevent warning about the `retainedProps` field being defined in both
// `self` and `componentSpec` record types.
@@warning("-30")

type reactClass

type jsProps

type reactElement = React.element

type reactRef

@val external null: reactElement = "null"

external string: string => reactElement = "%identity"

external array: array<reactElement> => reactElement = "%identity"

external refToJsObj: reactRef => {..} = "%identity"

@variadic @val @module("react")
external createElement: (reactClass, ~props: {..}=?, array<reactElement>) => reactElement =
  "createElement"

@variadic @module("react")
external cloneElement: (reactElement, ~props: {..}=?, array<reactElement>) => reactElement =
  "cloneElement"

@val @module("react")
external createElementVerbatim: 'a = "createElement"

let createDomElement = (s, ~props, children) => {
  let vararg = Js.Array.concat(children, [Obj.magic(s), Obj.magic(props)])
  /* Use varargs to avoid warnings on duplicate keys in children */
  Obj.magic(createElementVerbatim)["apply"](Js.Nullable.null, vararg)
}

@val external magicNull: 'a = "null"

type reactClassInternal = reactClass

type renderNotImplemented = RenderNotImplemented

type stateless = unit

type noRetainedProps = unit

type actionless = unit

/* **
 * Elements are what JSX blocks become. They represent the *potential* for a
 * component instance and state to be created / updated. They are not yet
 * instances.
 */
type rec element = Element(component<'state, 'retainedProps, 'action>): element
and jsPropsToReason<'jsProps, 'state, 'retainedProps, 'action> = 'jsProps => component<
  'state,
  'retainedProps,
  'action,
>
and uncurriedJsPropsToReason<'jsProps, 'state, 'retainedProps, 'action> = 'jsProps => component<
  'state,
  'retainedProps,
  'action,
>
/* **
 * Type of hidden field for Reason components that use JS components
 */
and jsElementWrapped = option<
  (~key: Js.nullable<string>, ~ref: Js.nullable<Js.nullable<reactRef> => unit>) => reactElement,
>
and update<'state, 'retainedProps, 'action> =
  | NoUpdate
  | Update('state)
  | SideEffects(self<'state, 'retainedProps, 'action> => unit)
  | UpdateWithSideEffects('state, self<'state, 'retainedProps, 'action> => unit)
/* **
 * Granularly types state, and initial state as being independent, so that we
 * may include a template that all instances extend from.
 */
and componentSpec<'state, 'initialState, 'retainedProps, 'initialRetainedProps, 'action> = {
  debugName: string,
  reactClassInternal: reactClassInternal,
  /* Keep here as a way to prove that the API may be implemented soundly */
  mutable handedOffState: ref<option<'state>>,
  willReceiveProps: self<'state, 'retainedProps, 'action> => 'state,
  didMount: self<'state, 'retainedProps, 'action> => unit,
  didUpdate: oldNewSelf<'state, 'retainedProps, 'action> => unit,
  willUnmount: self<'state, 'retainedProps, 'action> => unit,
  willUpdate: oldNewSelf<'state, 'retainedProps, 'action> => unit,
  shouldUpdate: oldNewSelf<'state, 'retainedProps, 'action> => bool,
  render: self<'state, 'retainedProps, 'action> => reactElement,
  initialState: unit => 'initialState,
  retainedProps: 'initialRetainedProps,
  reducer: ('action, 'state) => update<'state, 'retainedProps, 'action>,
  jsElementWrapped: jsElementWrapped,
}
and component<'state, 'retainedProps, 'action> = componentSpec<
  'state,
  'state,
  'retainedProps,
  'retainedProps,
  'action,
>
and self<'state, 'retainedProps, 'action> = {
  handle: 'payload. (('payload, self<'state, 'retainedProps, 'action>) => unit, 'payload) => unit,
  state: 'state,
  retainedProps: 'retainedProps,
  send: 'action => unit,
  onUnmount: (unit => unit) => unit,
}
and oldNewSelf<'state, 'retainedProps, 'action> = {
  oldSelf: self<'state, 'retainedProps, 'action>,
  newSelf: self<'state, 'retainedProps, 'action>,
}

type rec jsComponentThis<'state, 'props, 'retainedProps, 'action> = {
  "state": totalState<'state, 'retainedProps, 'action>,
  "props": {"reasonProps": 'props},
  "setState": (
    (
      totalState<'state, 'retainedProps, 'action>,
      'props,
    ) => totalState<'state, 'retainedProps, 'action>,
    Js.nullable<unit => unit>,
  ) => unit,
  "jsPropsToReason": option<uncurriedJsPropsToReason<'props, 'state, 'retainedProps, 'action>>,
}
/* **
 * `totalState` tracks all of the internal reason API bookkeeping.
 *
 * Since we will mutate `totalState` in `shouldComponentUpdate`, and since
 * there's no guarantee that returning true from `shouldComponentUpdate`
 * guarantees that a component's update *actually* takes place (it could get
 * rolled back by Fiber etc), then we should put all properties that we
 * mutate directly on the totalState, so that when Fiber makes backup shallow
 * backup copies of `totalState`, our changes can be rolled back correctly
 * even when we mutate them.
 */
and totalState<'state, 'retainedProps, 'action> = {"reasonState": 'state}

let anyToUnit = _ => ()

let anyToTrue = _ => true

let willReceivePropsDefault = ({state}) => state

let renderDefault = _self => string("RenderNotImplemented")

let initialStateDefault = () => ()

let reducerDefault: ('action, 'state) => update<'state, 'retainedProps, 'action> = (
  _action,
  _state,
) => NoUpdate

let convertPropsIfTheyreFromJs = (props, jsPropsToReason, debugName) => {
  let props = Obj.magic(props)
  switch (Js.Nullable.toOption(props["reasonProps"]), jsPropsToReason) {
  | (Some(props), _) => props
  | (None, Some(toReasonProps)) => Element(toReasonProps(props))
  | (None, None) =>
    throw(
      Invalid_argument(
        "A JS component called the Reason component " ++
        (debugName ++
        " which didn't implement the JS->Reason React props conversion."),
      ),
    )
  }
}

// Old reason-react sources converted from .re syntax.
// The OCaml object system is not available in ReScript anymore
// let createClass = (type reasonState retainedProps action, debugName): reactClass =>
//   ReasonReactOptimizedCreateClass.createClass(. Pexp_object not implemented in printer)
let createClass = Obj.magic

let basicComponent = debugName => {
  let componentTemplate = {
    reactClassInternal: createClass(debugName),
    debugName,
    /* Keep here as a way to prove that the API may be implemented soundly */
    handedOffState: {
      contents: None,
    },
    didMount: anyToUnit,
    willReceiveProps: willReceivePropsDefault,
    didUpdate: anyToUnit,
    willUnmount: anyToUnit,
    willUpdate: anyToUnit,
    /* **
     * Called when component will certainly mount at some point - and may be
     * called on the sever for server side React rendering.
     */
    shouldUpdate: anyToTrue,
    render: renderDefault,
    initialState: initialStateDefault,
    reducer: reducerDefault,
    jsElementWrapped: None,
    retainedProps: (),
  }
  componentTemplate
}

let statelessComponent = (debugName): component<stateless, noRetainedProps, actionless> =>
  basicComponent(debugName)

let statelessComponentWithRetainedProps = (debugName): componentSpec<
  stateless,
  stateless,
  'retainedProps,
  noRetainedProps,
  actionless,
> => basicComponent(debugName)

let reducerComponent = (debugName): componentSpec<
  'state,
  stateless,
  noRetainedProps,
  noRetainedProps,
  'action,
> => basicComponent(debugName)

let reducerComponentWithRetainedProps = (debugName): componentSpec<
  'state,
  stateless,
  'retainedProps,
  noRetainedProps,
  'action,
> => basicComponent(debugName)

/* **
 * Convenience for creating React elements before we have a better JSX transform.  Hopefully this makes it
 * usable to build some components while waiting to migrate the JSX transform to the next API.
 *
 * Constrain the component here instead of relying on the Element constructor which would lead to confusing
 * error messages.
 */
let element = (
  ~key: string=Obj.magic(Js.Nullable.undefined),
  ~ref: Js.nullable<reactRef> => unit=Obj.magic(Js.Nullable.undefined),
  component: component<'state, 'retainedProps, 'action>,
) => {
  let element = Element(component)
  switch component.jsElementWrapped {
  | Some(jsElementWrapped) =>
    jsElementWrapped(~key=Js.Nullable.return(key), ~ref=Js.Nullable.return(ref))
  | None =>
    createElement(
      component.reactClassInternal,
      ~props={"key": key, "ref": ref, "reasonProps": element},
      [],
    )
  }
}

let wrapReasonForJs = (
  ~component,
  jsPropsToReason: jsPropsToReason<'jsProps, 'state, 'retainedProps, 'action>,
) => {
  let jsPropsToReason: jsPropsToReason<jsProps, 'state, 'retainedProps, 'action> = Obj.magic(
    jsPropsToReason,
  ) /* cast 'jsProps to jsProps */
  let uncurriedJsPropsToReason: uncurriedJsPropsToReason<
    jsProps,
    'state,
    'retainedProps,
    'action,
  > = jsProps => jsPropsToReason(jsProps)
  Obj.magic(component.reactClassInternal)["prototype"]["jsPropsToReason"] = Some(
    uncurriedJsPropsToReason,
  )
  component.reactClassInternal
}

module WrapProps = {
  /* We wrap the props for reason->reason components, as a marker that "these props were passed from another
   reason component" */
  let wrapProps = (
    ~reactClass,
    ~props,
    children,
    ~key: Js.nullable<string>,
    ~ref: Js.nullable<Js.nullable<reactRef> => unit>,
  ) => {
    let props = Js.Obj.assign(
      Js.Obj.assign(Js.Obj.empty(), Obj.magic(props)),
      {"ref": ref, "key": key},
    )
    let varargs = Js.Array.concat(Obj.magic(children), [Obj.magic(reactClass), Obj.magic(props)])
    /* Use varargs under the hood */
    Obj.magic(createElementVerbatim)["apply"](Js.Nullable.null, varargs)
  }
  let dummyInteropComponent = basicComponent("interop")
  let wrapJsForReason = (~reactClass, ~props, children): component<
    stateless,
    noRetainedProps,
    _,
  > => {
    let jsElementWrapped = Some(wrapProps(~reactClass, ~props, children, ...))
    {...dummyInteropComponent, jsElementWrapped}
  }
}

let wrapJsForReason = WrapProps.wrapJsForReason

@module("react") external fragment: 'a = "Fragment"

module Router = ReasonReactRouter
