## Introduction

JSX V4, supported in the compiler version introduces a new idiomatic record-based representation of components which is incompatible with V3. Because of this, either the entire project or dependencies need to be compiled in V4 mode, or some compatibility features need to be used to mix V3 and V4 in the same project.
The V4 representation is part of the spec, so `@react.component` is effectively just an abbreviation for code that can be written by hand.

## Turn On V4

To build an entire project in V4 mode, including all its dependencies, use the new `"jsx"` configuration in `rescript.json` instead of the old `"reason"`:

```json
"jsx": { "version": 4 }
```

> Note that JSX V4 requires the rescript compiler 10.1 or higher, and `rescript-react` version `0.11` or higher. In addition, `react` version `18.0` is required.

## Configuration And Upgrade

### Dependency-level config

Dependencies inherit the `jsx` configuration of the root project. So if the root project uses V4 then the dependencies are built using V4, and the same for V3.
To build certain dependencies in V3 compatibility mode, whatever the version used in the root project, use `"v3-dependencies"`: the listed dependencies will be built in V3 mode, and in addition `-open ReactV3` is added to the compiler options.

For example, suppose a V3 project uses rescript-react 0.11, which requires compatibility mode if compiled with V3, and that 2 dependencies `"rescript-react-native", "rescript-react-navigation"` only build with compatibility mode. Then the setting will be:

```json
"jsx": {
  "version": 3,
  "v3-dependencies": ["rescript-react-native", "rescript-react-navigation"]
},
"bsc-flags": ["-open ReactV3"]
```

Another example is a V4 project that also uses `"rescript-react-native", "rescript-react-navigation"`. Then the setting will be:

```json
"jsx": {
  "version": 4,
  "v3-dependencies": ["rescript-react-native", "rescript-react-navigation"]
}
```

> Note: do not add @rescript/react to the v3-dependencies, or it will cause a cyclic dependencies error.

### Classic and Automatic Mode

Classic mode generates calls to `React.createElement` just as with V3.

```json
"jsx": {
  "version": 4,
  "mode": "classic"
}
```

Automatic mode is the default and generates calls to `_jsx` functions (similar to TypeScript's `react-jsx` mode)

```json
"jsx": {
  "version": 4,
  "mode": "automatic"
}
```

### File-level config

The top-level attribute `@@jsxConfig` is used to update the `jsx` config for the rest of the file (or until the next config update). Only the values mentioned are updated, the others are left unchanged.

```rescript
@@jsxConfig({ version: 4, mode: "automatic" })

module Wrapper = {
  module R1 = {
    @react.component  // V4 and new _jsx transform
    let make = () => body
  }

  @@jsxConfig({ version: 4, mode: "classic" })

  module R2 = {
    @react.component  // V4 with `React.createElement`
    let make = () => body
  }
}

@@jsxConfig({ version: 3 })

@react.component  // V3
let make = () => body
```

### Migration of V3 components that depend on the internal representation

Some components in existing projects are written in a way that is dependent on the V3 internal representation.
Here are a few examples of how to convert them to V4.

#### `makeProps` does not exist in V4

Rewrite this:

```rescript
// V3
module M = {
  @obj external makeProps: (~msg: 'msg, ~key: string=?, unit) => {"msg": 'msg} = "" // No more makeProps

  let make = (~msg) => {
    <div> {React.string(msg)} </div>
  }
}
```

To this:

```rescript
// V4
module M = {
  type props<'msg> = {msg: 'msg}
  let make = props => <div> {React.string(props.msg)} </div>
}
```

#### `React.Context`

Rewrite this:

```rescript
module Context = {
  let context = React.createContext(() => ())

  module Provider = {
    let provider = React.Context.provider(context)

    @react.component
    let make = (~value, ~children) => {
      React.createElement(provider, {"value": value, "children": children}) // Error
    }
  }
}
```

To this:

```rescript
module Context = {
  let context = React.createContext(() => ())

  module Provider = {
    let make = React.Context.provider(context)
  }
}
```

#### `React.forwardRef`(Discouraged)

`forwardRef` is discouraged, but sometimes used in existing V3 code such as this example:

```rescript
module FancyInput = {
  @react.component
  let make = React.forwardRef((
    ~className=?,
    ~children,
    ref_, // argument
  ) =>
    <div>
      <input
        type_="text"
        ?className
        ref=?{ref_->Js.Nullable.toOption->Belt.Option.map(ReactDOM.Ref.domRef)}
      />
      children
    </div>
  )
}

@react.component
let make = () => {
  let input = React.useRef(Js.Nullable.null)

  <div>
    <FancyInput ref=input> // prop
      <button onClick> {React.string("Click to focus")} </button>
    </FancyInput>
  </div>
}
```

In this example, there is an inconsistency between `ref` as prop and `ref_` as argument. With JSX V4, `ref` is only allowed as an argument.

```rescript
module FancyInput = {
  @react.component
  let make = React.forwardRef((
    ~className=?,
    ~children,
    ref, // only `ref` is allowed
  ) =>
    <div>
      <input
        type_="text"
        ?className
        ref=?{ref->Js.Nullable.toOption->Belt.Option.map(ReactDOM.Ref.domRef)}
      />
      children
    </div>
  )
}

@react.component
let make = () => {
  let input = React.useRef(Js.Nullable.null)

  <div>
    <FancyInput ref=input>
      <button onClick> {React.string("Click to focus")} </button>
    </FancyInput>
  </div>
}
```

## V4 Spec

This is the specification that decribes the two JSX V4 transformations:

- For component definition `@react.component let make = ...`
- For component application `<Foo x y />`

The transformations are optional in that it is possible to write the resulting code manually instead of using them.

### Pre-transformation for component definition

To simplify the description of component definition, a pre-transformation
is used to move `@react.component` to a place where the actual transformations operate.

#### Normal Case

```rescript
@react.component
let make = (~x, ~y, ~z) => body
```

is pre-transformed to

```rescript
let make = @react.component (~x, ~y, ~z) => body
```

#### Forward Ref

```rescript
@react.component
let make = React.forwardRef((~x, ~y, ref) => body)
```

is pre-transformed to

```rescript
let make = React.forwardRef({
  let fn =
    @react.component (~x, ~y) => ref => body
  (props, ref) => fn(props, ref)
})
```

### Transformation for Component Definition

```rescript
@react.component (~x, ~y=3+x, ~z=?) => body
```

is transformed to

```rescript
type props<'x, 'y, 'z> = {x: 'x, y?: 'y, z?: 'z}

({x, ?y, ?z}: props<_, _, _>) => {
  let y = switch y {
  | None => 3 + x
  | Some(y) => y
  }
  body
}
```

> Note: this implicit definition of type `props` means that there cannot be other type definitions of `props` in the same scope, or it will be a compiler error about multiple definitions of the type name.

### Transformation for Component Application

```rescript
<Comp x>
// is transformed to
React.createElement(Comp.make, {x: x})

<Comp x y=7 ?z>
// is transformed to
React.createElement(Comp.make, {x, y: 7, ?z})

<Comp x key="7">
// is transformed to
React.createElement(Comp.make, React.addKeyProp(~key="7", {x: x}))

<Comp x key=?Some("7")>
// is transformed to
React.createElement(Comp.make, React.addKeyProp(~key=?Some("7"), {x: x}))
```

### New experimental automatic mode

The V4 ppx supports [the new jsx transform](https://reactjs.org/blog/2020/09/22/introducing-the-new-jsx-transform.html) of React.js.

The jsx transform only affects component application, but not the definition.

```rescript
<Comp x>
// is transformed to
React.jsx(Comp.make, {x: x})
```

```rescript
<div name="div" />
// is transformed to
ReactDOM.jsx("div", { name: "div" })
```

The props type of dom elements, e.g. `div`, is inferred to `ReactDOM.domProps`.

```rescript
type domProps = {
  key?: string,
  id?: string,
  ...
}
```

### Interface And External

```rescript
@react.component (~x: int, ~y: int=?, ~z: int=?) => React.element

// is transformed to

type props<'x, 'y, 'z> = {x: 'x, y?: 'y, z?: 'z}

props<int, int, int> => React.element
```

Since an external is a function declaration, it follows the same rule.

### Component Name

The convention for names is the same one used in V3: the generated
function has the name of the enclosing module/file.

### Fragments

```rescript
<> comp1 comp2 comp3 </>

// is transformed to

// v4
React.createElement(React.fragment, {children: [comp1, comp2, comp3]})

// v4 @ new jsx transform
React.jsxs(React.jsxFragment, {children: [comp1, comp2, comp3]})
```

### Spread props (new feature)

V4 introduces support for the spread operator for props: `{...p}`.

```rescript
module A = {
  @react.component
  let make = (~x, ~y) => body
}

let p: A.props<_> = {x: "x", y: "y"}

<A {...p}>
<A {...p} x="X">

// not allowed
<A x="X" {...p}>
<A {...p} {...p1}>
```

### Shared props type (new feature)

V4 introduces support to control the definition of the `props` type by passing as argument to `@react.component` the body of the type definition of `props`. The main application is sharing a single type definition across several components. Here are a few examples:

```rescript
type sharedprops<'x, 'y> = {x: 'x, y: 'y, z:string}

module C1 = {
  @react.component(:sharedProps<'a, 'b>)
  let make = (~x, ~y) => React.string(x ++ y ++ z)
}

module C2 = {
  @react.component(:sharedProps<string, 'b>)
  let make = (~x, ~y) => React.string(x ++ y ++ z)
}

module C3 = {
  type myProps = sharedProps<int, int>
  @react.component(:myProps)
  let make = (~x, ~y) => React.int(x + y)
}
```

The generated code (some details removed) looks like this:

```rescript
@@jsxConfig({version: 4, mode: "classic"})

type sharedprops<'x, 'y> = {x: 'x, y: 'y, z: string}

module C1 = {
  type props<'a, 'b> = sharedProps<'a, 'b>
  let make = ({x, y, _}: props<_>) => React.string(x ++ y ++ z)
}

module C2 = {
  type props<'b> = sharedProps<string, 'b>
  let make = ({x, y, _}: props<_>) => React.string(x ++ y ++ z)
}

module C3 = {
  type myProps = sharedProps<int, int>
  type props = myProps
  let make = ({x, y, _}: props) => React.int(x + y)
}
```
