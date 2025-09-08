/*
  Abstract type for representing mdx
  components mostly passed as children to
  the component context API
 */
type mdxComponent

external fromReactElement: React.element => mdxComponent = "%identity"
external toReactElement: mdxComponent => React.element = "%identity"

external arrToReactElement: array<mdxComponent> => React.element = "%identity"

/* Useful for getting the type of a certain mdx component, such as
   "inlineCode" | "p" | "ul" | etc.

   Will return "unknown" if either given element is not an mdx component,
   or if there is no mdxType property found */
let getMdxType: mdxComponent => string = %raw(
  element =>
    "{
      if(element == null || element.props == null) {
        return 'unknown';
      }
      return element.props.mdxType;
    }"
)

module MdxChildren: {
  type unknown
  type t
  type case =
    | String(string)
    | Element(mdxComponent)
    | Array(array<mdxComponent>)
    | Unknown(unknown)
  let classify: t => case
  let getMdxChildren: mdxComponent => t
  let flatten: mdxComponent => array<string>
  let toReactElement: t => React.element
} = {
  type unknown

  @unboxed
  type rec t = Any('a): t

  type case =
    | String(string)
    | Element(mdxComponent)
    | Array(array<mdxComponent>)
    | Unknown(unknown)

  let classify = (Any(v): t): case =>
    if %raw(`function (a) { return  a instanceof Array}`)(v) {
      Array((Obj.magic(v): array<mdxComponent>))
    } else if Js.typeof(v) == "string" {
      String((Obj.magic(v): string))
    } else if Js.typeof(v) == "object" {
      Element((Obj.magic(v): mdxComponent))
    } else {
      Unknown((Obj.magic(v): unknown))
    }

  external toReactElement: t => React.element = "%identity"

  let getMdxChildren: mdxComponent => t = %raw(
    element =>
      "{
      if(element == null || element.props == null || element.props.children == null) {
        return;
      }
      return element.props.children;
    }"
  )

  // Flattens a tree of a mdx component to an array of leaf strings
  let rec flatten = (mdxComp: mdxComponent): array<string> =>
    switch getMdxChildren(mdxComp)->classify {
    | String(str) => [str]
    | Array(arr) => Belt.Array.reduce(arr, [], (acc, next) => Belt.Array.concat(acc, flatten(next)))
    | Element(el) => flatten(el)
    | Unknown(_) => []
    }
}

module Components = {
  type props = {"children": ReasonReact.reactElement}

  type headerProps = {
    "id": string,
    "children": // Used for anchor tags
    React.element,
  }

  // Used for reflection based logic in
  // components such as `code` or `ul`
  // with runtime reflection
  type unknown

  @deriving(abstract)
  type t = {
    /* MDX shortnames for more advanced components */
    @as("Cite") @optional
    cite: React.component<{
      "author": option<string>,
      "children": React.element,
    }>,
    @as("Info") @optional
    info: React.component<props>,
    @as("Warn") @optional
    warn: React.component<props>,
    @as("Intro") @optional
    intro: React.component<props>,
    @as("UrlBox") @optional
    urlBox: React.component<{
      "text": string,
      "href": string,
      "children": MdxChildren.t,
    }>,
    /* Common markdown elements */
    @optional
    p: React.component<props>,
    @optional
    li: React.component<props>,
    @optional
    h1: React.component<props>,
    @optional
    h2: React.component<headerProps>,
    @optional
    h3: React.component<headerProps>,
    @optional
    h4: React.component<headerProps>,
    @optional
    h5: React.component<headerProps>,
    @optional
    ul: React.component<props>,
    @optional
    ol: React.component<props>,
    @optional
    table: React.component<props>,
    @optional
    thead: React.component<props>,
    @optional
    th: React.component<props>,
    @optional
    td: React.component<props>,
    @optional
    blockquote: React.component<props>,
    @optional
    inlineCode: React.component<props>,
    @optional
    hr: React.component<{.}>,
    @optional
    code: React.component<{
      "className": option<string>,
      "metastring": option<string>,
      "children": unknown,
    }>,
    @optional
    pre: React.component<props>,
    @optional
    a: React.component<{
      "children": ReasonReact.reactElement,
      "href": string,
    }>,
  }
}

module Provider = {
  @module("@mdx-js/react") @react.component
  external make: (
    ~components: Components.t,
    ~children: ReasonReact.reactElement=?,
  ) => React.element = "MDXProvider"
}
