/* First time reading a ReScript file? */
/* `external` is the foreign function call in OCaml. */
/* here we're saying `I guarantee that on the JS side, we have a `render` function in the module "react-dom"
 that takes in a reactElement, a dom element, and returns unit (nothing) */
/* It's like `let`, except you're pointing the implementation to the JS side. The compiler will inline these
 calls and add the appropriate `require("react-dom")` in the file calling this `render` */

// Helper so that ReactDOM itself doesn't bring any runtime
@val @return(nullable)
external querySelector: string => option<Dom.element> = "document.querySelector"

module Client = {
  module Root = {
    type t

    @send external render: (t, React.element) => unit = "render"

    @send external unmount: (t, unit) => unit = "unmount"
  }

  @module("react-dom/client")
  external createRoot: Dom.element => Root.t = "createRoot"

  @module("react-dom/client")
  external hydrateRoot: (Dom.element, React.element) => Root.t = "hydrateRoot"
}

// Very rudimentary form data bindings
module FormData = {
  type t
  type file

  type formValue =
    | String(string)
    | File(file)

  @new external make: unit => t = "FormData"

  @send external append: (t, string, ~filename: string=?) => unit = "append"
  @send external delete: (t, string) => unit = "delete"
  @return(nullable) @send external getUnsafe: (t, string) => option<'a> = "get"
  @send external getAllUnsafe: (t, string) => array<'a> = "getAll"

  let getString = (formData, name) => {
    switch formData->getUnsafe(name) {
    | Some(value) => Js.typeof(value) === "string" ? Some(value) : None
    | _ => None
    }
  }

  external _asFile: 'a => file = "%identity"

  let getFile = (formData, name) => {
    switch formData->getUnsafe(name) {
    | Some(value) => Js.typeof(value) === "string" ? None : Some(value->_asFile)
    | _ => None
    }
  }

  let getAll = (t, string) => {
    t
    ->getAllUnsafe(string)
    ->Js.Array2.map(value => {
      Js.typeof(value) === "string" ? String(value) : File(value->_asFile)
    })
  }

  @send external set: (string, string) => unit = "set"
  @send external has: string => bool = "has"
  // @send external keys: t => Iterator.t<string> = "keys";
  // @send external values: t => Iterator.t<value> = "values";
}

@module("react-dom")
external createPortal: (React.element, Dom.element) => React.element = "createPortal"

external domElementToObj: Dom.element => {..} = "%identity"

type style = ReactDOMStyle.t

type domRef = JsxDOM.domRef

module Ref = {
  type t = domRef
  type currentDomRef = React.ref<Js.nullable<Dom.element>>
  type callbackDomRef = Js.nullable<Dom.element> => option<unit => unit>

  external domRef: currentDomRef => domRef = "%identity"
  external callbackDomRef: callbackDomRef => domRef = "%identity"
}

// Hooks

type formStatus<'state> = {
  /** If true, this means the parent <form> is pending submission. Otherwise, false. */
  pending: bool,
  /** An object implementing the FormData interface that contains the data the parent <form> is submitting. If there is no active submission or no parent <form>, it will be null. */
  data: FormData.t,
  /** This represents whether the parent <form> is submitting with either a GET or POST HTTP method. By default, a <form> will use the GET method and can be specified by the method property. */
  method: [#get | #post],
  /** A reference to the function passed to the action prop on the parent <form>. If there is no parent <form>, the property is null. If there is a URI value provided to the action prop, or no action prop specified, status.action will be null. */
  action: React.action<'state, FormData.t>,
}

external formAction: React.formAction<FormData.t> => string = "%identity"

/** `useFormStatus` is a Hook that gives you status information of the last form submission. */
@module("react-dom")
external useFormStatus: unit => formStatus<'state> = "useFormStatus"

// Resource Preloading APIs

/** The CORS policy to use. */
type crossOrigin = [
  | #anonymous
  | #"use-credentials"
]

/** The Referrer header to send when fetching. */
type referrerPolicy = [
  | #"referrer-when-downgrade"
  | #"no-referrer"
  | #origin
  | #"origin-when-cross-origin"
  | #"unsafe-url"
]

/** Suggests a relative priority for fetching the resource. */
type fetchPriority = [#auto | #high | #low]

/** `prefetchDNS` lets you eagerly look up the IP of a server that you expect to load resources from. */
@module("react-dom")
external prefetchDNS: string => unit = "prefetchDNS"

/** `preconnect` lets you eagerly connect to a server that you expect to load resources from. */
@module("react-dom")
external preconnect: string => unit = "preconnect"

type preloadOptions = {
  /** The type of resource. */
  @as("as")
  as_: [
    | #audio
    | #document
    | #embed
    | #fetch
    | #font
    | #image
    | #object
    | #script
    | #style
    | #track
    | #video
    | #worker
  ],
  /** The CORS policy to use. It is required when as is set to "fetch". */
  crossOrigin?: crossOrigin,
  /** The Referrer header to send when fetching. */
  referrerPolicy?: referrerPolicy,
  /** A cryptographic hash of the resource, to verify its authenticity. */
  integrity?: string,
  /** The MIME type of the resource. */
  @as("type")
  type_?: string,
  /** A cryptographic nonce to allow the resource when using a strict Content Security Policy. */
  nonce?: string,
  /** Suggests a relative priority for fetching the resource. */
  fetchPriority?: fetchPriority,
  /** For use only with as: "image". Specifies the source set of the image. */
  imageSrcSet?: string,
  /** For use only with as: "image". Specifies the sizes of the image. */
  imageSizes?: string,
}

/** `preload` lets you eagerly fetch a resource such as a stylesheet, font, or external script that you expect to use. */
@module("react-dom")
external preload: (string, preloadOptions) => unit = "preload"

type preloadModuleOptions = {
  /** The type of resource. */
  @as("as")
  as_: [#script],
  /** The CORS policy to use. It is required when as is set to "fetch". */
  crossOrigin?: crossOrigin,
  /** A cryptographic hash of the resource, to verify its authenticity. */
  integrity?: string,
  /** A cryptographic nonce to allow the resource when using a strict Content Security Policy. */
  nonce?: string,
}

/** `preloadModule` lets you eagerly fetch an ESM module that you expect to use. */
@module("react-dom")
external preloadModule: (string, preloadModuleOptions) => unit = "preloadModule"

type preinitOptions = {
  /** The type of resource. */
  @as("as")
  as_: [#script | #style],
  /** Required with stylesheets. Says where to insert the stylesheet relative to others. Stylesheets with higher precedence can override those with lower precedence. */
  precedence?: [#reset | #low | #medium | #high],
  /** The CORS policy to use. It is required when as is set to "fetch". */
  crossOrigin?: crossOrigin,
  /** The Referrer header to send when fetching. */
  referrerPolicy?: referrerPolicy,
  /** A cryptographic hash of the resource, to verify its authenticity. */
  integrity?: string,
  nonce?: string,
  /** Suggests a relative priority for fetching the resource. */
  fetchPriority?: fetchPriority,
}

/** `preinit` lets you eagerly fetch and evaluate a stylesheet or external script. */
@module("react-dom")
external preinit: (string, preinitOptions) => unit = "preinit"

/** To preinit an ESM module, call the `preinitModule` function from react-dom. */
@module("react-dom")
external preinitModule: (string, preloadModuleOptions) => unit = "preinitModule"

// Runtime

type domProps = JsxDOM.domProps

@variadic @module("react")
external createElement: (string, ~props: domProps=?, array<React.element>) => React.element =
  "createElement"

@variadic @module("react")
external createDOMElementVariadic: (
  string,
  ~props: domProps=?,
  array<React.element>,
) => React.element = "createElement"

external someElement: React.element => option<React.element> = "%identity"

@module("react/jsx-runtime")
external jsx: (string, JsxDOM.domProps) => Jsx.element = "jsx"

@module("react/jsx-runtime")
external jsxKeyed: (string, JsxDOM.domProps, ~key: string=?, @ignore unit) => Jsx.element = "jsx"

@module("react/jsx-runtime")
external jsxs: (string, JsxDOM.domProps) => Jsx.element = "jsxs"

@module("react/jsx-runtime")
external jsxsKeyed: (string, JsxDOM.domProps, ~key: string=?, @ignore unit) => Jsx.element = "jsxs"

module Style = ReactDOMStyle
