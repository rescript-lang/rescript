/* First time reading an OCaml/Reason/BuckleScript file? */
/* `external` is the foreign function call in OCaml. */
/* here we're saying `I guarantee that on the JS side, we have a `render` function in the module "react-dom"
 that takes in a reactElement, a dom element, and returns unit (nothing) */
/* It's like `let`, except you're pointing the implementation to the JS side. The compiler will inline these
 calls and add the appropriate `require("react-dom")` in the file calling this `render` */
@val @module("react-dom")
external render: (React.element, Dom.element) => unit = "render"

@val
external _getElementsByClassName: string => array<Dom.element> = "document.getElementsByClassName"

@val @return(nullable)
external _getElementById: string => option<Dom.element> = "document.getElementById"

let renderToElementWithClassName = (reactElement, className) =>
  switch _getElementsByClassName(className) {
  | [] =>
    raise(
      Invalid_argument(
        "ReactDOMRe.renderToElementWithClassName: no element of class " ++
        (className ++
        " found in the HTML."),
      ),
    )
  | elements => render(reactElement, Array.unsafe_get(elements, 0))
  }

let renderToElementWithId = (reactElement, id) =>
  switch _getElementById(id) {
  | None =>
    raise(
      Invalid_argument(
        "ReactDOMRe.renderToElementWithId : no element of id " ++ (id ++ " found in the HTML."),
      ),
    )
  | Some(element) => render(reactElement, element)
  }

@val @module("react-dom")
external hydrate: (React.element, Dom.element) => unit = "hydrate"

let hydrateToElementWithClassName = (reactElement, className) =>
  switch _getElementsByClassName(className) {
  | [] =>
    raise(
      Invalid_argument(
        "ReactDOMRe.hydrateToElementWithClassName: no element of class " ++
        (className ++
        " found in the HTML."),
      ),
    )
  | elements => hydrate(reactElement, Array.unsafe_get(elements, 0))
  }

let hydrateToElementWithId = (reactElement, id) =>
  switch _getElementById(id) {
  | None =>
    raise(
      Invalid_argument(
        "ReactDOMRe.hydrateToElementWithId : no element of id " ++ (id ++ " found in the HTML."),
      ),
    )
  | Some(element) => hydrate(reactElement, element)
  }

@val @module("react-dom")
external createPortal: (React.element, Dom.element) => React.element = "createPortal"

@val @module("react-dom")
external unmountComponentAtNode: Dom.element => unit = "unmountComponentAtNode"

@val @module("react-dom")
external findDOMNode: ReasonReact.reactRef => Dom.element = "findDOMNode"

external domElementToObj: Dom.element => {..} = "%identity"

type style

type domRef

module Ref = {
  type t = domRef
  type currentDomRef = React.Ref.t<Js.nullable<Dom.element>>
  type callbackDomRef = Js.nullable<Dom.element> => unit

  external domRef: currentDomRef => domRef = "%identity"
  external callbackDomRef: callbackDomRef => domRef = "%identity"
}

/* This list isn't exhaustive. We'll add more as we go. */
/*
 * Watch out! There are two props types and the only difference is the type of ref.
 * Please keep in sync.
 */
@deriving(abstract)
type domProps = {
  @optional
  key: string,
  @optional
  ref: domRef,
  /* accessibility */
  /* https://www.w3.org/TR/wai-aria-1.1/ */
  /* https://accessibilityresources.org/<aria-tag> is a great resource for these */
  /* [@optional] [@as "aria-current"] ariaCurrent: page|step|location|date|time|true|false, */
  @optional @as("aria-details")
  ariaDetails: string,
  @optional @as("aria-disabled")
  ariaDisabled: bool,
  @optional @as("aria-hidden")
  ariaHidden: bool,
  /* [@optional] [@as "aria-invalid"] ariaInvalid: grammar|false|spelling|true, */
  @optional @as("aria-keyshortcuts")
  ariaKeyshortcuts: string,
  @optional @as("aria-label")
  ariaLabel: string,
  @optional @as("aria-roledescription")
  ariaRoledescription: string,
  /* Widget Attributes */
  /* [@optional] [@as "aria-autocomplete"] ariaAutocomplete: inline|list|both|none, */
  /* [@optional] [@as "aria-checked"] ariaChecked: true|false|mixed, /* https://www.w3.org/TR/wai-aria-1.1/#valuetype_tristate */ */
  @optional @as("aria-expanded")
  ariaExpanded: bool,
  /* [@optional] [@as "aria-haspopup"] ariaHaspopup: false|true|menu|listbox|tree|grid|dialog, */
  @optional @as("aria-level")
  ariaLevel: int,
  @optional @as("aria-modal")
  ariaModal: bool,
  @optional @as("aria-multiline")
  ariaMultiline: bool,
  @optional @as("aria-multiselectable")
  ariaMultiselectable: bool,
  /* [@optional] [@as "aria-orientation"] ariaOrientation: horizontal|vertical|undefined, */
  @optional @as("aria-placeholder")
  ariaPlaceholder: string,
  /* [@optional] [@as "aria-pressed"] ariaPressed: true|false|mixed, /* https://www.w3.org/TR/wai-aria-1.1/#valuetype_tristate */ */
  @optional @as("aria-readonly")
  ariaReadonly: bool,
  @optional @as("aria-required")
  ariaRequired: bool,
  @optional @as("aria-selected")
  ariaSelected: bool,
  @optional @as("aria-sort")
  ariaSort: string,
  @optional @as("aria-valuemax")
  ariaValuemax: float,
  @optional @as("aria-valuemin")
  ariaValuemin: float,
  @optional @as("aria-valuenow")
  ariaValuenow: float,
  @optional @as("aria-valuetext")
  ariaValuetext: string,
  /* Live Region Attributes */
  @optional @as("aria-atomic")
  ariaAtomic: bool,
  @optional @as("aria-busy")
  ariaBusy: bool,
  /* [@optional] [@as "aria-live"] ariaLive: off|polite|assertive|rude, */
  @optional @as("aria-relevant")
  ariaRelevant: string,
  /* Drag-and-Drop Attributes */
  /* [@optional] [@as "aria-dropeffect"] ariaDropeffect: copy|move|link|execute|popup|none, */
  @optional @as("aria-grabbed")
  ariaGrabbed: bool,
  /* Relationship Attributes */
  @optional @as("aria-activedescendant")
  ariaActivedescendant: string,
  @optional @as("aria-colcount")
  ariaColcount: int,
  @optional @as("aria-colindex")
  ariaColindex: int,
  @optional @as("aria-colspan")
  ariaColspan: int,
  @optional @as("aria-controls")
  ariaControls: string,
  @optional @as("aria-describedby")
  ariaDescribedby: string,
  @optional @as("aria-errormessage")
  ariaErrormessage: string,
  @optional @as("aria-flowto")
  ariaFlowto: string,
  @optional @as("aria-labelledby")
  ariaLabelledby: string,
  @optional @as("aria-owns")
  ariaOwns: string,
  @optional @as("aria-posinset")
  ariaPosinset: int,
  @optional @as("aria-rowcount")
  ariaRowcount: int,
  @optional @as("aria-rowindex")
  ariaRowindex: int,
  @optional @as("aria-rowspan")
  ariaRowspan: int,
  @optional @as("aria-setsize")
  ariaSetsize: int,
  /* react textarea/input */
  @optional
  defaultChecked: bool,
  @optional
  defaultValue: string,
  /* global html attributes */
  @optional
  accessKey: string,
  @optional
  className: string /* substitute for "class" */,
  @optional
  contentEditable: bool,
  @optional
  contextMenu: string,
  @optional
  dir: string /* "ltr", "rtl" or "auto" */,
  @optional
  draggable: bool,
  @optional
  hidden: bool,
  @optional
  id: string,
  @optional
  lang: string,
  @optional
  role: string /* ARIA role */,
  @optional
  style: style,
  @optional
  spellCheck: bool,
  @optional
  tabIndex: int,
  @optional
  title: string,
  /* html5 microdata */
  @optional
  itemID: string,
  @optional
  itemProp: string,
  @optional
  itemRef: string,
  @optional
  itemScope: bool,
  @optional
  itemType: string /* uri */,
  /* tag-specific html attributes */
  @optional
  accept: string,
  @optional
  acceptCharset: string,
  @optional
  action: string /* uri */,
  @optional
  allowFullScreen: bool,
  @optional
  alt: string,
  @optional
  async: bool,
  @optional
  autoComplete: string /* has a fixed, but large-ish, set of possible values */,
  @optional
  autoFocus: bool,
  @optional
  autoPlay: bool,
  @optional
  challenge: string,
  @optional
  charSet: string,
  @optional
  checked: bool,
  @optional
  cite: string /* uri */,
  @optional
  crossorigin: bool,
  @optional
  cols: int,
  @optional
  colSpan: int,
  @optional
  content: string,
  @optional
  controls: bool,
  @optional
  coords: string /* set of values specifying the coordinates of a region */,
  @optional
  data: string /* uri */,
  @optional
  dateTime: string /* "valid date string with optional time" */,
  @optional
  default: bool,
  @optional
  defer: bool,
  @optional
  disabled: bool,
  @optional
  download: string /* should really be either a boolean, signifying presence, or a string */,
  @optional
  encType: string /* "application/x-www-form-urlencoded", "multipart/form-data" or "text/plain" */,
  @optional
  form: string,
  @optional
  formAction: string /* uri */,
  @optional
  formTarget: string /* "_blank", "_self", etc. */,
  @optional
  formMethod: string /* "post", "get", "put" */,
  @optional
  headers: string,
  @optional
  height: string /* in html5 this can only be a number, but in html4 it can ba a percentage as well */,
  @optional
  high: int,
  @optional
  href: string /* uri */,
  @optional
  hrefLang: string,
  @optional
  htmlFor: string /* substitute for "for" */,
  @optional
  httpEquiv: string /* has a fixed set of possible values */,
  @optional
  icon: string /* uri? */,
  @optional
  inputMode: string /* "verbatim", "latin", "numeric", etc. */,
  @optional
  integrity: string,
  @optional
  keyType: string,
  @optional
  kind: string /* has a fixed set of possible values */,
  @optional
  label: string,
  @optional
  list: string,
  @optional
  loop: bool,
  @optional
  low: int,
  @optional
  manifest: string /* uri */,
  @optional
  max: string /* should be int or Js.Date.t */,
  @optional
  maxLength: int,
  @optional
  media: string /* a valid media query */,
  @optional
  mediaGroup: string,
  @optional
  method: string /* "post" or "get" */,
  @optional
  min: int,
  @optional
  minLength: int,
  @optional
  multiple: bool,
  @optional
  muted: bool,
  @optional
  name: string,
  @optional
  nonce: string,
  @optional
  noValidate: bool,
  @optional @as("open")
  open_: bool /* use this one. Previous one is deprecated */,
  @optional
  optimum: int,
  @optional
  pattern: string /* valid Js RegExp */,
  @optional
  placeholder: string,
  @optional
  poster: string /* uri */,
  @optional
  preload: string /* "none", "metadata" or "auto" (and "" as a synonym for "auto") */,
  @optional
  radioGroup: string,
  @optional
  readOnly: bool,
  @optional
  rel: string /* a space- or comma-separated (depending on the element) list of a fixed set of "link types" */,
  @optional
  required: bool,
  @optional
  reversed: bool,
  @optional
  rows: int,
  @optional
  rowSpan: int,
  @optional
  sandbox: string /* has a fixed set of possible values */,
  @optional
  scope: string /* has a fixed set of possible values */,
  @optional
  scoped: bool,
  @optional
  scrolling: string /* html4 only, "auto", "yes" or "no" */,
  /* seamless - supported by React, but removed from the html5 spec */
  @optional
  selected: bool,
  @optional
  shape: string,
  @optional
  size: int,
  @optional
  sizes: string,
  @optional
  span: int,
  @optional
  src: string /* uri */,
  @optional
  srcDoc: string,
  @optional
  srcLang: string,
  @optional
  srcSet: string,
  @optional
  start: int,
  @optional
  step: float,
  @optional
  summary: string /* deprecated */,
  @optional
  target: string,
  @optional @as("type")
  type_: string /* has a fixed but large-ish set of possible values */ /* use this one. Previous one is deprecated */,
  @optional
  useMap: string,
  @optional
  value: string,
  @optional
  width: string /* in html5 this can only be a number, but in html4 it can ba a percentage as well */,
  @optional
  wrap: string /* "hard" or "soft" */,
  /* Clipboard events */
  @optional
  onCopy: ReactEvent.Clipboard.t => unit,
  @optional
  onCut: ReactEvent.Clipboard.t => unit,
  @optional
  onPaste: ReactEvent.Clipboard.t => unit,
  /* Composition events */
  @optional
  onCompositionEnd: ReactEvent.Composition.t => unit,
  @optional
  onCompositionStart: ReactEvent.Composition.t => unit,
  @optional
  onCompositionUpdate: ReactEvent.Composition.t => unit,
  /* Keyboard events */
  @optional
  onKeyDown: ReactEvent.Keyboard.t => unit,
  @optional
  onKeyPress: ReactEvent.Keyboard.t => unit,
  @optional
  onKeyUp: ReactEvent.Keyboard.t => unit,
  /* Focus events */
  @optional
  onFocus: ReactEvent.Focus.t => unit,
  @optional
  onBlur: ReactEvent.Focus.t => unit,
  /* Form events */
  @optional
  onChange: ReactEvent.Form.t => unit,
  @optional
  onInput: ReactEvent.Form.t => unit,
  @optional
  onSubmit: ReactEvent.Form.t => unit,
  /* Mouse events */
  @optional
  onClick: ReactEvent.Mouse.t => unit,
  @optional
  onContextMenu: ReactEvent.Mouse.t => unit,
  @optional
  onDoubleClick: ReactEvent.Mouse.t => unit,
  @optional
  onDrag: ReactEvent.Mouse.t => unit,
  @optional
  onDragEnd: ReactEvent.Mouse.t => unit,
  @optional
  onDragEnter: ReactEvent.Mouse.t => unit,
  @optional
  onDragExit: ReactEvent.Mouse.t => unit,
  @optional
  onDragLeave: ReactEvent.Mouse.t => unit,
  @optional
  onDragOver: ReactEvent.Mouse.t => unit,
  @optional
  onDragStart: ReactEvent.Mouse.t => unit,
  @optional
  onDrop: ReactEvent.Mouse.t => unit,
  @optional
  onMouseDown: ReactEvent.Mouse.t => unit,
  @optional
  onMouseEnter: ReactEvent.Mouse.t => unit,
  @optional
  onMouseLeave: ReactEvent.Mouse.t => unit,
  @optional
  onMouseMove: ReactEvent.Mouse.t => unit,
  @optional
  onMouseOut: ReactEvent.Mouse.t => unit,
  @optional
  onMouseOver: ReactEvent.Mouse.t => unit,
  @optional
  onMouseUp: ReactEvent.Mouse.t => unit,
  /* Selection events */
  @optional
  onSelect: ReactEvent.Selection.t => unit,
  /* Touch events */
  @optional
  onTouchCancel: ReactEvent.Touch.t => unit,
  @optional
  onTouchEnd: ReactEvent.Touch.t => unit,
  @optional
  onTouchMove: ReactEvent.Touch.t => unit,
  @optional
  onTouchStart: ReactEvent.Touch.t => unit,
  /* UI events */
  @optional
  onScroll: ReactEvent.UI.t => unit,
  /* Wheel events */
  @optional
  onWheel: ReactEvent.Wheel.t => unit,
  /* Media events */
  @optional
  onAbort: ReactEvent.Media.t => unit,
  @optional
  onCanPlay: ReactEvent.Media.t => unit,
  @optional
  onCanPlayThrough: ReactEvent.Media.t => unit,
  @optional
  onDurationChange: ReactEvent.Media.t => unit,
  @optional
  onEmptied: ReactEvent.Media.t => unit,
  @optional
  onEncrypetd: ReactEvent.Media.t => unit,
  @optional
  onEnded: ReactEvent.Media.t => unit,
  @optional
  onError: ReactEvent.Media.t => unit,
  @optional
  onLoadedData: ReactEvent.Media.t => unit,
  @optional
  onLoadedMetadata: ReactEvent.Media.t => unit,
  @optional
  onLoadStart: ReactEvent.Media.t => unit,
  @optional
  onPause: ReactEvent.Media.t => unit,
  @optional
  onPlay: ReactEvent.Media.t => unit,
  @optional
  onPlaying: ReactEvent.Media.t => unit,
  @optional
  onProgress: ReactEvent.Media.t => unit,
  @optional
  onRateChange: ReactEvent.Media.t => unit,
  @optional
  onSeeked: ReactEvent.Media.t => unit,
  @optional
  onSeeking: ReactEvent.Media.t => unit,
  @optional
  onStalled: ReactEvent.Media.t => unit,
  @optional
  onSuspend: ReactEvent.Media.t => unit,
  @optional
  onTimeUpdate: ReactEvent.Media.t => unit,
  @optional
  onVolumeChange: ReactEvent.Media.t => unit,
  @optional
  onWaiting: ReactEvent.Media.t => unit,
  /* Image events */
  @optional
  onLoad: ReactEvent.Image.t => unit /* duplicate */ /* ~onError: ReactEvent.Image.t => unit=?, */,
  /* Animation events */
  @optional
  onAnimationStart: ReactEvent.Animation.t => unit,
  @optional
  onAnimationEnd: ReactEvent.Animation.t => unit,
  @optional
  onAnimationIteration: ReactEvent.Animation.t => unit,
  /* Transition events */
  @optional
  onTransitionEnd: ReactEvent.Transition.t => unit,
  /* svg */
  @optional
  accentHeight: string,
  @optional
  accumulate: string,
  @optional
  additive: string,
  @optional
  alignmentBaseline: string,
  @optional
  allowReorder: string,
  @optional
  alphabetic: string,
  @optional
  amplitude: string,
  @optional
  arabicForm: string,
  @optional
  ascent: string,
  @optional
  attributeName: string,
  @optional
  attributeType: string,
  @optional
  autoReverse: string,
  @optional
  azimuth: string,
  @optional
  baseFrequency: string,
  @optional
  baseProfile: string,
  @optional
  baselineShift: string,
  @optional
  bbox: string,
  @optional @as("begin")
  begin_: string /* use this one. Previous one is deprecated */,
  @optional
  bias: string,
  @optional
  by: string,
  @optional
  calcMode: string,
  @optional
  capHeight: string,
  @optional
  clip: string,
  @optional
  clipPath: string,
  @optional
  clipPathUnits: string,
  @optional
  clipRule: string,
  @optional
  colorInterpolation: string,
  @optional
  colorInterpolationFilters: string,
  @optional
  colorProfile: string,
  @optional
  colorRendering: string,
  @optional
  contentScriptType: string,
  @optional
  contentStyleType: string,
  @optional
  cursor: string,
  @optional
  cx: string,
  @optional
  cy: string,
  @optional
  d: string,
  @optional
  decelerate: string,
  @optional
  descent: string,
  @optional
  diffuseConstant: string,
  @optional
  direction: string,
  @optional
  display: string,
  @optional
  divisor: string,
  @optional
  dominantBaseline: string,
  @optional
  dur: string,
  @optional
  dx: string,
  @optional
  dy: string,
  @optional
  edgeMode: string,
  @optional
  elevation: string,
  @optional
  enableBackground: string,
  @optional @as("end")
  end_: string /* use this one. Previous one is deprecated */,
  @optional
  exponent: string,
  @optional
  externalResourcesRequired: string,
  @optional
  fill: string,
  @optional
  fillOpacity: string,
  @optional
  fillRule: string,
  @optional
  filter: string,
  @optional
  filterRes: string,
  @optional
  filterUnits: string,
  @optional
  floodColor: string,
  @optional
  floodOpacity: string,
  @optional
  focusable: string,
  @optional
  fontFamily: string,
  @optional
  fontSize: string,
  @optional
  fontSizeAdjust: string,
  @optional
  fontStretch: string,
  @optional
  fontStyle: string,
  @optional
  fontVariant: string,
  @optional
  fontWeight: string,
  @optional
  fomat: string,
  @optional
  from: string,
  @optional
  fx: string,
  @optional
  fy: string,
  @optional
  g1: string,
  @optional
  g2: string,
  @optional
  glyphName: string,
  @optional
  glyphOrientationHorizontal: string,
  @optional
  glyphOrientationVertical: string,
  @optional
  glyphRef: string,
  @optional
  gradientTransform: string,
  @optional
  gradientUnits: string,
  @optional
  hanging: string,
  @optional
  horizAdvX: string,
  @optional
  horizOriginX: string,
  @optional
  ideographic: string,
  @optional
  imageRendering: string,
  @optional @as("in")
  in_: string /* use this one. Previous one is deprecated */,
  @optional
  in2: string,
  @optional
  intercept: string,
  @optional
  k: string,
  @optional
  k1: string,
  @optional
  k2: string,
  @optional
  k3: string,
  @optional
  k4: string,
  @optional
  kernelMatrix: string,
  @optional
  kernelUnitLength: string,
  @optional
  kerning: string,
  @optional
  keyPoints: string,
  @optional
  keySplines: string,
  @optional
  keyTimes: string,
  @optional
  lengthAdjust: string,
  @optional
  letterSpacing: string,
  @optional
  lightingColor: string,
  @optional
  limitingConeAngle: string,
  @optional
  local: string,
  @optional
  markerEnd: string,
  @optional
  markerHeight: string,
  @optional
  markerMid: string,
  @optional
  markerStart: string,
  @optional
  markerUnits: string,
  @optional
  markerWidth: string,
  @optional
  mask: string,
  @optional
  maskContentUnits: string,
  @optional
  maskUnits: string,
  @optional
  mathematical: string,
  @optional
  mode: string,
  @optional
  numOctaves: string,
  @optional
  offset: string,
  @optional
  opacity: string,
  @optional
  operator: string,
  @optional
  order: string,
  @optional
  orient: string,
  @optional
  orientation: string,
  @optional
  origin: string,
  @optional
  overflow: string,
  @optional
  overflowX: string,
  @optional
  overflowY: string,
  @optional
  overlinePosition: string,
  @optional
  overlineThickness: string,
  @optional
  paintOrder: string,
  @optional
  panose1: string,
  @optional
  pathLength: string,
  @optional
  patternContentUnits: string,
  @optional
  patternTransform: string,
  @optional
  patternUnits: string,
  @optional
  pointerEvents: string,
  @optional
  points: string,
  @optional
  pointsAtX: string,
  @optional
  pointsAtY: string,
  @optional
  pointsAtZ: string,
  @optional
  preserveAlpha: string,
  @optional
  preserveAspectRatio: string,
  @optional
  primitiveUnits: string,
  @optional
  r: string,
  @optional
  radius: string,
  @optional
  refX: string,
  @optional
  refY: string,
  @optional
  renderingIntent: string,
  @optional
  repeatCount: string,
  @optional
  repeatDur: string,
  @optional
  requiredExtensions: string,
  @optional
  requiredFeatures: string,
  @optional
  restart: string,
  @optional
  result: string,
  @optional
  rotate: string,
  @optional
  rx: string,
  @optional
  ry: string,
  @optional
  scale: string,
  @optional
  seed: string,
  @optional
  shapeRendering: string,
  @optional
  slope: string,
  @optional
  spacing: string,
  @optional
  specularConstant: string,
  @optional
  specularExponent: string,
  @optional
  speed: string,
  @optional
  spreadMethod: string,
  @optional
  startOffset: string,
  @optional
  stdDeviation: string,
  @optional
  stemh: string,
  @optional
  stemv: string,
  @optional
  stitchTiles: string,
  @optional
  stopColor: string,
  @optional
  stopOpacity: string,
  @optional
  strikethroughPosition: string,
  @optional
  strikethroughThickness: string,
  @optional
  string: string,
  @optional
  stroke: string,
  @optional
  strokeDasharray: string,
  @optional
  strokeDashoffset: string,
  @optional
  strokeLinecap: string,
  @optional
  strokeLinejoin: string,
  @optional
  strokeMiterlimit: string,
  @optional
  strokeOpacity: string,
  @optional
  strokeWidth: string,
  @optional
  surfaceScale: string,
  @optional
  systemLanguage: string,
  @optional
  tableValues: string,
  @optional
  targetX: string,
  @optional
  targetY: string,
  @optional
  textAnchor: string,
  @optional
  textDecoration: string,
  @optional
  textLength: string,
  @optional
  textRendering: string,
  @optional @as("to")
  to_: string /* use this one. Previous one is deprecated */,
  @optional
  transform: string,
  @optional
  u1: string,
  @optional
  u2: string,
  @optional
  underlinePosition: string,
  @optional
  underlineThickness: string,
  @optional
  unicode: string,
  @optional
  unicodeBidi: string,
  @optional
  unicodeRange: string,
  @optional
  unitsPerEm: string,
  @optional
  vAlphabetic: string,
  @optional
  vHanging: string,
  @optional
  vIdeographic: string,
  @optional
  vMathematical: string,
  @optional
  values: string,
  @optional
  vectorEffect: string,
  @optional
  version: string,
  @optional
  vertAdvX: string,
  @optional
  vertAdvY: string,
  @optional
  vertOriginX: string,
  @optional
  vertOriginY: string,
  @optional
  viewBox: string,
  @optional
  viewTarget: string,
  @optional
  visibility: string,
  /* width::string? => */
  @optional
  widths: string,
  @optional
  wordSpacing: string,
  @optional
  writingMode: string,
  @optional
  x: string,
  @optional
  x1: string,
  @optional
  x2: string,
  @optional
  xChannelSelector: string,
  @optional
  xHeight: string,
  @optional
  xlinkActuate: string,
  @optional
  xlinkArcrole: string,
  @optional
  xlinkHref: string,
  @optional
  xlinkRole: string,
  @optional
  xlinkShow: string,
  @optional
  xlinkTitle: string,
  @optional
  xlinkType: string,
  @optional
  xmlns: string,
  @optional
  xmlnsXlink: string,
  @optional
  xmlBase: string,
  @optional
  xmlLang: string,
  @optional
  xmlSpace: string,
  @optional
  y: string,
  @optional
  y1: string,
  @optional
  y2: string,
  @optional
  yChannelSelector: string,
  @optional
  z: string,
  @optional
  zoomAndPan: string,
  /* RDFa */
  @optional
  about: string,
  @optional
  datatype: string,
  @optional
  inlist: string,
  @optional
  prefix: string,
  @optional
  property: string,
  @optional
  resource: string,
  @optional
  typeof: string,
  @optional
  vocab: string,
  /* react-specific */
  @optional
  dangerouslySetInnerHTML: {"__html": string},
  @optional
  suppressContentEditableWarning: bool,
}

@variadic @module("react")
external createDOMElementVariadic: (
  string,
  ~props: domProps=?,
  array<React.element>,
) => React.element = "createElement"

/* This list isn't exhaustive. We'll add more as we go. */
/*
 * Watch out! There are two props types and the only difference is the type of ref.
 * Please keep in sync.
 */
@deriving(abstract)
type props = {
  @optional
  key: string,
  @optional
  ref: Js.nullable<Dom.element> => unit,
  /* accessibility */
  /* https://www.w3.org/TR/wai-aria-1.1/ */
  /* https://accessibilityresources.org/<aria-tag> is a great resource for these */
  /* [@optional] [@as "aria-current"] ariaCurrent: page|step|location|date|time|true|false, */
  @optional @as("aria-details")
  ariaDetails: string,
  @optional @as("aria-disabled")
  ariaDisabled: bool,
  @optional @as("aria-hidden")
  ariaHidden: bool,
  /* [@optional] [@as "aria-invalid"] ariaInvalid: grammar|false|spelling|true, */
  @optional @as("aria-keyshortcuts")
  ariaKeyshortcuts: string,
  @optional @as("aria-label")
  ariaLabel: string,
  @optional @as("aria-roledescription")
  ariaRoledescription: string,
  /* Widget Attributes */
  /* [@optional] [@as "aria-autocomplete"] ariaAutocomplete: inline|list|both|none, */
  /* [@optional] [@as "aria-checked"] ariaChecked: true|false|mixed, /* https://www.w3.org/TR/wai-aria-1.1/#valuetype_tristate */ */
  @optional @as("aria-expanded")
  ariaExpanded: bool,
  /* [@optional] [@as "aria-haspopup"] ariaHaspopup: false|true|menu|listbox|tree|grid|dialog, */
  @optional @as("aria-level")
  ariaLevel: int,
  @optional @as("aria-modal")
  ariaModal: bool,
  @optional @as("aria-multiline")
  ariaMultiline: bool,
  @optional @as("aria-multiselectable")
  ariaMultiselectable: bool,
  /* [@optional] [@as "aria-orientation"] ariaOrientation: horizontal|vertical|undefined, */
  @optional @as("aria-placeholder")
  ariaPlaceholder: string,
  /* [@optional] [@as "aria-pressed"] ariaPressed: true|false|mixed, /* https://www.w3.org/TR/wai-aria-1.1/#valuetype_tristate */ */
  @optional @as("aria-readonly")
  ariaReadonly: bool,
  @optional @as("aria-required")
  ariaRequired: bool,
  @optional @as("aria-selected")
  ariaSelected: bool,
  @optional @as("aria-sort")
  ariaSort: string,
  @optional @as("aria-valuemax")
  ariaValuemax: float,
  @optional @as("aria-valuemin")
  ariaValuemin: float,
  @optional @as("aria-valuenow")
  ariaValuenow: float,
  @optional @as("aria-valuetext")
  ariaValuetext: string,
  /* Live Region Attributes */
  @optional @as("aria-atomic")
  ariaAtomic: bool,
  @optional @as("aria-busy")
  ariaBusy: bool,
  /* [@optional] [@as "aria-live"] ariaLive: off|polite|assertive|rude, */
  @optional @as("aria-relevant")
  ariaRelevant: string,
  /* Drag-and-Drop Attributes */
  /* [@optional] [@as "aria-dropeffect"] ariaDropeffect: copy|move|link|execute|popup|none, */
  @optional @as("aria-grabbed")
  ariaGrabbed: bool,
  /* Relationship Attributes */
  @optional @as("aria-activedescendant")
  ariaActivedescendant: string,
  @optional @as("aria-colcount")
  ariaColcount: int,
  @optional @as("aria-colindex")
  ariaColindex: int,
  @optional @as("aria-colspan")
  ariaColspan: int,
  @optional @as("aria-controls")
  ariaControls: string,
  @optional @as("aria-describedby")
  ariaDescribedby: string,
  @optional @as("aria-errormessage")
  ariaErrormessage: string,
  @optional @as("aria-flowto")
  ariaFlowto: string,
  @optional @as("aria-labelledby")
  ariaLabelledby: string,
  @optional @as("aria-owns")
  ariaOwns: string,
  @optional @as("aria-posinset")
  ariaPosinset: int,
  @optional @as("aria-rowcount")
  ariaRowcount: int,
  @optional @as("aria-rowindex")
  ariaRowindex: int,
  @optional @as("aria-rowspan")
  ariaRowspan: int,
  @optional @as("aria-setsize")
  ariaSetsize: int,
  /* react textarea/input */
  @optional
  defaultChecked: bool,
  @optional
  defaultValue: string,
  /* global html attributes */
  @optional
  accessKey: string,
  @optional
  className: string /* substitute for "class" */,
  @optional
  contentEditable: bool,
  @optional
  contextMenu: string,
  @optional
  dir: string /* "ltr", "rtl" or "auto" */,
  @optional
  draggable: bool,
  @optional
  hidden: bool,
  @optional
  id: string,
  @optional
  lang: string,
  @optional
  role: string /* ARIA role */,
  @optional
  style: style,
  @optional
  spellCheck: bool,
  @optional
  tabIndex: int,
  @optional
  title: string,
  /* html5 microdata */
  @optional
  itemID: string,
  @optional
  itemProp: string,
  @optional
  itemRef: string,
  @optional
  itemScope: bool,
  @optional
  itemType: string /* uri */,
  /* tag-specific html attributes */
  @optional
  accept: string,
  @optional
  acceptCharset: string,
  @optional
  action: string /* uri */,
  @optional
  allowFullScreen: bool,
  @optional
  alt: string,
  @optional
  async: bool,
  @optional
  autoComplete: string /* has a fixed, but large-ish, set of possible values */,
  @optional
  autoFocus: bool,
  @optional
  autoPlay: bool,
  @optional
  challenge: string,
  @optional
  charSet: string,
  @optional
  checked: bool,
  @optional
  cite: string /* uri */,
  @optional
  crossorigin: bool,
  @optional
  cols: int,
  @optional
  colSpan: int,
  @optional
  content: string,
  @optional
  controls: bool,
  @optional
  coords: string /* set of values specifying the coordinates of a region */,
  @optional
  data: string /* uri */,
  @optional
  dateTime: string /* "valid date string with optional time" */,
  @optional
  default: bool,
  @optional
  defer: bool,
  @optional
  disabled: bool,
  @optional
  download: string /* should really be either a boolean, signifying presence, or a string */,
  @optional
  encType: string /* "application/x-www-form-urlencoded", "multipart/form-data" or "text/plain" */,
  @optional
  form: string,
  @optional
  formAction: string /* uri */,
  @optional
  formTarget: string /* "_blank", "_self", etc. */,
  @optional
  formMethod: string /* "post", "get", "put" */,
  @optional
  headers: string,
  @optional
  height: string /* in html5 this can only be a number, but in html4 it can ba a percentage as well */,
  @optional
  high: int,
  @optional
  href: string /* uri */,
  @optional
  hrefLang: string,
  @optional
  htmlFor: string /* substitute for "for" */,
  @optional
  httpEquiv: string /* has a fixed set of possible values */,
  @optional
  icon: string /* uri? */,
  @optional
  inputMode: string /* "verbatim", "latin", "numeric", etc. */,
  @optional
  integrity: string,
  @optional
  keyType: string,
  @optional
  kind: string /* has a fixed set of possible values */,
  @optional
  label: string,
  @optional
  list: string,
  @optional
  loop: bool,
  @optional
  low: int,
  @optional
  manifest: string /* uri */,
  @optional
  max: string /* should be int or Js.Date.t */,
  @optional
  maxLength: int,
  @optional
  media: string /* a valid media query */,
  @optional
  mediaGroup: string,
  @optional
  method: string /* "post" or "get" */,
  @optional
  min: int,
  @optional
  minLength: int,
  @optional
  multiple: bool,
  @optional
  muted: bool,
  @optional
  name: string,
  @optional
  nonce: string,
  @optional
  noValidate: bool,
  @optional @as("open")
  open_: bool /* use this one. Previous one is deprecated */,
  @optional
  optimum: int,
  @optional
  pattern: string /* valid Js RegExp */,
  @optional
  placeholder: string,
  @optional
  poster: string /* uri */,
  @optional
  preload: string /* "none", "metadata" or "auto" (and "" as a synonym for "auto") */,
  @optional
  radioGroup: string,
  @optional
  readOnly: bool,
  @optional
  rel: string /* a space- or comma-separated (depending on the element) list of a fixed set of "link types" */,
  @optional
  required: bool,
  @optional
  reversed: bool,
  @optional
  rows: int,
  @optional
  rowSpan: int,
  @optional
  sandbox: string /* has a fixed set of possible values */,
  @optional
  scope: string /* has a fixed set of possible values */,
  @optional
  scoped: bool,
  @optional
  scrolling: string /* html4 only, "auto", "yes" or "no" */,
  /* seamless - supported by React, but removed from the html5 spec */
  @optional
  selected: bool,
  @optional
  shape: string,
  @optional
  size: int,
  @optional
  sizes: string,
  @optional
  span: int,
  @optional
  src: string /* uri */,
  @optional
  srcDoc: string,
  @optional
  srcLang: string,
  @optional
  srcSet: string,
  @optional
  start: int,
  @optional
  step: float,
  @optional
  summary: string /* deprecated */,
  @optional
  target: string,
  @optional @as("type")
  type_: string /* has a fixed but large-ish set of possible values */ /* use this one. Previous one is deprecated */,
  @optional
  useMap: string,
  @optional
  value: string,
  @optional
  width: string /* in html5 this can only be a number, but in html4 it can ba a percentage as well */,
  @optional
  wrap: string /* "hard" or "soft" */,
  /* Clipboard events */
  @optional
  onCopy: ReactEvent.Clipboard.t => unit,
  @optional
  onCut: ReactEvent.Clipboard.t => unit,
  @optional
  onPaste: ReactEvent.Clipboard.t => unit,
  /* Composition events */
  @optional
  onCompositionEnd: ReactEvent.Composition.t => unit,
  @optional
  onCompositionStart: ReactEvent.Composition.t => unit,
  @optional
  onCompositionUpdate: ReactEvent.Composition.t => unit,
  /* Keyboard events */
  @optional
  onKeyDown: ReactEvent.Keyboard.t => unit,
  @optional
  onKeyPress: ReactEvent.Keyboard.t => unit,
  @optional
  onKeyUp: ReactEvent.Keyboard.t => unit,
  /* Focus events */
  @optional
  onFocus: ReactEvent.Focus.t => unit,
  @optional
  onBlur: ReactEvent.Focus.t => unit,
  /* Form events */
  @optional
  onChange: ReactEvent.Form.t => unit,
  @optional
  onInput: ReactEvent.Form.t => unit,
  @optional
  onSubmit: ReactEvent.Form.t => unit,
  /* Mouse events */
  @optional
  onClick: ReactEvent.Mouse.t => unit,
  @optional
  onContextMenu: ReactEvent.Mouse.t => unit,
  @optional
  onDoubleClick: ReactEvent.Mouse.t => unit,
  @optional
  onDrag: ReactEvent.Mouse.t => unit,
  @optional
  onDragEnd: ReactEvent.Mouse.t => unit,
  @optional
  onDragEnter: ReactEvent.Mouse.t => unit,
  @optional
  onDragExit: ReactEvent.Mouse.t => unit,
  @optional
  onDragLeave: ReactEvent.Mouse.t => unit,
  @optional
  onDragOver: ReactEvent.Mouse.t => unit,
  @optional
  onDragStart: ReactEvent.Mouse.t => unit,
  @optional
  onDrop: ReactEvent.Mouse.t => unit,
  @optional
  onMouseDown: ReactEvent.Mouse.t => unit,
  @optional
  onMouseEnter: ReactEvent.Mouse.t => unit,
  @optional
  onMouseLeave: ReactEvent.Mouse.t => unit,
  @optional
  onMouseMove: ReactEvent.Mouse.t => unit,
  @optional
  onMouseOut: ReactEvent.Mouse.t => unit,
  @optional
  onMouseOver: ReactEvent.Mouse.t => unit,
  @optional
  onMouseUp: ReactEvent.Mouse.t => unit,
  /* Selection events */
  @optional
  onSelect: ReactEvent.Selection.t => unit,
  /* Touch events */
  @optional
  onTouchCancel: ReactEvent.Touch.t => unit,
  @optional
  onTouchEnd: ReactEvent.Touch.t => unit,
  @optional
  onTouchMove: ReactEvent.Touch.t => unit,
  @optional
  onTouchStart: ReactEvent.Touch.t => unit,
  /* UI events */
  @optional
  onScroll: ReactEvent.UI.t => unit,
  /* Wheel events */
  @optional
  onWheel: ReactEvent.Wheel.t => unit,
  /* Media events */
  @optional
  onAbort: ReactEvent.Media.t => unit,
  @optional
  onCanPlay: ReactEvent.Media.t => unit,
  @optional
  onCanPlayThrough: ReactEvent.Media.t => unit,
  @optional
  onDurationChange: ReactEvent.Media.t => unit,
  @optional
  onEmptied: ReactEvent.Media.t => unit,
  @optional
  onEncrypetd: ReactEvent.Media.t => unit,
  @optional
  onEnded: ReactEvent.Media.t => unit,
  @optional
  onError: ReactEvent.Media.t => unit,
  @optional
  onLoadedData: ReactEvent.Media.t => unit,
  @optional
  onLoadedMetadata: ReactEvent.Media.t => unit,
  @optional
  onLoadStart: ReactEvent.Media.t => unit,
  @optional
  onPause: ReactEvent.Media.t => unit,
  @optional
  onPlay: ReactEvent.Media.t => unit,
  @optional
  onPlaying: ReactEvent.Media.t => unit,
  @optional
  onProgress: ReactEvent.Media.t => unit,
  @optional
  onRateChange: ReactEvent.Media.t => unit,
  @optional
  onSeeked: ReactEvent.Media.t => unit,
  @optional
  onSeeking: ReactEvent.Media.t => unit,
  @optional
  onStalled: ReactEvent.Media.t => unit,
  @optional
  onSuspend: ReactEvent.Media.t => unit,
  @optional
  onTimeUpdate: ReactEvent.Media.t => unit,
  @optional
  onVolumeChange: ReactEvent.Media.t => unit,
  @optional
  onWaiting: ReactEvent.Media.t => unit,
  /* Image events */
  @optional
  onLoad: ReactEvent.Image.t => unit /* duplicate */ /* ~onError: ReactEvent.Image.t => unit=?, */,
  /* Animation events */
  @optional
  onAnimationStart: ReactEvent.Animation.t => unit,
  @optional
  onAnimationEnd: ReactEvent.Animation.t => unit,
  @optional
  onAnimationIteration: ReactEvent.Animation.t => unit,
  /* Transition events */
  @optional
  onTransitionEnd: ReactEvent.Transition.t => unit,
  /* svg */
  @optional
  accentHeight: string,
  @optional
  accumulate: string,
  @optional
  additive: string,
  @optional
  alignmentBaseline: string,
  @optional
  allowReorder: string,
  @optional
  alphabetic: string,
  @optional
  amplitude: string,
  @optional
  arabicForm: string,
  @optional
  ascent: string,
  @optional
  attributeName: string,
  @optional
  attributeType: string,
  @optional
  autoReverse: string,
  @optional
  azimuth: string,
  @optional
  baseFrequency: string,
  @optional
  baseProfile: string,
  @optional
  baselineShift: string,
  @optional
  bbox: string,
  @optional @as("begin")
  begin_: string /* use this one. Previous one is deprecated */,
  @optional
  bias: string,
  @optional
  by: string,
  @optional
  calcMode: string,
  @optional
  capHeight: string,
  @optional
  clip: string,
  @optional
  clipPath: string,
  @optional
  clipPathUnits: string,
  @optional
  clipRule: string,
  @optional
  colorInterpolation: string,
  @optional
  colorInterpolationFilters: string,
  @optional
  colorProfile: string,
  @optional
  colorRendering: string,
  @optional
  contentScriptType: string,
  @optional
  contentStyleType: string,
  @optional
  cursor: string,
  @optional
  cx: string,
  @optional
  cy: string,
  @optional
  d: string,
  @optional
  decelerate: string,
  @optional
  descent: string,
  @optional
  diffuseConstant: string,
  @optional
  direction: string,
  @optional
  display: string,
  @optional
  divisor: string,
  @optional
  dominantBaseline: string,
  @optional
  dur: string,
  @optional
  dx: string,
  @optional
  dy: string,
  @optional
  edgeMode: string,
  @optional
  elevation: string,
  @optional
  enableBackground: string,
  @optional @as("end")
  end_: string /* use this one. Previous one is deprecated */,
  @optional
  exponent: string,
  @optional
  externalResourcesRequired: string,
  @optional
  fill: string,
  @optional
  fillOpacity: string,
  @optional
  fillRule: string,
  @optional
  filter: string,
  @optional
  filterRes: string,
  @optional
  filterUnits: string,
  @optional
  floodColor: string,
  @optional
  floodOpacity: string,
  @optional
  focusable: string,
  @optional
  fontFamily: string,
  @optional
  fontSize: string,
  @optional
  fontSizeAdjust: string,
  @optional
  fontStretch: string,
  @optional
  fontStyle: string,
  @optional
  fontVariant: string,
  @optional
  fontWeight: string,
  @optional
  fomat: string,
  @optional
  from: string,
  @optional
  fx: string,
  @optional
  fy: string,
  @optional
  g1: string,
  @optional
  g2: string,
  @optional
  glyphName: string,
  @optional
  glyphOrientationHorizontal: string,
  @optional
  glyphOrientationVertical: string,
  @optional
  glyphRef: string,
  @optional
  gradientTransform: string,
  @optional
  gradientUnits: string,
  @optional
  hanging: string,
  @optional
  horizAdvX: string,
  @optional
  horizOriginX: string,
  @optional
  ideographic: string,
  @optional
  imageRendering: string,
  @optional @as("in")
  in_: string /* use this one. Previous one is deprecated */,
  @optional
  in2: string,
  @optional
  intercept: string,
  @optional
  k: string,
  @optional
  k1: string,
  @optional
  k2: string,
  @optional
  k3: string,
  @optional
  k4: string,
  @optional
  kernelMatrix: string,
  @optional
  kernelUnitLength: string,
  @optional
  kerning: string,
  @optional
  keyPoints: string,
  @optional
  keySplines: string,
  @optional
  keyTimes: string,
  @optional
  lengthAdjust: string,
  @optional
  letterSpacing: string,
  @optional
  lightingColor: string,
  @optional
  limitingConeAngle: string,
  @optional
  local: string,
  @optional
  markerEnd: string,
  @optional
  markerHeight: string,
  @optional
  markerMid: string,
  @optional
  markerStart: string,
  @optional
  markerUnits: string,
  @optional
  markerWidth: string,
  @optional
  mask: string,
  @optional
  maskContentUnits: string,
  @optional
  maskUnits: string,
  @optional
  mathematical: string,
  @optional
  mode: string,
  @optional
  numOctaves: string,
  @optional
  offset: string,
  @optional
  opacity: string,
  @optional
  operator: string,
  @optional
  order: string,
  @optional
  orient: string,
  @optional
  orientation: string,
  @optional
  origin: string,
  @optional
  overflow: string,
  @optional
  overflowX: string,
  @optional
  overflowY: string,
  @optional
  overlinePosition: string,
  @optional
  overlineThickness: string,
  @optional
  paintOrder: string,
  @optional
  panose1: string,
  @optional
  pathLength: string,
  @optional
  patternContentUnits: string,
  @optional
  patternTransform: string,
  @optional
  patternUnits: string,
  @optional
  pointerEvents: string,
  @optional
  points: string,
  @optional
  pointsAtX: string,
  @optional
  pointsAtY: string,
  @optional
  pointsAtZ: string,
  @optional
  preserveAlpha: string,
  @optional
  preserveAspectRatio: string,
  @optional
  primitiveUnits: string,
  @optional
  r: string,
  @optional
  radius: string,
  @optional
  refX: string,
  @optional
  refY: string,
  @optional
  renderingIntent: string,
  @optional
  repeatCount: string,
  @optional
  repeatDur: string,
  @optional
  requiredExtensions: string,
  @optional
  requiredFeatures: string,
  @optional
  restart: string,
  @optional
  result: string,
  @optional
  rotate: string,
  @optional
  rx: string,
  @optional
  ry: string,
  @optional
  scale: string,
  @optional
  seed: string,
  @optional
  shapeRendering: string,
  @optional
  slope: string,
  @optional
  spacing: string,
  @optional
  specularConstant: string,
  @optional
  specularExponent: string,
  @optional
  speed: string,
  @optional
  spreadMethod: string,
  @optional
  startOffset: string,
  @optional
  stdDeviation: string,
  @optional
  stemh: string,
  @optional
  stemv: string,
  @optional
  stitchTiles: string,
  @optional
  stopColor: string,
  @optional
  stopOpacity: string,
  @optional
  strikethroughPosition: string,
  @optional
  strikethroughThickness: string,
  @optional
  string: string,
  @optional
  stroke: string,
  @optional
  strokeDasharray: string,
  @optional
  strokeDashoffset: string,
  @optional
  strokeLinecap: string,
  @optional
  strokeLinejoin: string,
  @optional
  strokeMiterlimit: string,
  @optional
  strokeOpacity: string,
  @optional
  strokeWidth: string,
  @optional
  surfaceScale: string,
  @optional
  systemLanguage: string,
  @optional
  tableValues: string,
  @optional
  targetX: string,
  @optional
  targetY: string,
  @optional
  textAnchor: string,
  @optional
  textDecoration: string,
  @optional
  textLength: string,
  @optional
  textRendering: string,
  @optional @as("to")
  to_: string /* use this one. Previous one is deprecated */,
  @optional
  transform: string,
  @optional
  u1: string,
  @optional
  u2: string,
  @optional
  underlinePosition: string,
  @optional
  underlineThickness: string,
  @optional
  unicode: string,
  @optional
  unicodeBidi: string,
  @optional
  unicodeRange: string,
  @optional
  unitsPerEm: string,
  @optional
  vAlphabetic: string,
  @optional
  vHanging: string,
  @optional
  vIdeographic: string,
  @optional
  vMathematical: string,
  @optional
  values: string,
  @optional
  vectorEffect: string,
  @optional
  version: string,
  @optional
  vertAdvX: string,
  @optional
  vertAdvY: string,
  @optional
  vertOriginX: string,
  @optional
  vertOriginY: string,
  @optional
  viewBox: string,
  @optional
  viewTarget: string,
  @optional
  visibility: string,
  /* width::string? => */
  @optional
  widths: string,
  @optional
  wordSpacing: string,
  @optional
  writingMode: string,
  @optional
  x: string,
  @optional
  x1: string,
  @optional
  x2: string,
  @optional
  xChannelSelector: string,
  @optional
  xHeight: string,
  @optional
  xlinkActuate: string,
  @optional
  xlinkArcrole: string,
  @optional
  xlinkHref: string,
  @optional
  xlinkRole: string,
  @optional
  xlinkShow: string,
  @optional
  xlinkTitle: string,
  @optional
  xlinkType: string,
  @optional
  xmlns: string,
  @optional
  xmlnsXlink: string,
  @optional
  xmlBase: string,
  @optional
  xmlLang: string,
  @optional
  xmlSpace: string,
  @optional
  y: string,
  @optional
  y1: string,
  @optional
  y2: string,
  @optional
  yChannelSelector: string,
  @optional
  z: string,
  @optional
  zoomAndPan: string,
  /* RDFa */
  @optional
  about: string,
  @optional
  datatype: string,
  @optional
  inlist: string,
  @optional
  prefix: string,
  @optional
  property: string,
  @optional
  resource: string,
  @optional
  typeof: string,
  @optional
  vocab: string,
  /* react-specific */
  @optional
  dangerouslySetInnerHTML: {"__html": string},
  @optional
  suppressContentEditableWarning: bool,
}

external objToDOMProps: {..} => props = "%identity"

@deprecated("Please use ReactDOMRe.props instead")
type reactDOMProps = props

@variadic @val @module("react")
external createElement: (string, ~props: props=?, array<React.element>) => React.element =
  "createElement"

/* Only wanna expose createElementVariadic here. Don't wanna write an interface file */
include (
  /* Use varargs to avoid the ReactJS warning for duplicate keys in children */
  {
    @val @module("react")
    external createElementInternalHack: 'a = "createElement"
    @send
    external apply: ('theFunction, 'theContext, 'arguments) => 'returnTypeOfTheFunction = "apply"

    let createElementVariadic = (domClassName, ~props=?, children) => {
      let variadicArguments =
        [Obj.magic(domClassName), Obj.magic(props)]->Js.Array.concat(children)
      createElementInternalHack->apply(Js.Nullable.null, variadicArguments)
    }
  }: {
    let createElementVariadic: (string, ~props: props=?, array<React.element>) => React.element
  }
)

module Style = {
  type t = style
  @obj
  external make: (
    ~azimuth: string=?,
    ~background: string=?,
    ~backgroundAttachment: string=?,
    ~backgroundColor: string=?,
    ~backgroundImage: string=?,
    ~backgroundPosition: string=?,
    ~backgroundRepeat: string=?,
    ~border: string=?,
    ~borderCollapse: string=?,
    ~borderColor: string=?,
    ~borderSpacing: string=?,
    ~borderStyle: string=?,
    ~borderTop: string=?,
    ~borderRight: string=?,
    ~borderBottom: string=?,
    ~borderLeft: string=?,
    ~borderTopColor: string=?,
    ~borderRightColor: string=?,
    ~borderBottomColor: string=?,
    ~borderLeftColor: string=?,
    ~borderTopStyle: string=?,
    ~borderRightStyle: string=?,
    ~borderBottomStyle: string=?,
    ~borderLeftStyle: string=?,
    ~borderTopWidth: string=?,
    ~borderRightWidth: string=?,
    ~borderBottomWidth: string=?,
    ~borderLeftWidth: string=?,
    ~borderWidth: string=?,
    ~bottom: string=?,
    ~captionSide: string=?,
    ~clear: string=?,
    ~clip: string=?,
    ~color: string=?,
    ~content: string=?,
    ~counterIncrement: string=?,
    ~counterReset: string=?,
    ~cue: string=?,
    ~cueAfter: string=?,
    ~cueBefore: string=?,
    ~cursor: string=?,
    ~direction: string=?,
    ~display: string=?,
    ~elevation: string=?,
    ~emptyCells: string=?,
    ~float: string=?,
    ~font: string=?,
    ~fontFamily: string=?,
    ~fontSize: string=?,
    ~fontSizeAdjust: string=?,
    ~fontStretch: string=?,
    ~fontStyle: string=?,
    ~fontVariant: string=?,
    ~fontWeight: string=?,
    ~height: string=?,
    ~left: string=?,
    ~letterSpacing: string=?,
    ~lineHeight: string=?,
    ~listStyle: string=?,
    ~listStyleImage: string=?,
    ~listStylePosition: string=?,
    ~listStyleType: string=?,
    ~margin: string=?,
    ~marginTop: string=?,
    ~marginRight: string=?,
    ~marginBottom: string=?,
    ~marginLeft: string=?,
    ~markerOffset: string=?,
    ~marks: string=?,
    ~maxHeight: string=?,
    ~maxWidth: string=?,
    ~minHeight: string=?,
    ~minWidth: string=?,
    ~orphans: string=?,
    ~outline: string=?,
    ~outlineColor: string=?,
    ~outlineStyle: string=?,
    ~outlineWidth: string=?,
    ~overflow: string=?,
    ~overflowX: string=?,
    ~overflowY: string=?,
    ~padding: string=?,
    ~paddingTop: string=?,
    ~paddingRight: string=?,
    ~paddingBottom: string=?,
    ~paddingLeft: string=?,
    ~page: string=?,
    ~pageBreakAfter: string=?,
    ~pageBreakBefore: string=?,
    ~pageBreakInside: string=?,
    ~pause: string=?,
    ~pauseAfter: string=?,
    ~pauseBefore: string=?,
    ~pitch: string=?,
    ~pitchRange: string=?,
    ~playDuring: string=?,
    ~position: string=?,
    ~quotes: string=?,
    ~richness: string=?,
    ~right: string=?,
    ~size: string=?,
    ~speak: string=?,
    ~speakHeader: string=?,
    ~speakNumeral: string=?,
    ~speakPunctuation: string=?,
    ~speechRate: string=?,
    ~stress: string=?,
    ~tableLayout: string=?,
    ~textAlign: string=?,
    ~textDecoration: string=?,
    ~textIndent: string=?,
    ~textShadow: string=?,
    ~textTransform: string=?,
    ~top: string=?,
    ~unicodeBidi: string=?,
    ~verticalAlign: string=?,
    ~visibility: string=?,
    ~voiceFamily: string=?,
    ~volume: string=?,
    ~whiteSpace: string=?,
    ~widows: string=?,
    ~width: string=?,
    ~wordSpacing: string=?,
    ~zIndex: string=?,
    ~opacity: /* Below properties based on https://www.w3.org/Style/CSS/all-properties */
    /* Color Level 3 - REC */
    string=?,
    ~backgroundOrigin: /* Backgrounds and Borders Level 3 - CR */
    /* backgroundRepeat - already defined by CSS2Properties */
    /* backgroundAttachment - already defined by CSS2Properties */
    string=?,
    ~backgroundSize: string=?,
    ~backgroundClip: string=?,
    ~borderRadius: string=?,
    ~borderTopLeftRadius: string=?,
    ~borderTopRightRadius: string=?,
    ~borderBottomLeftRadius: string=?,
    ~borderBottomRightRadius: string=?,
    ~borderImage: string=?,
    ~borderImageSource: string=?,
    ~borderImageSlice: string=?,
    ~borderImageWidth: string=?,
    ~borderImageOutset: string=?,
    ~borderImageRepeat: string=?,
    ~boxShadow: string=?,
    ~columns: /* Multi-column Layout - CR */
    string=?,
    ~columnCount: string=?,
    ~columnFill: string=?,
    ~columnGap: string=?,
    ~columnRule: string=?,
    ~columnRuleColor: string=?,
    ~columnRuleStyle: string=?,
    ~columnRuleWidth: string=?,
    ~columnSpan: string=?,
    ~columnWidth: string=?,
    ~breakAfter: string=?,
    ~breakBefore: string=?,
    ~breakInside: string=?,
    ~rest: /* Speech - CR */
    string=?,
    ~restAfter: string=?,
    ~restBefore: string=?,
    ~speakAs: string=?,
    ~voiceBalance: string=?,
    ~voiceDuration: string=?,
    ~voicePitch: string=?,
    ~voiceRange: string=?,
    ~voiceRate: string=?,
    ~voiceStress: string=?,
    ~voiceVolume: string=?,
    ~objectFit: /* Image Values and Replaced Content Level 3 - CR */
    string=?,
    ~objectPosition: string=?,
    ~imageResolution: string=?,
    ~imageOrientation: string=?,
    ~alignContent: /* Flexible Box Layout - CR */
    string=?,
    ~alignItems: string=?,
    ~alignSelf: string=?,
    ~flex: string=?,
    ~flexBasis: string=?,
    ~flexDirection: string=?,
    ~flexFlow: string=?,
    ~flexGrow: string=?,
    ~flexShrink: string=?,
    ~flexWrap: string=?,
    ~justifyContent: string=?,
    ~order: string=?,
    ~textDecorationColor: /* Text Decoration Level 3 - CR */
    /* textDecoration - already defined by CSS2Properties */
    string=?,
    ~textDecorationLine: string=?,
    ~textDecorationSkip: string=?,
    ~textDecorationStyle: string=?,
    ~textEmphasis: string=?,
    ~textEmphasisColor: string=?,
    ~textEmphasisPosition: string=?,
    ~textEmphasisStyle: string=?,
    ~textUnderlinePosition: /* textShadow - already defined by CSS2Properties */
    string=?,
    ~fontFeatureSettings: /* Fonts Level 3 - CR */
    string=?,
    ~fontKerning: string=?,
    ~fontLanguageOverride: string=?,
    ~fontSynthesis: /* fontSizeAdjust - already defined by CSS2Properties */
    /* fontStretch - already defined by CSS2Properties */
    string=?,
    ~forntVariantAlternates: string=?,
    ~fontVariantCaps: string=?,
    ~fontVariantEastAsian: string=?,
    ~fontVariantLigatures: string=?,
    ~fontVariantNumeric: string=?,
    ~fontVariantPosition: string=?,
    ~all: /* Cascading and Inheritance Level 3 - CR */
    string=?,
    ~glyphOrientationVertical: /* Writing Modes Level 3 - CR */
    string=?,
    ~textCombineUpright: string=?,
    ~textOrientation: string=?,
    ~writingMode: string=?,
    ~shapeImageThreshold: /* Shapes Level 1 - CR */
    string=?,
    ~shapeMargin: string=?,
    ~shapeOutside: string=?,
    ~clipPath: /* Masking Level 1 - CR */
    string=?,
    ~clipRule: string=?,
    ~mask: string=?,
    ~maskBorder: string=?,
    ~maskBorderMode: string=?,
    ~maskBorderOutset: string=?,
    ~maskBorderRepeat: string=?,
    ~maskBorderSlice: string=?,
    ~maskBorderSource: string=?,
    ~maskBorderWidth: string=?,
    ~maskClip: string=?,
    ~maskComposite: string=?,
    ~maskImage: string=?,
    ~maskMode: string=?,
    ~maskOrigin: string=?,
    ~maskPosition: string=?,
    ~maskRepeat: string=?,
    ~maskSize: string=?,
    ~maskType: string=?,
    ~backgroundBlendMode: /* Compositing and Blending Level 1 - CR */
    string=?,
    ~isolation: string=?,
    ~mixBlendMode: string=?,
    ~boxDecorationBreak: /* Fragmentation Level 3 - CR */
    string=?,
    ~boxSizing: /* breakAfter - already defined by Multi-column Layout */
    /* breakBefore - already defined by Multi-column Layout */
    /* breakInside - already defined by Multi-column Layout */
    /* Basic User Interface Level 3 - CR */
    string=?,
    ~caretColor: string=?,
    ~navDown: string=?,
    ~navLeft: string=?,
    ~navRight: string=?,
    ~navUp: string=?,
    ~outlineOffset: string=?,
    ~resize: string=?,
    ~textOverflow: string=?,
    ~grid: /* Grid Layout Level 1 - CR */
    string=?,
    ~gridArea: string=?,
    ~gridAutoColumns: string=?,
    ~gridAutoFlow: string=?,
    ~gridAutoRows: string=?,
    ~gridColumn: string=?,
    ~gridColumnEnd: string=?,
    ~gridColumnGap: string=?,
    ~gridColumnStart: string=?,
    ~gridGap: string=?,
    ~gridRow: string=?,
    ~gridRowEnd: string=?,
    ~gridRowGap: string=?,
    ~gridRowStart: string=?,
    ~gridTemplate: string=?,
    ~gridTemplateAreas: string=?,
    ~gridTemplateColumns: string=?,
    ~gridTemplateRows: string=?,
    ~willChange: /* Will Change Level 1 - CR */
    string=?,
    ~hangingPunctuation: /* Text Level 3 - LC */
    string=?,
    ~hyphens: string=?,
    ~lineBreak: /* letterSpacing - already defined by CSS2Properties */
    string=?,
    ~overflowWrap: string=?,
    ~tabSize: string=?,
    ~textAlignLast: /* textAlign - already defined by CSS2Properties */
    string=?,
    ~textJustify: string=?,
    ~wordBreak: string=?,
    ~wordWrap: string=?,
    ~animation: /* Animations - WD */
    string=?,
    ~animationDelay: string=?,
    ~animationDirection: string=?,
    ~animationDuration: string=?,
    ~animationFillMode: string=?,
    ~animationIterationCount: string=?,
    ~animationName: string=?,
    ~animationPlayState: string=?,
    ~animationTimingFunction: string=?,
    ~transition: /* Transitions - WD */
    string=?,
    ~transitionDelay: string=?,
    ~transitionDuration: string=?,
    ~transitionProperty: string=?,
    ~transitionTimingFunction: string=?,
    ~backfaceVisibility: /* Transforms Level 1 - WD */
    string=?,
    ~perspective: string=?,
    ~perspectiveOrigin: string=?,
    ~transform: string=?,
    ~transformOrigin: string=?,
    ~transformStyle: string=?,
    ~justifyItems: /* Box Alignment Level 3 - WD */
    /* alignContent - already defined by Flexible Box Layout */
    /* alignItems - already defined by Flexible Box Layout */
    string=?,
    ~justifySelf: string=?,
    ~placeContent: string=?,
    ~placeItems: string=?,
    ~placeSelf: string=?,
    ~appearance: /* Basic User Interface Level 4 - FPWD */
    string=?,
    ~caret: string=?,
    ~caretAnimation: string=?,
    ~caretShape: string=?,
    ~userSelect: string=?,
    ~maxLines: /* Overflow Level 3 - WD */
    string=?,
    ~marqueeDirection: /* Basix Box Model - WD */
    string=?,
    ~marqueeLoop: string=?,
    ~marqueeSpeed: string=?,
    ~marqueeStyle: string=?,
    ~overflowStyle: string=?,
    ~rotation: string=?,
    ~rotationPoint: string=?,
    ~alignmentBaseline: /* SVG 1.1 - REC */
    string=?,
    ~baselineShift: string=?,
    ~clip: string=?,
    ~clipPath: string=?,
    ~clipRule: string=?,
    ~colorInterpolation: string=?,
    ~colorInterpolationFilters: string=?,
    ~colorProfile: string=?,
    ~colorRendering: string=?,
    ~cursor: string=?,
    ~dominantBaseline: string=?,
    ~fill: string=?,
    ~fillOpacity: string=?,
    ~fillRule: string=?,
    ~filter: string=?,
    ~floodColor: string=?,
    ~floodOpacity: string=?,
    ~glyphOrientationHorizontal: string=?,
    ~glyphOrientationVertical: string=?,
    ~imageRendering: string=?,
    ~kerning: string=?,
    ~lightingColor: string=?,
    ~markerEnd: string=?,
    ~markerMid: string=?,
    ~markerStart: string=?,
    ~pointerEvents: string=?,
    ~shapeRendering: string=?,
    ~stopColor: string=?,
    ~stopOpacity: string=?,
    ~stroke: string=?,
    ~strokeDasharray: string=?,
    ~strokeDashoffset: string=?,
    ~strokeLinecap: string=?,
    ~strokeLinejoin: string=?,
    ~strokeMiterlimit: string=?,
    ~strokeOpacity: string=?,
    ~strokeWidth: string=?,
    ~textAnchor: string=?,
    ~textRendering: string=?,
    ~rubyAlign: /* Ruby Layout Level 1 - WD */
    string=?,
    ~rubyMerge: string=?,
    ~rubyPosition: string=?,
    /* Lists and Counters Level 3 - WD */
    /* listStyle - already defined by CSS2Properties */
    /* listStyleImage - already defined by CSS2Properties */
    /* listStylePosition - already defined by CSS2Properties */
    /* listStyleType - already defined by CSS2Properties */
    /* counterIncrement - already defined by CSS2Properties */
    /* counterReset - already defined by CSS2Properties */
    /* Not added yet
     * -------------
     * Generated Content for Paged Media - WD
     * Generated Content Level 3 - WD
     * Line Grid Level 1 - WD
     * Regions - WD
     * Inline Layout Level 3 - WD
     * Round Display Level 1 - WD
     * Image Values and Replaced Content Level 4 - WD
     * Positioned Layout Level 3 - WD
     * Filter Effects Level 1 -  -WD
     * Exclusions Level 1 - WD
     * Text Level 4 - FPWD
     * SVG Markers - FPWD
     * Motion Path Level 1 - FPWD
     * Color Level 4 - FPWD
     * SVG Strokes - FPWD
     * Table Level 3 - FPWD
     */
    unit,
  ) => style = ""
  /* CSS2Properties: https://www.w3.org/TR/DOM-Level-2-Style/css.html#CSS-CSS2Properties */
  let combine: (style, style) => style = (a, b) => {
    let a: Js.t<{..}> = Obj.magic(a)
    let b: Js.t<{..}> = Obj.magic(b)
    Js.Obj.assign(Js.Obj.assign(Js.Obj.empty(), a), b)->Obj.magic
  }
  let unsafeAddProp: (style, string, string) => style = (style, property, value) => {
    let propStyle: style = {
      let dict = Js.Dict.empty()
      Js.Dict.set(dict, property, value)
      Obj.magic(dict)
    }
    combine(style, propStyle)
  }
}
