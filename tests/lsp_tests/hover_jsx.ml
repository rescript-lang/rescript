module Hover_test = Hover

open Lsp
open Lsp.Types

let ( /+ ) = Filename.concat

let jsx_rescript_json =
  {|{
  "name": "hover-jsx-test",
  "sources": {"dir": "."},
  "dependencies": ["@rescript/react"],
  "jsx": {"version": 4},
  "package-specs": {"module": "esmodule", "in-source": false},
  "suffix": ".res.js"
}|}

let test_hover_with_interface ~interface ~text ~position =
  let root =
    Filename.get_temp_dir_name ()
    /+ if Option.is_some interface then "hover-jsx-interface" else "hover-jsx"
  in
  let uri = Uri.of_path (root /+ "test.res") in
  let doc = Test.make_document ~text ~uri in
  let doc, documents =
    match interface with
    | None -> (doc, [doc])
    | Some text ->
      let interface_doc =
        Test.make_document ~text ~uri:(Uri.of_path (root /+ "test.resi"))
      in
      (interface_doc, [doc; interface_doc])
  in
  let uri = doc.uri in
  Test.setup_react ~root;
  Test.setup_project ~root ~rescript_json:jsx_rescript_json ~run_build:true
    ~documents;

  let line, character = position in

  Test.with_client ~root (fun client ->
      Test.perform_all client
        [
          Test.open_document doc;
          Hover_test.hover ~uri ~position:(Position.create ~line ~character);
        ])
  |> ignore

let test_jsx_hover ~text ~position =
  test_hover_with_interface ~interface:None ~text ~position

let test_jsx_interface_hover ~interface ~text ~position =
  test_hover_with_interface ~interface:(Some interface) ~text ~position

let%expect_test "infers a value inside a module containing a React component" =
  test_jsx_hover ~position:(1, 6)
    ~text:
      {|module HoverInsideModuleWithComponent = {
  let x = 2
  @react.component
  let make = () => React.null
}
|};
  [%expect
    {| { "contents": { "kind": "markdown", "value": "```rescript\nint\n```" } } |}]

let%expect_test "infers a React component prop from its use" =
  test_jsx_hover ~position:(1, 13)
    ~text:{|@react.component
let make = (~name) => React.string(name)
|};
  [%expect
    {| { "contents": { "kind": "markdown", "value": "```rescript\nstring\n```" } } |}]

let%expect_test "shows an annotated prop in a nested React component" =
  test_jsx_hover ~position:(2, 15)
    ~text:
      {|module C2 = {
  @react.component
  let make2 = (~name: string) => React.string(name)
}
|};
  [%expect
    {| { "contents": { "kind": "markdown", "value": "```rescript\nstring\n```" } } |}]

let%expect_test "shows the type of an intrinsic JSX tag" =
  test_jsx_hover ~position:(0, 10) ~text:{|let q = <div />
|};
  [%expect
    {| { "contents": { "kind": "markdown", "value": "```rescript\nstring\n```" } } |}]

let%expect_test "shows the type of an intrinsic JSX attribute" =
  test_jsx_hover ~position:(0, 14) ~text:{|let _ = <div name="" />
|};
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nJsxDOM.domProps\n```\n\n---\n```rescript\ntype JsxDOM.domProps = {\n  key?: string,\n  children?: Jsx.element,\n  ref?: domRef,\n  allow?: string,\n  @as(\"aria-current\") ariaCurrent?: [\n    | #date\n    | #\"false\"\n    | #location\n    | #page\n    | #step\n    | #time\n    | #\"true\"\n  ],\n  @as(\"aria-details\") ariaDetails?: string,\n  @as(\"aria-disabled\") ariaDisabled?: bool,\n  @as(\"aria-hidden\") ariaHidden?: bool,\n  @as(\"aria-invalid\") ariaInvalid?: [\n    | #\"false\"\n    | #grammar\n    | #spelling\n    | #\"true\"\n  ],\n  @as(\"aria-keyshortcuts\") ariaKeyshortcuts?: string,\n  @as(\"aria-label\") ariaLabel?: string,\n  @as(\"aria-roledescription\") ariaRoledescription?: string,\n  @as(\"aria-autocomplete\") ariaAutocomplete?: [\n    | #both\n    | #inline\n    | #list\n    | #none\n  ],\n  @as(\"aria-checked\") ariaChecked?: [\n    | #\"false\"\n    | #mixed\n    | #\"true\"\n  ],\n  @as(\"aria-expanded\") ariaExpanded?: bool,\n  @as(\"aria-haspopup\") ariaHaspopup?: [\n    | #dialog\n    | #\"false\"\n    | #grid\n    | #listbox\n    | #menu\n    | #tree\n    | #\"true\"\n  ],\n  @as(\"aria-level\") ariaLevel?: int,\n  @as(\"aria-modal\") ariaModal?: bool,\n  @as(\"aria-multiline\") ariaMultiline?: bool,\n  @as(\"aria-multiselectable\") ariaMultiselectable?: bool,\n  @as(\"aria-orientation\") ariaOrientation?: [\n    | #horizontal\n    | #undefined\n    | #vertical\n  ],\n  @as(\"aria-placeholder\") ariaPlaceholder?: string,\n  @as(\"aria-pressed\") ariaPressed?: [\n    | #\"false\"\n    | #mixed\n    | #\"true\"\n  ],\n  @as(\"aria-readonly\") ariaReadonly?: bool,\n  @as(\"aria-required\") ariaRequired?: bool,\n  @as(\"aria-selected\") ariaSelected?: bool,\n  @as(\"aria-sort\") ariaSort?: string,\n  @as(\"aria-valuemax\") ariaValuemax?: float,\n  @as(\"aria-valuemin\") ariaValuemin?: float,\n  @as(\"aria-valuenow\") ariaValuenow?: float,\n  @as(\"aria-valuetext\") ariaValuetext?: string,\n  @as(\"aria-atomic\") ariaAtomic?: bool,\n  @as(\"aria-busy\") ariaBusy?: bool,\n  @as(\"aria-live\") ariaLive?: [\n    | #assertive\n    | #off\n    | #polite\n    | #rude\n  ],\n  @as(\"aria-relevant\") ariaRelevant?: string,\n  @as(\"aria-dropeffect\") ariaDropeffect?: [\n    | #copy\n    | #execute\n    | #link\n    | #move\n    | #none\n    | #popup\n  ],\n  @as(\"aria-grabbed\") ariaGrabbed?: bool,\n  @as(\"aria-activedescendant\") ariaActivedescendant?: string,\n  @as(\"aria-colcount\") ariaColcount?: int,\n  @as(\"aria-colindex\") ariaColindex?: int,\n  @as(\"aria-colspan\") ariaColspan?: int,\n  @as(\"aria-controls\") ariaControls?: string,\n  @as(\"aria-describedby\") ariaDescribedby?: string,\n  @as(\"aria-errormessage\") ariaErrormessage?: string,\n  @as(\"aria-flowto\") ariaFlowto?: string,\n  @as(\"aria-labelledby\") ariaLabelledby?: string,\n  @as(\"aria-owns\") ariaOwns?: string,\n  @as(\"aria-posinset\") ariaPosinset?: int,\n  @as(\"aria-rowcount\") ariaRowcount?: int,\n  @as(\"aria-rowindex\") ariaRowindex?: int,\n  @as(\"aria-rowspan\") ariaRowspan?: int,\n  @as(\"aria-setsize\") ariaSetsize?: int,\n  defaultChecked?: bool,\n  defaultValue?: string,\n  accessKey?: string,\n  capture?: [#environment | #user],\n  className?: string,\n  contentEditable?: bool,\n  contextMenu?: string,\n  @as(\"data-testid\") dataTestId?: string,\n  dir?: string,\n  draggable?: bool,\n  hidden?: bool,\n  id?: string,\n  inert?: bool,\n  lang?: string,\n  popover?: popover,\n  popoverTarget?: string,\n  popoverTargetAction?: popoverTargetAction,\n  role?: string,\n  style?: style,\n  spellCheck?: bool,\n  tabIndex?: int,\n  title?: string,\n  itemID?: string,\n  itemProp?: string,\n  itemRef?: string,\n  itemScope?: bool,\n  itemType?: string,\n  accept?: string,\n  acceptCharset?: string,\n  action?: string,\n  allowFullScreen?: bool,\n  alt?: string,\n  @as(\"as\") as_?: string,\n  async?: bool,\n  autoComplete?: string,\n  autoCapitalize?: string,\n  autoFocus?: bool,\n  autoPlay?: bool,\n  challenge?: string,\n  charSet?: string,\n  checked?: bool,\n  cite?: string,\n  crossOrigin?: string,\n  cols?: int,\n  colSpan?: int,\n  content?: string,\n  controls?: bool,\n  coords?: string,\n  data?: string,\n  dateTime?: string,\n  default?: bool,\n  defer?: bool,\n  disabled?: bool,\n  download?: string,\n  encType?: string,\n  form?: string,\n  formAction?: string,\n  formTarget?: string,\n  formMethod?: string,\n  frameBorder?: int,\n  headers?: string,\n  height?: string,\n  high?: int,\n  href?: string,\n  hrefLang?: string,\n  htmlFor?: string,\n  httpEquiv?: string,\n  icon?: string,\n  inputMode?: string,\n  integrity?: string,\n  keyType?: string,\n  kind?: string,\n  label?: string,\n  list?: string,\n  loading?: [#eager | #lazy],\n  loop?: bool,\n  low?: int,\n  manifest?: string,\n  max?: string,\n  maxLength?: int,\n  media?: string,\n  mediaGroup?: string,\n  method?: string,\n  min?: string,\n  minLength?: int,\n  multiple?: bool,\n  muted?: bool,\n  name?: string,\n  nonce?: string,\n  noValidate?: bool,\n  @as(\"open\") open_?: bool,\n  optimum?: int,\n  pattern?: string,\n  placeholder?: string,\n  playsInline?: bool,\n  poster?: string,\n  preload?: string,\n  radioGroup?: string,\n  readOnly?: bool,\n  rel?: string,\n  required?: bool,\n  reversed?: bool,\n  rows?: int,\n  rowSpan?: int,\n  sandbox?: string,\n  scope?: string,\n  scoped?: bool,\n  scrolling?: string,\n  selected?: bool,\n  shape?: string,\n  size?: int,\n  sizes?: string,\n  span?: int,\n  src?: string,\n  srcDoc?: string,\n  srcLang?: string,\n  srcSet?: string,\n  start?: int,\n  step?: float,\n  summary?: string,\n  target?: string,\n  @as(\"type\") type_?: string,\n  useMap?: string,\n  value?: string,\n  width?: string,\n  wrap?: string,\n  onCancel?: JsxEvent.Synthetic.t => unit,\n  onCancelCapture?: JsxEvent.Synthetic.t => unit,\n  onClose?: JsxEvent.Synthetic.t => unit,\n  onCloseCapture?: JsxEvent.Synthetic.t => unit,\n  onFullscreenChange?: JsxEvent.Synthetic.t => unit,\n  onFullscreenChangeCapture?: JsxEvent.Synthetic.t => unit,\n  onFullscreenError?: JsxEvent.Synthetic.t => unit,\n  onFullscreenErrorCapture?: JsxEvent.Synthetic.t => unit,\n  onBeforeToggle?: JsxEvent.Toggle.t => unit,\n  onBeforeToggleCapture?: JsxEvent.Toggle.t => unit,\n  onToggle?: JsxEvent.Toggle.t => unit,\n  onToggleCapture?: JsxEvent.Toggle.t => unit,\n  onCopy?: JsxEvent.Clipboard.t => unit,\n  onCopyCapture?: JsxEvent.Clipboard.t => unit,\n  onCut?: JsxEvent.Clipboard.t => unit,\n  onCutCapture?: JsxEvent.Clipboard.t => unit,\n  onPaste?: JsxEvent.Clipboard.t => unit,\n  onPasteCapture?: JsxEvent.Clipboard.t => unit,\n  onCompositionEnd?: JsxEvent.Composition.t => unit,\n  onCompositionEndCapture?: JsxEvent.Composition.t => unit,\n  onCompositionStart?: JsxEvent.Composition.t => unit,\n  onCompositionStartCapture?: JsxEvent.Composition.t => unit,\n  onCompositionUpdate?: JsxEvent.Composition.t => unit,\n  onCompositionUpdateCapture?: JsxEvent.Composition.t => unit,\n  onKeyDown?: JsxEvent.Keyboard.t => unit,\n  onKeyDownCapture?: JsxEvent.Keyboard.t => unit,\n  onKeyPress?: JsxEvent.Keyboard.t => unit,\n  onKeyPressCapture?: JsxEvent.Keyboard.t => unit,\n  onKeyUp?: JsxEvent.Keyboard.t => unit,\n  onKeyUpCapture?: JsxEvent.Keyboard.t => unit,\n  onFocus?: JsxEvent.Focus.t => unit,\n  onFocusCapture?: JsxEvent.Focus.t => unit,\n  onBlur?: JsxEvent.Focus.t => unit,\n  onBlurCapture?: JsxEvent.Focus.t => unit,\n  onBeforeInput?: JsxEvent.Form.t => unit,\n  onBeforeInputCapture?: JsxEvent.Form.t => unit,\n  onChange?: JsxEvent.Form.t => unit,\n  onChangeCapture?: JsxEvent.Form.t => unit,\n  onInput?: JsxEvent.Form.t => unit,\n  onInputCapture?: JsxEvent.Form.t => unit,\n  onReset?: JsxEvent.Form.t => unit,\n  onResetCapture?: JsxEvent.Form.t => unit,\n  onSubmit?: JsxEvent.Form.t => unit,\n  onSubmitCapture?: JsxEvent.Form.t => unit,\n  onInvalid?: JsxEvent.Form.t => unit,\n  onInvalidCapture?: JsxEvent.Form.t => unit,\n  onAuxClick?: JsxEvent.Mouse.t => unit,\n  onAuxClickCapture?: JsxEvent.Mouse.t => unit,\n  onClick?: JsxEvent.Mouse.t => unit,\n  onClickCapture?: JsxEvent.Mouse.t => unit,\n  onContextMenu?: JsxEvent.Mouse.t => unit,\n  onContextMenuCapture?: JsxEvent.Mouse.t => unit,\n  onDoubleClick?: JsxEvent.Mouse.t => unit,\n  onDoubleClickCapture?: JsxEvent.Mouse.t => unit,\n  onDrag?: JsxEvent.Mouse.t => unit,\n  onDragCapture?: JsxEvent.Mouse.t => unit,\n  onDragEnd?: JsxEvent.Mouse.t => unit,\n  onDragEndCapture?: JsxEvent.Mouse.t => unit,\n  onDragEnter?: JsxEvent.Mouse.t => unit,\n  onDragEnterCapture?: JsxEvent.Mouse.t => unit,\n  onDragExit?: JsxEvent.Mouse.t => unit,\n  onDragExitCapture?: JsxEvent.Mouse.t => unit,\n  onDragLeave?: JsxEvent.Mouse.t => unit,\n  onDragLeaveCapture?: JsxEvent.Mouse.t => unit,\n  onDragOver?: JsxEvent.Mouse.t => unit,\n  onDragOverCapture?: JsxEvent.Mouse.t => unit,\n  onDragStart?: JsxEvent.Mouse.t => unit,\n  onDragStartCapture?: JsxEvent.Mouse.t => unit,\n  onDrop?: JsxEvent.Mouse.t => unit,\n  onDropCapture?: JsxEvent.Mouse.t => unit,\n  onMouseDown?: JsxEvent.Mouse.t => unit,\n  onMouseDownCapture?: JsxEvent.Mouse.t => unit,\n  onMouseEnter?: JsxEvent.Mouse.t => unit,\n  onMouseLeave?: JsxEvent.Mouse.t => unit,\n  onMouseMove?: JsxEvent.Mouse.t => unit,\n  onMouseMoveCapture?: JsxEvent.Mouse.t => unit,\n  onMouseOut?: JsxEvent.Mouse.t => unit,\n  onMouseOutCapture?: JsxEvent.Mouse.t => unit,\n  onMouseOver?: JsxEvent.Mouse.t => unit,\n  onMouseOverCapture?: JsxEvent.Mouse.t => unit,\n  onMouseUp?: JsxEvent.Mouse.t => unit,\n  onMouseUpCapture?: JsxEvent.Mouse.t => unit,\n  onSelect?: JsxEvent.Selection.t => unit,\n  onSelectCapture?: JsxEvent.Selection.t => unit,\n  onTouchCancel?: JsxEvent.Touch.t => unit,\n  onTouchCancelCapture?: JsxEvent.Touch.t => unit,\n  onTouchEnd?: JsxEvent.Touch.t => unit,\n  onTouchEndCapture?: JsxEvent.Touch.t => unit,\n  onTouchMove?: JsxEvent.Touch.t => unit,\n  onTouchMoveCapture?: JsxEvent.Touch.t => unit,\n  onTouchStart?: JsxEvent.Touch.t => unit,\n  onTouchStartCapture?: JsxEvent.Touch.t => unit,\n  onPointerOver?: JsxEvent.Pointer.t => unit,\n  onPointerOverCapture?: JsxEvent.Pointer.t => unit,\n  onPointerEnter?: JsxEvent.Pointer.t => unit,\n  onPointerDown?: JsxEvent.Pointer.t => unit,\n  onPointerDownCapture?: JsxEvent.Pointer.t => unit,\n  onPointerMove?: JsxEvent.Pointer.t => unit,\n  onPointerMoveCapture?: JsxEvent.Pointer.t => unit,\n  onPointerUp?: JsxEvent.Pointer.t => unit,\n  onPointerUpCapture?: JsxEvent.Pointer.t => unit,\n  onPointerCancel?: JsxEvent.Pointer.t => unit,\n  onPointerCancelCapture?: JsxEvent.Pointer.t => unit,\n  onPointerOut?: JsxEvent.Pointer.t => unit,\n  onPointerOutCapture?: JsxEvent.Pointer.t => unit,\n  onPointerLeave?: JsxEvent.Pointer.t => unit,\n  onGotPointerCapture?: JsxEvent.Pointer.t => unit,\n  onGotPointerCaptureCapture?: JsxEvent.Pointer.t => unit,\n  onLostPointerCapture?: JsxEvent.Pointer.t => unit,\n  onLostPointerCaptureCapture?: JsxEvent.Pointer.t => unit,\n  onScroll?: JsxEvent.UI.t => unit,\n  onScrollCapture?: JsxEvent.UI.t => unit,\n  onScrollEnd?: JsxEvent.UI.t => unit,\n  onScrollEndCapture?: JsxEvent.UI.t => unit,\n  onWheel?: JsxEvent.Wheel.t => unit,\n  onWheelCapture?: JsxEvent.Wheel.t => unit,\n  onAbort?: JsxEvent.Media.t => unit,\n  onAbortCapture?: JsxEvent.Media.t => unit,\n  onCanPlay?: JsxEvent.Media.t => unit,\n  onCanPlayCapture?: JsxEvent.Media.t => unit,\n  onCanPlayThrough?: JsxEvent.Media.t => unit,\n  onCanPlayThroughCapture?: JsxEvent.Media.t => unit,\n  onDurationChange?: JsxEvent.Media.t => unit,\n  onDurationChangeCapture?: JsxEvent.Media.t => unit,\n  onEmptied?: JsxEvent.Media.t => unit,\n  onEmptiedCapture?: JsxEvent.Media.t => unit,\n  onEncrypted?: JsxEvent.Media.t => unit,\n  onEncryptedCapture?: JsxEvent.Media.t => unit,\n  onEnded?: JsxEvent.Media.t => unit,\n  onEndedCapture?: JsxEvent.Media.t => unit,\n  onError?: JsxEvent.Media.t => unit,\n  onErrorCapture?: JsxEvent.Media.t => unit,\n  onLoadedData?: JsxEvent.Media.t => unit,\n  onLoadedDataCapture?: JsxEvent.Media.t => unit,\n  onLoadedMetadata?: JsxEvent.Media.t => unit,\n  onLoadedMetadataCapture?: JsxEvent.Media.t => unit,\n  onLoadStart?: JsxEvent.Media.t => unit,\n  onLoadStartCapture?: JsxEvent.Media.t => unit,\n  onPause?: JsxEvent.Media.t => unit,\n  onPauseCapture?: JsxEvent.Media.t => unit,\n  onPlay?: JsxEvent.Media.t => unit,\n  onPlayCapture?: JsxEvent.Media.t => unit,\n  onPlaying?: JsxEvent.Media.t => unit,\n  onPlayingCapture?: JsxEvent.Media.t => unit,\n  onProgress?: JsxEvent.Media.t => unit,\n  onProgressCapture?: JsxEvent.Media.t => unit,\n  onRateChange?: JsxEvent.Media.t => unit,\n  onRateChangeCapture?: JsxEvent.Media.t => unit,\n  onResize?: JsxEvent.Media.t => unit,\n  onResizeCapture?: JsxEvent.Media.t => unit,\n  onSeeked?: JsxEvent.Media.t => unit,\n  onSeekedCapture?: JsxEvent.Media.t => unit,\n  onSeeking?: JsxEvent.Media.t => unit,\n  onSeekingCapture?: JsxEvent.Media.t => unit,\n  onStalled?: JsxEvent.Media.t => unit,\n  onStalledCapture?: JsxEvent.Media.t => unit,\n  onSuspend?: JsxEvent.Media.t => unit,\n  onSuspendCapture?: JsxEvent.Media.t => unit,\n  onTimeUpdate?: JsxEvent.Media.t => unit,\n  onTimeUpdateCapture?: JsxEvent.Media.t => unit,\n  onVolumeChange?: JsxEvent.Media.t => unit,\n  onVolumeChangeCapture?: JsxEvent.Media.t => unit,\n  onWaiting?: JsxEvent.Media.t => unit,\n  onWaitingCapture?: JsxEvent.Media.t => unit,\n  onLoad?: JsxEvent.Image.t => unit,\n  onLoadCapture?: JsxEvent.Image.t => unit,\n  onAnimationStart?: JsxEvent.Animation.t => unit,\n  onAnimationStartCapture?: JsxEvent.Animation.t => unit,\n  onAnimationEnd?: JsxEvent.Animation.t => unit,\n  onAnimationEndCapture?: JsxEvent.Animation.t => unit,\n  onAnimationIteration?: JsxEvent.Animation.t => unit,\n  onAnimationIterationCapture?: JsxEvent.Animation.t => unit,\n  onTransitionCancel?: JsxEvent.Transition.t => unit,\n  onTransitionCancelCapture?: JsxEvent.Transition.t => unit,\n  onTransitionRun?: JsxEvent.Transition.t => unit,\n  onTransitionRunCapture?: JsxEvent.Transition.t => unit,\n  onTransitionStart?: JsxEvent.Transition.t => unit,\n  onTransitionStartCapture?: JsxEvent.Transition.t => unit,\n  onTransitionEnd?: JsxEvent.Transition.t => unit,\n  onTransitionEndCapture?: JsxEvent.Transition.t => unit,\n  accentHeight?: string,\n  accumulate?: string,\n  additive?: string,\n  alignmentBaseline?: string,\n  allowReorder?: string,\n  alphabetic?: string,\n  amplitude?: string,\n  arabicForm?: string,\n  ascent?: string,\n  attributeName?: string,\n  attributeType?: string,\n  autoReverse?: string,\n  azimuth?: string,\n  baseFrequency?: string,\n  baseProfile?: string,\n  baselineShift?: string,\n  bbox?: string,\n  begin?: string,\n  bias?: string,\n  by?: string,\n  calcMode?: string,\n  capHeight?: string,\n  clip?: string,\n  clipPath?: string,\n  clipPathUnits?: string,\n  clipRule?: string,\n  colorInterpolation?: string,\n  colorInterpolationFilters?: string,\n  colorProfile?: string,\n  colorRendering?: string,\n  contentScriptType?: string,\n  contentStyleType?: string,\n  cursor?: string,\n  cx?: string,\n  cy?: string,\n  d?: string,\n  decelerate?: string,\n  descent?: string,\n  diffuseConstant?: string,\n  direction?: string,\n  display?: string,\n  divisor?: string,\n  dominantBaseline?: string,\n  dur?: string,\n  dx?: string,\n  dy?: string,\n  edgeMode?: string,\n  elevation?: string,\n  enableBackground?: string,\n  end?: string,\n  exponent?: string,\n  externalResourcesRequired?: string,\n  fill?: string,\n  fillOpacity?: string,\n  fillRule?: string,\n  filter?: string,\n  filterRes?: string,\n  filterUnits?: string,\n  floodColor?: string,\n  floodOpacity?: string,\n  focusable?: string,\n  fontFamily?: string,\n  fontSize?: string,\n  fontSizeAdjust?: string,\n  fontStretch?: string,\n  fontStyle?: string,\n  fontVariant?: string,\n  fontWeight?: string,\n  fomat?: string,\n  from?: string,\n  fx?: string,\n  fy?: string,\n  g1?: string,\n  g2?: string,\n  glyphName?: string,\n  glyphOrientationHorizontal?: string,\n  glyphOrientationVertical?: string,\n  glyphRef?: string,\n  gradientTransform?: string,\n  gradientUnits?: string,\n  hanging?: string,\n  horizAdvX?: string,\n  horizOriginX?: string,\n  ideographic?: string,\n  imageRendering?: string,\n  @as(\"in\") in_?: string,\n  in2?: string,\n  intercept?: string,\n  k?: string,\n  k1?: string,\n  k2?: string,\n  k3?: string,\n  k4?: string,\n  kernelMatrix?: string,\n  kernelUnitLength?: string,\n  kerning?: string,\n  keyPoints?: string,\n  keySplines?: string,\n  keyTimes?: string,\n  lengthAdjust?: string,\n  letterSpacing?: string,\n  lightingColor?: string,\n  limitingConeAngle?: string,\n  local?: string,\n  markerEnd?: string,\n  markerHeight?: string,\n  markerMid?: string,\n  markerStart?: string,\n  markerUnits?: string,\n  markerWidth?: string,\n  mask?: string,\n  maskContentUnits?: string,\n  maskUnits?: string,\n  mathematical?: string,\n  mode?: string,\n  numOctaves?: string,\n  offset?: string,\n  opacity?: string,\n  operator?: string,\n  order?: string,\n  orient?: string,\n  orientation?: string,\n  origin?: string,\n  overflow?: string,\n  overflowX?: string,\n  overflowY?: string,\n  overlinePosition?: string,\n  overlineThickness?: string,\n  paintOrder?: string,\n  panose1?: string,\n  pathLength?: string,\n  patternContentUnits?: string,\n  patternTransform?: string,\n  patternUnits?: string,\n  pointerEvents?: string,\n  points?: string,\n  pointsAtX?: string,\n  pointsAtY?: string,\n  pointsAtZ?: string,\n  preserveAlpha?: string,\n  preserveAspectRatio?: string,\n  primitiveUnits?: string,\n  r?: string,\n  radius?: string,\n  referrerPolicy?: string,\n  refX?: string,\n  refY?: string,\n  renderingIntent?: string,\n  repeatCount?: string,\n  repeatDur?: string,\n  requiredExtensions?: string,\n  requiredFeatures?: string,\n  restart?: string,\n  result?: string,\n  rotate?: string,\n  rx?: string,\n  ry?: string,\n  scale?: string,\n  seed?: string,\n  shapeRendering?: string,\n  slope?: string,\n  slot?: string,\n  spacing?: string,\n  specularConstant?: string,\n  specularExponent?: string,\n  speed?: string,\n  spreadMethod?: string,\n  startOffset?: string,\n  stdDeviation?: string,\n  stemh?: string,\n  stemv?: string,\n  stitchTiles?: string,\n  stopColor?: string,\n  stopOpacity?: string,\n  strikethroughPosition?: string,\n  strikethroughThickness?: string,\n  string?: string,\n  stroke?: string,\n  strokeDasharray?: string,\n  strokeDashoffset?: string,\n  strokeLinecap?: string,\n  strokeLinejoin?: string,\n  strokeMiterlimit?: string,\n  strokeOpacity?: string,\n  strokeWidth?: string,\n  surfaceScale?: string,\n  systemLanguage?: string,\n  tableValues?: string,\n  targetX?: string,\n  targetY?: string,\n  textAnchor?: string,\n  textDecoration?: string,\n  textLength?: string,\n  textRendering?: string,\n  to?: string,\n  transform?: string,\n  u1?: string,\n  u2?: string,\n  underlinePosition?: string,\n  underlineThickness?: string,\n  unicode?: string,\n  unicodeBidi?: string,\n  unicodeRange?: string,\n  unitsPerEm?: string,\n  vAlphabetic?: string,\n  vHanging?: string,\n  vIdeographic?: string,\n  vMathematical?: string,\n  values?: string,\n  vectorEffect?: string,\n  version?: string,\n  vertAdvX?: string,\n  vertAdvY?: string,\n  vertOriginX?: string,\n  vertOriginY?: string,\n  viewBox?: string,\n  viewTarget?: string,\n  visibility?: string,\n  widths?: string,\n  wordSpacing?: string,\n  writingMode?: string,\n  x?: string,\n  x1?: string,\n  x2?: string,\n  xChannelSelector?: string,\n  xHeight?: string,\n  xlinkActuate?: string,\n  xlinkArcrole?: string,\n  xlinkHref?: string,\n  xlinkRole?: string,\n  xlinkShow?: string,\n  xlinkTitle?: string,\n  xlinkType?: string,\n  xmlns?: string,\n  xmlnsXlink?: string,\n  xmlBase?: string,\n  xmlLang?: string,\n  xmlSpace?: string,\n  y?: string,\n  y1?: string,\n  y2?: string,\n  yChannelSelector?: string,\n  z?: string,\n  zoomAndPan?: string,\n  about?: string,\n  datatype?: string,\n  inlist?: string,\n  prefix?: string,\n  property?: string,\n  resource?: string,\n  typeof?: string,\n  vocab?: string,\n  dangerouslySetInnerHTML?: {\"__html\": string},\n  suppressContentEditableWarning?: bool,\n}\n```\n\n\n---\n```rescript\n@notUndefined type Jsx.element\n```\n\n\n---\n```rescript\ntype domRef\n```\n\n\n---\n```rescript\ntype popover =\n  | @as(\"auto\") Auto\n  | @as(\"manual\") Manual\n  | @as(\"hint\") Hint\n```\n\n\n---\n```rescript\ntype popoverTargetAction =\n  | @as(\"toggle\") Toggle\n  | @as(\"show\") Show\n  | @as(\"hide\") Hide\n```\n\n\n---\n```rescript\ntype style = JsxDOMStyle.t\n```\n\n\n---\n```rescript\ntype JsxEvent.Synthetic.t = synthetic<tag>\n```\n\n\n---\n```rescript\ntype JsxEvent.Toggle.t = synthetic<tag>\n```\n\n\n---\n```rescript\ntype JsxEvent.Clipboard.t = synthetic<tag>\n```\n\n\n---\n```rescript\ntype JsxEvent.Composition.t = synthetic<tag>\n```\n\n\n---\n```rescript\ntype JsxEvent.Keyboard.t = synthetic<tag>\n```\n\n\n---\n```rescript\ntype JsxEvent.Focus.t = synthetic<tag>\n```\n\n\n---\n```rescript\ntype JsxEvent.Form.t = synthetic<tag>\n```\n\n\n---\n```rescript\ntype JsxEvent.Mouse.t = synthetic<tag>\n```\n\n\n---\n```rescript\ntype JsxEvent.Selection.t = synthetic<tag>\n```\n\n\n---\n```rescript\ntype JsxEvent.Touch.t = synthetic<tag>\n```\n\n\n---\n```rescript\ntype JsxEvent.Pointer.t = synthetic<tag>\n```\n\n\n---\n```rescript\ntype JsxEvent.UI.t = synthetic<tag>\n```\n\n\n---\n```rescript\ntype JsxEvent.Wheel.t = synthetic<tag>\n```\n\n\n---\n```rescript\ntype JsxEvent.Media.t = synthetic<tag>\n```\n\n\n---\n```rescript\ntype JsxEvent.Image.t = synthetic<tag>\n```\n\n\n---\n```rescript\ntype JsxEvent.Animation.t = synthetic<tag>\n```\n\n\n---\n```rescript\ntype JsxEvent.Transition.t = synthetic<tag>\n```\n"
      }
    }
    |}]

let%expect_test "shows a JSX component with documentation and optional props" =
  test_jsx_hover ~position:(6, 9)
    ~text:
      {|module M4 = {
  /** Doc Comment For M4 */
  @react.component
  let make = (~first, ~fun="", ~second="") => React.string(first ++ fun ++ second)
}

let _ = <M4 first="abc" />
|};
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nReact.component<M4.props<string, string, string>>\n```\n---\n Doc Comment For M4 \n\n---\n```rescript\ntype React.component<'props> = Jsx.component<'props>\n```\n\n\n---\n```rescript\ntype M4.props<'first, 'fun, 'second> = {\n  first: 'first,\n  fun?: 'fun,\n  second?: 'second,\n}\n```\n"
      }
    }
    |}]

let fragment_component =
  {|module SectionHeader = {
  @react.component
  let make = (~children) => children
}

let z = <> <SectionHeader> {React.string("abc")} </SectionHeader> </>
|}

let%expect_test "shows a component opening tag inside a JSX fragment" =
  test_jsx_hover ~text:fragment_component ~position:(5, 18);
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nReact.component<SectionHeader.props<React.element>>\n```\n\n---\n```rescript\ntype React.component<'props> = Jsx.component<'props>\n```\n\n\n---\n```rescript\ntype SectionHeader.props<'children> = {children: 'children}\n```\n\n\n---\n```rescript\ntype React.element = Jsx.element\n```\n"
      }
    }
    |}]

let%expect_test "shows hover at a component closing tag inside a JSX fragment" =
  test_jsx_hover ~text:fragment_component ~position:(5, 55);
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nReact.component<React.fragmentProps>\n```\n\n---\n```rescript\ntype React.component<'props> = Jsx.component<'props>\n```\n\n\n---\n```rescript\ntype React.fragmentProps = {children?: element}\n```\n"
      }
    }
    |}]

let%expect_test "shows a component prop inside a JSX fragment" =
  test_jsx_hover ~position:(6, 12)
    ~text:
      {|module Comp = {
  @react.component
  let make = (~age) => React.int(age)
}

let _ = {
  <> <Comp age=34 /> </>
}
|};
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nComp.props<int>\n```\n\n---\n```rescript\ntype Comp.props<'age> = {age: 'age}\n```\n"
      }
    }
    |}]

let%expect_test "shows a component prop inside nested JSX fragments" =
  test_jsx_hover ~position:(6, 16)
    ~text:
      {|module Comp = {
  @react.component
  let make = (~age) => React.int(age)
}

let _ = {
  <> {<> <Comp age=34 /> </>} </>
}
|};
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nComp.props<int>\n```\n\n---\n```rescript\ntype Comp.props<'age> = {age: 'age}\n```\n"
      }
    }
    |}]

let children_component =
  {|module Comp = {
  @react.component
  let make = (~children: React.element) => children
}

module Comp1 = Comp

let _ =
  <Comp>
    <div />
    <div />
  </Comp>

let _ =
  <Comp1>
    <div />
    <div />
  </Comp1>
|}

let%expect_test
    "returns no hover after a JSX closing tag with multiple children" =
  test_jsx_hover ~text:children_component ~position:(11, 10);
  [%expect {| <Empty> |}]

let%expect_test "returns no hover after an aliased JSX component closing tag" =
  test_jsx_hover ~text:children_component ~position:(17, 10);
  [%expect {| <Empty> |}]

let%expect_test "infers optional props of a function returning a React element"
    =
  test_jsx_hover ~position:(8, 4)
    ~text:
      {|module CompV4 = {
  type props<'n, 's> = {n?: 'n, s: 's}
  let make = props => {
    let _ = props.n == Some(10)
    React.string(props.s)
  }
}

let mk = CompV4.make
|};
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nCompV4.props<int, string> => React.element\n```\n\n---\n```rescript\ntype CompV4.props<'n, 's> = {n?: 'n, s: 's}\n```\n\n\n---\n```rescript\ntype React.element = Jsx.element\n```\n"
      }
    }
    |}]

let component_implementation =
  {|@react.component
let make = (~first) => React.string(first)
let y = 1
|}

let component_interface =
  {|@react.component
let make: (~first: string) => React.element

let y: int
|}

let%expect_test "shows a React component signature in an interface" =
  test_jsx_interface_hover ~text:component_implementation
    ~interface:component_interface ~position:(1, 4);
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```rescript\nprops<string>\n```\n\n---\n```rescript\ntype props<'first> = {first: 'first}\n```\n"
      }
    }
    |}]

let%expect_test
    "shows a value signature next to a React component in an interface" =
  test_jsx_interface_hover ~text:component_implementation
    ~interface:component_interface ~position:(3, 4);
  [%expect
    {| { "contents": { "kind": "markdown", "value": "```rescript\nint\n```" } } |}]
