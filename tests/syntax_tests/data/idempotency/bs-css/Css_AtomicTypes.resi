// Docs copied from MDN

module Cascading: {
  type t = [#initial | #inherit_ | #unset]

  let initial: [> t]
  let inherit_: [> t]
  let unset: [> t]

  let toString: t => string
}

module Var: {
  type t = [#var(string) | #varDefault(string, string)]

  let var: string => [> t]
  let varDefault: (string, string) => [> t]

  let toString: t => string
}

module Time: {
  @ocaml.doc("
   The <time> CSS data type represents a time value expressed in seconds or milliseconds.
   It is used in animation, transition, and related properties.
   ")
  type t = [#s(float) | #ms(float)]

  let s: float => [> #s(float)]
  let ms: float => [> #ms(float)]

  let toString: t => string
}

module Percentage: {
  @ocaml.doc("
   The <percentage> CSS data type represents a percentage value.
   It is often used to define a size as relative to an element's parent object.
   ")
  type t = [#percent(float)]

  let pct: float => [> t]

  let toString: t => string
}

module Url: {
  @ocaml.doc("
   The <url> CSS data type denotes a pointer to a resource, such as an image or a font.
   URLs can be used in numerous CSS properties, such as background-image, cursor, and list-style.
   ")
  type t = [#url(string)]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/length
 ")
module Length: {
  // calc/percent are incorrect

  @ocaml.doc(" The <length> CSS data type represents a distance value. ")
  type rec t = [
    | #ch(float)
    | #em(float)
    | #ex(float)
    | #rem(float)
    | #vh(float)
    | #vw(float)
    | #vmin(float)
    | #vmax(float)
    | #px(int)
    | #pxFloat(float)
    | #cm(float)
    | #mm(float)
    | #inch(float)
    | #pc(float)
    | #pt(int)
    | #zero
    | #calc([#add | #sub], t, t)
    | #percent(float)
  ]

  @ocaml.doc(
    " Represents the width, or more precisely the advance measure, of the glyph \"0\" (zero, the Unicode character U+0030) in the element's font. "
  )
  let ch: float => [> #ch(float)]
  @ocaml.doc(
    " Represents the calculated font-size of the element. If used on the font-size property itself, it represents the inherited font-size of the element. "
  )
  let em: float => [> #em(float)]
  @ocaml.doc(
    " Represents the x-height of the element's font. On fonts with the \"x\" letter, this is generally the height of lowercase letters in the font; 1ex ≈ 0.5em in many fonts. "
  )
  let ex: float => [> #ex(float)]
  @ocaml.doc(
    " Represents the font-size of the root element (typically <html>). When used within the root element font-size, it represents its initial value (a common browser default is 16px, but user-defined preferences may modify this) "
  )
  let rem: float => [> #rem(float)]
  @ocaml.doc(" Equal to 1% of the height of the viewport's initial containing block ")
  let vh: float => [> #vh(float)]
  @ocaml.doc(" Equal to 1% of the width of the viewport's initial containing block ")
  let vw: float => [> #vw(float)]
  @ocaml.doc(" Equal to the smaller of vw and vh ")
  let vmin: float => [> #vmin(float)]
  @ocaml.doc(" Equal to the larger of vw and vh ")
  let vmax: float => [> #vmax(float)]
  @ocaml.doc(
    " One pixel. For screen displays, it traditionally represents one device pixel (dot). However, for printers and high-resolution screens, one CSS pixel implies multiple device pixels. 1px = 1/96th of 1in "
  )
  let px: int => [> #px(int)]
  let pxFloat: float => [> #pxFloat(float)]
  @ocaml.doc(" One centimeter. 1cm = 96px/2.54 ")
  let cm: float => [> #cm(float)]
  @ocaml.doc(" One millimeter. 1mm = 1/10th of 1cm ")
  let mm: float => [> #mm(float)]
  @ocaml.doc(" One inch. 1in = 2.54cm = 96px ")
  let inch: float => [> #inch(float)]
  @ocaml.doc(" One pica. 1pc = 12pt = 1/6th of 1in ")
  let pc: float => [> #pc(float)]
  @ocaml.doc(" One point. 1pt = 1/72nd of 1in ")
  let pt: int => [> #pt(int)]
  @ocaml.doc(" The value 0 ")
  let zero: [> #zero]

  let toString: t => string
}

module Angle: {
  @ocaml.doc("
   The angle CSS data type represents an angle value expressed in degrees, gradians, radians, or turns.
   It is used, for example, in <gradient>s and in some transform functions.
   ")
  type t = [#deg(float) | #rad(float) | #grad(float) | #turn(float)]

  let deg: float => t
  let rad: float => t
  let grad: float => t
  let turn: float => t

  let toString: t => string
}

module Direction: {
  type t = [#ltr | #rtl]

  @ocaml.doc(" Text and other elements go from left to right. This is the default value. ")
  let ltr: [> t]
  @ocaml.doc(" Text and other elements go from right to left. ")
  let rtl: [> t]

  let toString: t => string
}

module Position: {
  type t = [#absolute | #relative | #static | #fixed | #sticky]

  let absolute: [> t]
  let relative: [> t]
  let static: [> t]
  let fixed: [> t]
  let sticky: [> t]

  let toString: t => string
}

module Resize: {
  type t = [#none | #both | #horizontal | #vertical | #block | #inline]

  @ocaml.doc(" The element offers no user-controllable method for resizing it ")
  let none: [> t]
  @ocaml.doc(
    " The element displays a mechanism for allowing the user to resize it, which may be resized both horizontally and vertically "
  )
  let both: [> t]
  @ocaml.doc(
    " The element displays a mechanism for allowing the user to resize it in the horizontal direction "
  )
  let horizontal: [> t]
  @ocaml.doc(
    " The element displays a mechanism for allowing the user to resize it in the vertical direction "
  )
  let vertical: [> t]
  @ocaml.doc(
    " The element displays a mechanism for allowing the user to resize it in the block direction (either horizontally or vertically, depending on the writing-mode and direction value) "
  )
  let block: [> t]
  @ocaml.doc(
    " The element displays a mechanism for allowing the user to resize it in the inline direction (either horizontally or vertically, depending on the writing-mode and direction value) "
  )
  let inline: [> t]

  let toString: t => string
}

module FontVariant: {
  type t = [#normal | #smallCaps]

  let normal: [> t]
  let smallCaps: [> t]

  let toString: t => string
}

module FontStyle: {
  type t = [#normal | #italic | #oblique]

  let normal: [> t]
  let italic: [> t]
  let oblique: [> t]

  let toString: t => string
}

module FlexBasis: {
  type t = [
    | #auto
    | #fill
    | #content
    | #maxContent
    | #minContent
    | #fitContent
  ]

  let fill: [> t]
  let content: [> t]
  let maxContent: [> t]
  let minContent: [> t]
  let fitContent: [> t]

  let toString: t => string
}

module Overflow: {
  type t = [#hidden | #visible | #scroll | #auto]

  let hidden: [> t]
  let visible: [> t]
  let scroll: [> t]
  let auto: [> t]

  let toString: t => string
}

module Margin: {
  type t = [#auto]

  let auto: [> t]

  let toString: t => string
}

module GridAutoFlow: {
  type t = [#column | #row | #columnDense | #rowDense]

  let toString: t => string
}

module ColumnGap: {
  type t = [#normal]

  let toString: t => string
}

module VerticalAlign: {
  type t = [
    | #baseline
    | #sub
    | #super
    | #top
    | #textTop
    | #middle
    | #bottom
    | #textBottom
  ]

  let toString: t => string
}

module TimingFunction: {
  type t = [
    | #linear
    | #ease
    | #easeIn
    | #easeOut
    | #easeInOut
    | #stepStart
    | #stepEnd
    | #steps(int, [#start | #end_])
    | #cubicBezier(float, float, float, float)
  ]

  let linear: [> t]
  let ease: [> t]
  let easeIn: [> t]
  let easeOut: [> t]
  let easeInOut: [> t]
  let stepStart: [> t]
  let stepEnd: [> t]
  let steps: (int, [#start | #end_]) => [> t]
  let cubicBezier: (float, float, float, float) => [> t]

  let toString: t => string
}

module RepeatValue: {
  type t = [#autoFill | #autoFit | #num(int)]

  let toString: t => string
}

module ListStyleType: {
  type t = [
    | #disc
    | #circle
    | #square
    | #decimal
    | #lowerAlpha
    | #upperAlpha
    | #lowerGreek
    | #lowerLatin
    | #upperLatin
    | #lowerRoman
    | #upperRoman
    | #none
  ]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/list-style-position
 ")
module ListStylePosition: {
  type t = [#inside | #outside]

  let toString: t => string
}

module OutlineStyle: {
  type t = [
    | #none
    | #hidden
    | #dotted
    | #dashed
    | #solid
    | #double
    | #groove
    | #ridge
    | #inset
    | #outset
  ]

  let toString: t => string
}

module FontWeight: {
  /* see https://developer.mozilla.org/docs/Web/CSS/font-weight#Common_weight_name_mapping */
  type t = [
    | #num(int)
    | #thin
    | #extraLight
    | #light
    | #normal
    | #medium
    | #semiBold
    | #bold
    | #extraBold
    | #black
    | #lighter
    | #bolder
  ]

  let thin: [> t]
  let extraLight: [> t]
  let light: [> t]
  let medium: [> t]
  let semiBold: [> t]
  let bold: [> t]
  let extraBold: [> t]
  let lighter: [> t]
  let bolder: [> t]

  let toString: t => string
}

module Transform: {
  type t = [
    | #translate(Length.t, Length.t)
    | #translate3d(Length.t, Length.t, Length.t)
    | #translateX(Length.t)
    | #translateY(Length.t)
    | #translateZ(Length.t)
    | #scale(float, float)
    | #scale3d(float, float, float)
    | #scaleX(float)
    | #scaleY(float)
    | #scaleZ(float)
    | #rotate(Angle.t)
    | #rotate3d(float, float, float, Angle.t)
    | #rotateX(Angle.t)
    | #rotateY(Angle.t)
    | #rotateZ(Angle.t)
    | #skew(Angle.t, Angle.t)
    | #skewX(Angle.t)
    | #skewY(Angle.t)
    | #perspective(int)
  ]

  let translate: (Length.t, Length.t) => [> #translate(Length.t, Length.t)]
  let translate3d: (Length.t, Length.t, Length.t) => [> #translate3d(Length.t, Length.t, Length.t)]
  let translateX: Length.t => [> t]
  let translateY: Length.t => [> t]
  let translateZ: Length.t => [> t]
  let scale: (float, float) => [> t]
  let scale3d: (float, float, float) => [> t]
  let scaleX: float => [> t]
  let scaleY: float => [> t]
  let scaleZ: float => [> t]
  let rotate: Angle.t => [> t]
  let rotate3d: (float, float, float, Angle.t) => [> t]
  let rotateX: Angle.t => [> t]
  let rotateY: Angle.t => [> t]
  let rotateZ: Angle.t => [> t]
  let skew: (Angle.t, Angle.t) => [> t]
  let skewX: Angle.t => [> t]
  let skewY: Angle.t => [> t]

  let toString: t => string
}

module AnimationDirection: {
  type t = [#normal | #reverse | #alternate | #alternateReverse]

  let toString: t => string
}

module AnimationFillMode: {
  type t = [#none | #forwards | #backwards | #both]

  let toString: t => string
}

module AnimationIterationCount: {
  type t = [#infinite | #count(int)]

  let toString: t => string
}

module AnimationPlayState: {
  type t = [#paused | #running]

  let toString: t => string
}

module Cursor: {
  type t = [
    | #auto
    | #default
    | #none
    | #contextMenu
    | #help
    | #pointer
    | #progress
    | #wait
    | #cell
    | #crosshair
    | #text
    | #verticalText
    | #alias
    | #copy
    | #move
    | #noDrop
    | #notAllowed
    | #grab
    | #grabbing
    | #allScroll
    | #colResize
    | #rowResize
    | #nResize
    | #eResize
    | #sResize
    | #wResize
    | #neResize
    | #nwResize
    | #seResize
    | #swResize
    | #ewResize
    | #nsResize
    | #neswResize
    | #nwseResize
    | #zoomIn
    | #zoomOut
  ]

  let auto: [> t]
  let default: [> t]
  let none: [> t]
  let contextMenu: [> t]
  let help: [> t]
  let pointer: [> t]
  let progress: [> t]
  let wait: [> t]
  let cell: [> t]
  let crosshair: [> t]
  let text: [> t]
  let verticalText: [> t]
  let alias: [> t]
  let copy: [> t]
  let move: [> t]
  let noDrop: [> t]
  let notAllowed: [> t]
  let grab: [> t]
  let grabbing: [> t]
  let allScroll: [> t]
  let colResize: [> t]
  let rowResize: [> t]
  let nResize: [> t]
  let eResize: [> t]
  let sResize: [> t]
  let wResize: [> t]
  let neResize: [> t]
  let nwResize: [> t]
  let seResize: [> t]
  let swResize: [> t]
  let ewResize: [> t]
  let nsResize: [> t]
  let neswResize: [> t]
  let nwseResize: [> t]
  let zoomIn: [> t]
  let zoomOut: [> t]

  let toString: t => string
}

module Color: {
  type t = [
    | #rgb(int, int, int)
    | #rgba(int, int, int, [#num(float) | Percentage.t])
    | #hsl(Angle.t, Percentage.t, Percentage.t)
    | #hsla(Angle.t, Percentage.t, Percentage.t, [#num(float) | Percentage.t])
    | #hex(string)
    | #transparent
    | #currentColor
  ]

  let rgb: (int, int, int) => [> t]
  let rgba: (int, int, int, [#num(float) | Percentage.t]) => [> t]
  let hsl: (Angle.t, Percentage.t, Percentage.t) => [> t]
  let hsla: (Angle.t, Percentage.t, Percentage.t, [#num(float) | Percentage.t]) => [> t]
  let hex: string => [> t]
  let transparent: [> t]
  let currentColor: [> t]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/border-style
 ")
module BorderStyle: {
  type t = [
    | #none
    | #hidden
    | #dotted
    | #dashed
    | #solid
    | #double
    | #groove
    | #ridge
    | #inset
    | #outset
  ]

  let toString: t => string
}

module PointerEvents: {
  type t = [#auto | #none]

  let toString: t => string
}

module Perspective: {
  type t = [#none]

  let toString: t => string
}

module LetterSpacing: {
  type t = [#normal]

  @ocaml.doc("
   The normal letter spacing for the current font.
   Unlike a value of 0, this keyword allows the user agent to alter the space between characters in order to justify text.
   ")
  let normal: [> t]

  let toString: t => string
}

module LineHeight: {
  type t = [#normal | #abs(float)]

  let toString: t => string
}

module WordSpacing: {
  type t = [#normal]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/display
 ")
module DisplayOutside: {
  type t = [#block | #inline | #runIn]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/display
 ")
module DisplayInside: {
  type t = [#table | #flex | #grid]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/display
 ")
module DisplayListItem: {
  type t = [#listItem]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/display
 ")
module DisplayInternal: {
  type t = [
    | #tableRowGroup
    | #tableHeaderGroup
    | #tableFooterGroup
    | #tableRow
    | #tableCell
    | #tableColumnGroup
    | #tableColumn
    | #tableCaption
  ]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/display-box
 ")
module DisplayBox: {
  type t = [#contents | #none]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/display
 ")
module DisplayLegacy: {
  type t = [#inlineBlock | #inlineFlex | #inlineGrid | #inlineTable]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/justify-self
 ")
module JustifySelf: {
  type t = [#auto | #normal | #stretch]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/justify-self
 https://developer.mozilla.org/docs/Web/CSS/justify-content
 ")
module PositionalAlignment: {
  type t = [
    | #center
    | #start
    | #end_
    | #flexStart
    | #flexEnd
    | #selfStart
    | #selfEnd
    | #left
    | #right
  ]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Box_Alignment#Overflow_alignment
 ")
module OverflowAlignment: {
  type t = [
    | #safe(PositionalAlignment.t)
    | #unsafe(PositionalAlignment.t)
  ]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Box_Alignment#Baseline_alignment
 ")
module BaselineAlignment: {
  type t = [#baseline | #firstBaseline | #lastBaseline]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/justify-content
 ")
module NormalAlignment: {
  type t = [#normal]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/justify-content
 ")
module DistributedAlignment: {
  type t = [#spaceBetween | #spaceAround | #spaceEvenly | #stretch]

  let toString: t => string
}

@ocaml.doc("
 https://drafts.csswg.org/css-align-3/#propdef-justify-items
 ")
module LegacyAlignment: {
  type t = [#legacy | #legacyRight | #legacyLeft | #legacyCenter]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/text-align
 ")
module TextAlign: {
  type t = [#left | #right | #center | #justify]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/word-break
 ")
module WordBreak: {
  type t = [#normal | #breakAll | #keepAll]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/white-space
 ")
module WhiteSpace: {
  type t = [#normal | #nowrap | #pre | #preLine | #preWrap | #breakSpaces]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/align-items
 ")
module AlignItems: {
  type t = [#normal | #stretch]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/align-self
 ")
module AlignSelf: {
  type t = [#auto | #normal | #stretch]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/align-content
 ")
module AlignContent: {
  type t = [#center | #start | #end_ | #flexStart | #flexEnd]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/object-fit
 ")
module ObjectFit: {
  type t = [#fill | #contain | #cover | #none | #scaleDown]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/clear
 ")
module Clear: {
  type t = [#none | #left | #right | #both | #inlineStart | #inlineEnd]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/float
 ")
module Float: {
  type t = [#left | #right | #none | #inlineStart | #inlineEnd]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/visibility
 ")
module Visibility: {
  type t = [#visible | #hidden | #collapse]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/table-layout
 ")
module TableLayout: {
  type t = [#auto | #fixed]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/border-collapse
 ")
module BorderCollapse: {
  type t = [#collapse | #separate]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/flex-wrap
 ")
module FlexWrap: {
  type t = [#nowrap | #wrap | #wrapReverse]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/flex-direction
 ")
module FlexDirection: {
  type t = [#row | #rowReverse | #column | #columnReverse]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/box-sizing
 ")
module BoxSizing: {
  type t = [#contentBox | #borderBox]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/box-sizing
 ")
module ColumnCount: {
  type t = [#auto | #count(int)]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/user-select
 ")
module UserSelect: {
  type t = [#none | #auto | #text | #contain | #all]

  let toString: t => string
}

module TextTransform: {
  type t = [#none | #capitalize | #uppercase | #lowercase]

  let toString: t => string
}

module GridTemplateAreas: {
  type t = [#none | #areas(list<string>)]

  let areas: list<string> => [> t]

  let toString: t => string
}

module GridArea: {
  type t = [
    | #auto
    | #ident(string)
    | #num(int)
    | #numIdent(int, string)
    | #span([#num(int) | #ident(string)])
  ]

  let auto: [> t]
  let ident: string => [> t]
  let num: int => [> t]
  let numIdent: (int, string) => [> t]
  let span: [#num(int) | #ident(string)] => [> t]

  let toString: t => string
}

@ocaml.doc("
  https://developer.mozilla.org/en-US/docs/Web/CSS/backdrop-filter
  ")
module BackdropFilter: {
  type t = [
    | #blur(Length.t)
    | #brightness([#num(int) | #percent(float)])
    | #contrast([#num(int) | #percent(float)])
    | #dropShadow([#num(int) | #percent(float)])
    | #grayscale([#num(int) | #percent(float)])
    | #hueRotate([Angle.t | #zero])
    | #invert([#num(int) | #percent(float)])
    | #none
    | #opacity([#num(int) | #percent(float)])
    | #saturate([#num(int) | #percent(float)])
    | #sepia([#num(int) | #percent(float)])
  ]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/background-attachment
 ")
module BackgroundAttachment: {
  type t = [#scroll | #fixed | #local]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/background-clip
 ")
module BackgroundClip: {
  type t = [#borderBox | #paddingBox | #contentBox]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/background-origin
 ")
module BackgroundOrigin: {
  type t = [#borderBox | #paddingBox | #contentBox]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/background-position
 ")
module BackgroundPosition: {
  module X: {
    type t = [#left | #right | #center]

    let toString: t => string
  }

  module Y: {
    type t = [#top | #bottom | #center]

    let toString: t => string
  }

  type t = [X.t | Y.t]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/background-origin
 ")
module BackgroundRepeat: {
  type twoValue = [#repeat | #space | #round | #noRepeat]
  type t = [#repeatX | #repeatY | twoValue]
  type horizontal = twoValue
  type vertical = twoValue

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/text-overflow
 ")
module TextOverflow: {
  type t = [#clip | #ellipsis | #string(string)]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/text-decoration-line
 ")
module TextDecorationLine: {
  type t = [#none | #underline | #overline | #lineThrough | #blink]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/text-decoration-style
 ")
module TextDecorationStyle: {
  type t = [#solid | #double | #dotted | #dashed | #wavy]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/width
 ")
module Width: {
  type t = [#auto | #fitContent]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/max-width
 ")
module MaxWidth: {
  type t = [#none]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/height
 ")
module Height: {
  type t = [#auto]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/max-height
 ")
module MaxHeight: {
  type t = [#none]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/overflow-wrap
 ")
module OverflowWrap: {
  type t = [#normal | #breakWord | #anywhere]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/gradient
 ")
module Gradient: {
  type t<'colorOrVar> = [
    | #linearGradient(Angle.t, list<(Length.t, [< Color.t | Var.t] as 'colorOrVar)>)
    | #repeatingLinearGradient(Angle.t, list<(Length.t, [< Color.t | Var.t] as 'colorOrVar)>)
    | #radialGradient(list<(Length.t, [< Color.t | Var.t] as 'colorOrVar)>)
    | #repeatingRadialGradient(list<(Length.t, [< Color.t | Var.t] as 'colorOrVar)>)
  ]

  @ocaml.doc(" Linear gradients transition colors progressively along an imaginary line. ")
  let linearGradient: (
    Angle.t,
    list<(Length.t, [< Color.t | Var.t] as 'colorOrVar)>,
  ) => [> t<'colorOrVar>]
  @ocaml.doc(" Radial gradients transition colors progressively from a center point (origin). ")
  let radialGradient: list<(Length.t, [< Color.t | Var.t] as 'colorOrVar)> => [> t<'colorOrVar>]

  @ocaml.doc(
    " Repeating gradients duplicate a gradient as much as necessary to fill a given area (linearGradient function). "
  )
  let repeatingLinearGradient: (
    Angle.t,
    list<(Length.t, [< Color.t | Var.t] as 'colorOrVar)>,
  ) => [> t<'colorOrVar>]
  @ocaml.doc(
    " Repeating gradients duplicate a gradient as much as necessary to fill a given area (radialGradient function). "
  )
  let repeatingRadialGradient: list<(Length.t, [< Color.t | Var.t] as 'colorOrVar)> => [>
    | t<'colorOrVar>
  ]

  let toString: t<[< Color.t | Var.t]> => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/background-image
 ")
module BackgroundImage: {
  type t = [#none]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/clip-path
 ")
module GeometyBox: {
  type t = [
    | #marginBox
    | #borderBox
    | #paddingBox
    | #contentBox
    | #fillBox
    | #strokeBox
    | #viewBox
  ]

  @ocaml.doc(" Uses the margin box as the reference box. ")
  let marginBox: [> t]
  @ocaml.doc(" Uses the border box as the reference box. ")
  let borderBox: [> t]
  @ocaml.doc(" Uses the padding box as the reference box. ")
  let paddingBox: [> t]
  @ocaml.doc(" Uses the content box as the reference box. ")
  let contentBox: [> t]
  @ocaml.doc(" Uses the object bounding box as the reference box. ")
  let fillBox: [> t]
  @ocaml.doc(" Uses the stroke bounding box as the reference box. ")
  let strokeBox: [> t]
  @ocaml.doc("
   Uses the nearest SVG viewport as the reference box.
   If a viewBox attribute is specified for the element creating the SVG viewport,
   the reference box is positioned at the origin of the coordinate system established by the viewBox attribute and
   the dimension of the size of the reference box is set to the width and height values of the viewBox attribute.
   ")
  let viewBox: [> t]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/clip-path
 ")
module ClipPath: {
  type t = [#none]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/backface-visibility
 ")
module BackfaceVisibility: {
  type t = [#visible | #hidden]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/flex
 ")
module Flex: {
  type t = [#auto | #initial | #none]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/transform-style
 ")
module TransformStyle: {
  type t = [#preserve3d | #flat]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/list-style-image
 ")
module ListStyleImage: {
  type t = [#none]

  let toString: t => string
}

@ocaml.doc("
 https://developer.mozilla.org/docs/Web/CSS/font-family
 ")
module FontFamilyName: {
  type t = [
    | #custom(string)
    | #serif
    | #sansSerif
    | #cursive
    | #fantasy
    | #monospace
    | #systemUi
    | #emoji
    | #math
    | #fangsong
  ]

  let toString: t => string
}

@ocaml.doc("
 * https://developer.mozilla.org/fr/docs/Web/CSS/@font-face/font-display
 ")
module FontDisplay: {
  type t = [#auto | #block | #swap | #fallback | #optional]

  let toString: t => string
}

@ocaml.doc("
 * https://developer.mozilla.org/en-US/docs/Web/CSS/counters
 ")
module CounterStyleType: {
  type t = [ListStyleType.t]

  let toString: t => string
}

@ocaml.doc("
 * https://developer.mozilla.org/en-US/docs/Web/CSS/counter
 ")
module Counter: {
  type style = [CounterStyleType.t | #unset]
  type t = [#counter(string, style)]

  let counter: (~style: style=?, string) => t

  let toString: t => string
}

@ocaml.doc("
 * https://developer.mozilla.org/en-US/docs/Web/CSS/counters
 ")
module Counters: {
  type style = [CounterStyleType.t | #unset]
  type t = [#counters(string, string, style)]

  let counters: (~style: style=?, ~separator: string=?, string) => t

  let toString: t => string
}

@ocaml.doc("
 * https://developer.mozilla.org/en-US/docs/Web/CSS/counter-increment
 ")
module CounterIncrement: {
  type t = [#none | #increment(string, int)]

  let increment: (~value: int=?, string) => t

  let toString: t => string
}

@ocaml.doc("
 * https://developer.mozilla.org/en-US/docs/Web/CSS/counter-reset
 ")
module CounterReset: {
  type t = [#none | #reset(string, int)]

  let reset: (~value: int=?, string) => t

  let toString: t => string
}

@ocaml.doc("
 * https://developer.mozilla.org/en-US/docs/Web/CSS/counter-set
 ")
module CounterSet: {
  type t = [#none | #set(string, int)]

  let set: (~value: int=?, string) => t

  let toString: t => string
}

@ocaml.doc("
 * https://developer.mozilla.org/en-US/docs/Web/CSS/content
 ")
module Content: {
  type t = [
    | #none
    | #normal
    | #openQuote
    | #closeQuote
    | #noOpenQuote
    | #noCloseQuote
    | #attr(string)
    | #text(string)
  ]

  let toString: t => string
}

module SVG: {
  module Fill: {
    type t = [#none | #contextFill | #contextStroke]

    let contextFill: [> t]
    let contextStroke: [> t]

    let toString: t => string
  }
}
