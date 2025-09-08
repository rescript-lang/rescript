let join = (strings, separator) => {
  let rec run = (strings, acc) =>
    switch strings {
    | list{} => acc
    | list{x} => acc ++ x
    | list{x, ...xs} => run(xs, acc ++ (x ++ separator))
    }
  run(strings, "")
}

module Cascading = {
  type t = [#initial | #inherit_ | #unset]

  let initial = #initial
  let inherit_ = #inherit_
  let unset = #unset

  let toString = x =>
    switch x {
    | #initial => "initial"
    | #inherit_ => "inherit"
    | #unset => "unset"
    }
}

module Var = {
  type t = [#var(string) | #varDefault(string, string)]

  let var = x => #var(x)
  let varDefault = (x, default) => #varDefault(x, default)

  let prefix = x => Js.String.startsWith("--", x) ? x : "--" ++ x

  let toString = x =>
    switch x {
    | #var(x) => "var(" ++ (prefix(x) ++ ")")
    | #varDefault(x, v) => "var(" ++ (prefix(x) ++ ("," ++ (v ++ ")")))
    }
}

module Time = {
  type t = [#s(float) | #ms(float)]

  let s = x => #s(x)
  let ms = x => #ms(x)

  let toString = x =>
    switch x {
    | #s(v) => Js.Float.toString(v) ++ "s"
    | #ms(v) => Js.Float.toString(v) ++ "ms"
    }
}

module Percentage = {
  type t = [#percent(float)]

  let pct = x => #percent(x)

  let toString = x =>
    switch x {
    | #percent(x) => Js.Float.toString(x) ++ "%"
    }
}

module Url = {
  type t = [#url(string)]

  let toString = x =>
    switch x {
    | #url(s) => "url(" ++ (s ++ ")")
    }
}

module Length = {
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

  let ch = x => #ch(x)
  let em = x => #em(x)
  let ex = x => #ex(x)
  let rem = x => #rem(x)
  let vh = x => #vh(x)
  let vw = x => #vw(x)
  let vmin = x => #vmin(x)
  let vmax = x => #vmax(x)
  let px = x => #px(x)
  let pxFloat = x => #pxFloat(x)
  let cm = x => #cm(x)
  let mm = x => #mm(x)
  let inch = x => #inch(x)
  let pc = x => #pc(x)
  let pt = x => #pt(x)
  let zero = #zero

  let rec toString = x =>
    switch x {
    | #ch(x) => Js.Float.toString(x) ++ "ch"
    | #em(x) => Js.Float.toString(x) ++ "em"
    | #ex(x) => Js.Float.toString(x) ++ "ex"
    | #rem(x) => Js.Float.toString(x) ++ "rem"
    | #vh(x) => Js.Float.toString(x) ++ "vh"
    | #vw(x) => Js.Float.toString(x) ++ "vw"
    | #vmin(x) => Js.Float.toString(x) ++ "vmin"
    | #vmax(x) => Js.Float.toString(x) ++ "vmax"
    | #px(x) => Js.Int.toString(x) ++ "px"
    | #pxFloat(x) => Js.Float.toString(x) ++ "px"
    | #cm(x) => Js.Float.toString(x) ++ "cm"
    | #mm(x) => Js.Float.toString(x) ++ "mm"
    | #inch(x) => Js.Float.toString(x) ++ "in"
    | #pc(x) => Js.Float.toString(x) ++ "pc"
    | #pt(x) => Js.Int.toString(x) ++ "pt"
    | #zero => "0"

    | #calc(#add, a, b) => "calc(" ++ (toString(a) ++ (" + " ++ (toString(b) ++ ")")))
    | #calc(#sub, a, b) => "calc(" ++ (toString(a) ++ (" - " ++ (toString(b) ++ ")")))
    | #percent(x) => Js.Float.toString(x) ++ "%"
    }
}

module Angle = {
  type t = [#deg(float) | #rad(float) | #grad(float) | #turn(float)]

  let deg = (x: float) => #deg(x)
  let rad = (x: float) => #rad(x)
  let grad = (x: float) => #grad(x)
  let turn = (x: float) => #turn(x)

  let toString = x =>
    switch x {
    | #deg(x) => Js.Float.toString(x) ++ "deg"
    | #rad(x) => Js.Float.toString(x) ++ "rad"
    | #grad(x) => Js.Float.toString(x) ++ "grad"
    | #turn(x) => Js.Float.toString(x) ++ "turn"
    }
}

module Direction = {
  type t = [#ltr | #rtl]

  let ltr = #ltr
  let rtl = #rtl

  let toString = x =>
    switch x {
    | #ltr => "ltr"
    | #rtl => "rtl"
    }
}

module Position = {
  type t = [#absolute | #relative | #static | #fixed | #sticky]

  let absolute = #absolute
  let relative = #relative
  let static = #static
  let fixed = #fixed
  let sticky = #sticky

  let toString = x =>
    switch x {
    | #absolute => "absolute"
    | #relative => "relative"
    | #static => "static"
    | #fixed => "fixed"
    | #sticky => "sticky"
    }
}

module Resize = {
  type t = [#none | #both | #horizontal | #vertical | #block | #inline]

  let none = #none
  let both = #both
  let horizontal = #horizontal
  let vertical = #vertical
  let block = #block
  let inline = #inline

  let toString = x =>
    switch x {
    | #none => "none"
    | #both => "both"
    | #horizontal => "horizontal"
    | #vertical => "vertical"
    | #block => "block"
    | #inline => "inline"
    }
}

module FontVariant = {
  type t = [#normal | #smallCaps]

  let normal = #normal
  let smallCaps = #smallCaps

  let toString = x =>
    switch x {
    | #normal => "normal"
    | #smallCaps => "smallCaps"
    }
}

module FontStyle = {
  type t = [#normal | #italic | #oblique]

  let normal = #normal
  let italic = #italic
  let oblique = #oblique

  let toString = x =>
    switch x {
    | #normal => "normal"
    | #italic => "italic"
    | #oblique => "oblique"
    }
}

module FlexBasis = {
  type t = [
    | #auto
    | #fill
    | #content
    | #maxContent
    | #minContent
    | #fitContent
  ]

  let fill = #fill
  let content = #content
  let maxContent = #maxContent
  let minContent = #minContent
  let fitContent = #fitContent

  let toString = x =>
    switch x {
    | #auto => "auto"
    | #fill => "fill"
    | #content => "content"
    | #maxContent => "max-content"
    | #minContent => "min-content"
    | #fitContent => "fit-content"
    }
}

module Overflow = {
  type t = [#hidden | #visible | #scroll | #auto]

  let hidden = #hidden
  let visible = #visible
  let scroll = #scroll
  let auto = #auto

  let toString = x =>
    switch x {
    | #hidden => "hidden"
    | #visible => "visible"
    | #scroll => "scroll"
    | #auto => "auto"
    }
}

module Margin = {
  type t = [#auto]

  let auto = #auto

  let toString = x =>
    switch x {
    | #auto => "auto"
    }
}

module GridAutoFlow = {
  type t = [#column | #row | #columnDense | #rowDense]

  let toString = x =>
    switch x {
    | #column => "column"
    | #row => "row"
    | #columnDense => "column dense"
    | #rowDense => "row dense"
    }
}

module ColumnGap = {
  type t = [#normal]

  let toString = x =>
    switch x {
    | #normal => "normal"
    }
}

module VerticalAlign = {
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

  let toString = x =>
    switch x {
    | #baseline => "baseline"
    | #sub => "sub"
    | #super => "super"
    | #top => "top"
    | #textTop => "text-top"
    | #middle => "middle"
    | #bottom => "bottom"
    | #textBottom => "text-bottom"
    }
}

module TimingFunction = {
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

  let linear = #linear
  let ease = #ease
  let easeIn = #easeIn
  let easeInOut = #easeInOut
  let easeOut = #easeOut
  let stepStart = #stepStart
  let stepEnd = #stepEnd
  let steps = (i, dir) => #steps(i, dir)
  let cubicBezier = (a, b, c, d) => #cubicBezier(a, b, c, d)

  let toString = x =>
    switch x {
    | #linear => "linear"
    | #ease => "ease"
    | #easeIn => "ease-in"
    | #easeOut => "ease-out"
    | #easeInOut => "ease-in-out"
    | #stepStart => "step-start"
    | #stepEnd => "step-end"
    | #steps(i, #start) => "steps(" ++ (Js.Int.toString(i) ++ ", start)")
    | #steps(i, #end_) => "steps(" ++ (Js.Int.toString(i) ++ ", end)")
    | #cubicBezier(a, b, c, d) =>
      "cubic-bezier(" ++
      (Js.Float.toString(a) ++
      (", " ++
      (Js.Float.toString(b) ++
      (", " ++ (Js.Float.toString(c) ++ (", " ++ (Js.Float.toString(d) ++ ")")))))))
    }
}

module RepeatValue = {
  type t = [#autoFill | #autoFit | #num(int)]

  let toString = x =>
    switch x {
    | #autoFill => "auto-fill"
    | #autoFit => "auto-fit"
    | #num(x) => Js.Int.toString(x)
    }
}

module ListStyleType = {
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

  let toString = x =>
    switch x {
    | #disc => "disc"
    | #circle => "circle"
    | #square => "square"
    | #decimal => "decimal"
    | #lowerAlpha => "lower-alpha"
    | #upperAlpha => "upper-alpha"
    | #lowerGreek => "lower-greek"
    | #lowerLatin => "lower-latin"
    | #upperLatin => "upper-latin"
    | #lowerRoman => "lower-roman"
    | #upperRoman => "upper-roman"
    | #none => "none"
    }
}

module ListStylePosition = {
  type t = [#inside | #outside]

  let toString = x =>
    switch x {
    | #inside => "inside"
    | #outside => "outside"
    }
}

module OutlineStyle = {
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

  let toString = x =>
    switch x {
    | #none => "none"
    | #hidden => "hidden"
    | #dotted => "dotted"
    | #dashed => "dashed"
    | #solid => "solid"
    | #double => "double"
    | #groove => "grove"
    | #ridge => "ridge"
    | #inset => "inset"
    | #outset => "outset"
    }
}

module FontWeight = {
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

  let thin = #thin
  let extraLight = #extraLight
  let light = #light
  let medium = #medium
  let semiBold = #semiBold
  let bold = #bold
  let extraBold = #extraBold
  let lighter = #lighter
  let bolder = #bolder

  let toString = x =>
    switch x {
    | #num(n) => Js.Int.toString(n)
    | #thin => "100"
    | #extraLight => "200"
    | #light => "300"
    | #normal => "400"
    | #medium => "500"
    | #semiBold => "600"
    | #bold => "700"
    | #extraBold => "800"
    | #black => "900"
    | #lighter => "lighter"
    | #bolder => "bolder"
    }
}

module Transform = {
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

  let translate = (x, y) => #translate(x, y)
  let translate3d = (x, y, z) => #translate3d(x, y, z)
  let translateX = x => #translateX(x)
  let translateY = y => #translateY(y)
  let translateZ = z => #translateZ(z)
  let scale = (x, y) => #scale(x, y)
  let scale3d = (x, y, z) => #scale3d(x, y, z)
  let scaleX = x => #scaleX(x)
  let scaleY = x => #scaleY(x)
  let scaleZ = x => #scaleZ(x)
  let rotate = a => #rotate(a)
  let rotate3d = (x, y, z, a) => #rotate3d(x, y, z, a)
  let rotateX = a => #rotateX(a)
  let rotateY = a => #rotateY(a)
  let rotateZ = a => #rotateZ(a)
  let skew = (a, a') => #skew(a, a')
  let skewX = a => #skewX(a)
  let skewY = a => #skewY(a)

  let string_of_scale = (x, y) =>
    "scale(" ++ (Js.Float.toString(x) ++ (", " ++ (Js.Float.toString(y) ++ ")")))

  let string_of_translate3d = (x, y, z) =>
    "translate3d(" ++
    (Length.toString(x) ++
    (", " ++ (Length.toString(y) ++ (", " ++ (Length.toString(z) ++ ")")))))

  let toString = x =>
    switch x {
    | #translate(x, y) =>
      "translate(" ++ (Length.toString(x) ++ (", " ++ (Length.toString(y) ++ ")")))
    | #translate3d(x, y, z) => string_of_translate3d(x, y, z)
    | #translateX(x) => "translateX(" ++ (Length.toString(x) ++ ")")
    | #translateY(y) => "translateY(" ++ (Length.toString(y) ++ ")")
    | #translateZ(z) => "translateZ(" ++ (Length.toString(z) ++ ")")
    | #scale(x, y) => string_of_scale(x, y)
    | #scale3d(x, y, z) =>
      "scale3d(" ++
      (Js.Float.toString(x) ++
      (", " ++ (Js.Float.toString(y) ++ (", " ++ (Js.Float.toString(z) ++ ")")))))
    | #scaleX(x) => "scaleX(" ++ (Js.Float.toString(x) ++ ")")
    | #scaleY(y) => "scaleY(" ++ (Js.Float.toString(y) ++ ")")
    | #scaleZ(z) => "scaleZ(" ++ (Js.Float.toString(z) ++ ")")
    | #rotate(a) => "rotate(" ++ (Angle.toString(a) ++ ")")
    | #rotate3d(x, y, z, a) =>
      "rotate3d(" ++
      (Js.Float.toString(x) ++
      (", " ++
      (Js.Float.toString(y) ++
      (", " ++ (Js.Float.toString(z) ++ (", " ++ (Angle.toString(a) ++ ")")))))))
    | #rotateX(a) => "rotateX(" ++ (Angle.toString(a) ++ ")")
    | #rotateY(a) => "rotateY(" ++ (Angle.toString(a) ++ ")")
    | #rotateZ(a) => "rotateZ(" ++ (Angle.toString(a) ++ ")")
    | #skew(x, y) => "skew(" ++ (Angle.toString(x) ++ (", " ++ (Angle.toString(y) ++ ")")))
    | #skewX(a) => "skewX(" ++ (Angle.toString(a) ++ ")")
    | #skewY(a) => "skewY(" ++ (Angle.toString(a) ++ ")")
    | #perspective(x) => "perspective(" ++ (Js.Int.toString(x) ++ ")")
    }
}

module AnimationDirection = {
  type t = [#normal | #reverse | #alternate | #alternateReverse]

  let toString = x =>
    switch x {
    | #normal => "normal"
    | #reverse => "reverse"
    | #alternate => "alternate"
    | #alternateReverse => "alternate-reverse"
    }
}

module AnimationFillMode = {
  type t = [#none | #forwards | #backwards | #both]

  let toString = x =>
    switch x {
    | #none => "none"
    | #forwards => "forwards"
    | #backwards => "backwards"
    | #both => "both"
    }
}

module AnimationIterationCount = {
  type t = [#infinite | #count(int)]

  let toString = x =>
    switch x {
    | #infinite => "infinite"
    | #count(x) => Js.Int.toString(x)
    }
}

module AnimationPlayState = {
  type t = [#paused | #running]

  let toString = x =>
    switch x {
    | #paused => "paused"
    | #running => "running"
    }
}

module Cursor = {
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

  let auto = #auto
  let default = #default
  let none = #none
  let contextMenu = #contextMenu
  let help = #help
  let pointer = #pointer
  let progress = #progress
  let wait = #wait
  let cell = #cell
  let crosshair = #crosshair
  let text = #text
  let verticalText = #verticalText
  let alias = #alias
  let copy = #copy
  let move = #move
  let noDrop = #noDrop
  let notAllowed = #notAllowed
  let grab = #grab
  let grabbing = #grabbing
  let allScroll = #allScroll
  let colResize = #colResize
  let rowResize = #rowResize
  let nResize = #nResize
  let eResize = #eResize
  let sResize = #sResize
  let wResize = #wResize
  let neResize = #neResize
  let nwResize = #nwResize
  let seResize = #seResize
  let swResize = #swResize
  let ewResize = #ewResize
  let nsResize = #nsResize
  let neswResize = #neswResize
  let nwseResize = #nwseResize
  let zoomIn = #zoomIn
  let zoomOut = #zoomOut

  let toString = x =>
    switch x {
    | #auto => "auto"
    | #default => "default"
    | #none => "none"
    | #contextMenu => "context-menu"
    | #help => "help"
    | #pointer => "pointer"
    | #progress => "progress"
    | #wait => "wait"
    | #cell => "cell"
    | #crosshair => "crosshair"
    | #text => "text"
    | #verticalText => "vertical-text"
    | #alias => "alias"
    | #copy => "copy"
    | #move => "move"
    | #noDrop => "no-drop"
    | #notAllowed => "not-allowed"
    | #grab => "grab"
    | #grabbing => "grabbing"
    | #allScroll => "all-scroll"
    | #colResize => "col-resize"
    | #rowResize => "row-resize"
    | #nResize => "n-resize"
    | #eResize => "e-resize"
    | #sResize => "s-resize"
    | #wResize => "w-resize"
    | #neResize => "ne-resize"
    | #nwResize => "nw-resize"
    | #seResize => "se-resize"
    | #swResize => "sw-resize"
    | #ewResize => "ew-resize"
    | #nsResize => "ns-resize"
    | #neswResize => "nesw-resize"
    | #nwseResize => "nwse-resize"
    | #zoomIn => "zoom-in"
    | #zoomOut => "zoom-out"
    }
}

module Color = {
  type t = [
    | #rgb(int, int, int)
    | #rgba(int, int, int, [#num(float) | Percentage.t])
    | #hsl(Angle.t, Percentage.t, Percentage.t)
    | #hsla(Angle.t, Percentage.t, Percentage.t, [#num(float) | Percentage.t])
    | #hex(string)
    | #transparent
    | #currentColor
  ]

  let rgb = (r, g, b) => #rgb(r, g, b)
  let rgba = (r, g, b, a) => #rgba(r, g, b, a)
  let hsl = (h, s, l) => #hsl(h, s, l)
  let hsla = (h, s, l, a) => #hsla(h, s, l, a)
  let hex = x => #hex(x)
  let transparent = #transparent
  let currentColor = #currentColor

  let string_of_alpha = x =>
    switch x {
    | #num(f) => Js.Float.toString(f)
    | #...Percentage.t as pc => Percentage.toString(pc)
    }

  let toString = x =>
    switch x {
    | #rgb(r, g, b) =>
      "rgb(" ++
      (Js.Int.toString(r) ++
      (", " ++ (Js.Int.toString(g) ++ (", " ++ (Js.Int.toString(b) ++ ")")))))
    | #rgba(r, g, b, a) =>
      "rgba(" ++
      (Js.Int.toString(r) ++
      (", " ++
      (Js.Int.toString(g) ++
      (", " ++ (Js.Int.toString(b) ++ (", " ++ (string_of_alpha(a) ++ ")")))))))
    | #hsl(h, s, l) =>
      "hsl(" ++
      (Angle.toString(h) ++
      (", " ++ (Percentage.toString(s) ++ (", " ++ (Percentage.toString(l) ++ ")")))))
    | #hsla(h, s, l, a) =>
      "hsla(" ++
      (Angle.toString(h) ++
      (", " ++
      (Percentage.toString(s) ++
      (", " ++ (Percentage.toString(l) ++ (", " ++ (string_of_alpha(a) ++ ")")))))))
    | #hex(s) => "#" ++ s
    | #transparent => "transparent"
    | #currentColor => "currentColor"
    }
}

module BorderStyle = {
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

  let toString = x =>
    switch x {
    | #none => "none"
    | #hidden => "hidden"
    | #dotted => "dotted"
    | #dashed => "dashed"
    | #solid => "solid"
    | #double => "double"
    | #groove => "groove"
    | #ridge => "ridge"
    | #inset => "inset"
    | #outset => "outset"
    }
}

module PointerEvents = {
  type t = [#auto | #none]

  let toString = x =>
    switch x {
    | #auto => "auto"
    | #none => "none"
    }
}

module Perspective = {
  type t = [#none]

  let toString = x =>
    switch x {
    | #none => "none"
    }
}

module LetterSpacing = {
  type t = [#normal]

  let normal = #normal

  let toString = x =>
    switch x {
    | #normal => "normal"
    }
}

module LineHeight = {
  type t = [#normal | #abs(float)]

  let toString = x =>
    switch x {
    | #normal => "normal"
    | #abs(x) => Js.Float.toString(x)
    }
}

module WordSpacing = {
  type t = [#normal]

  let toString = x =>
    switch x {
    | #normal => "normal"
    }
}

module DisplayOutside = {
  type t = [#block | #inline | #runIn]

  let toString = x =>
    switch x {
    | #block => "block"
    | #inline => "inline"
    | #runIn => "run-in"
    }
}

module DisplayInside = {
  type t = [#table | #flex | #grid]

  let toString = x =>
    switch x {
    | #table => "table"
    | #flex => "flex"
    | #grid => "grid"
    }
}

module DisplayListItem = {
  type t = [#listItem]

  let toString = x =>
    switch x {
    | #listItem => "list-item"
    }
}

module DisplayInternal = {
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

  let toString = x =>
    switch x {
    | #tableRowGroup => "table-row-group"
    | #tableHeaderGroup => "table-header-group"
    | #tableFooterGroup => "table-footer-group"
    | #tableRow => "table-row"
    | #tableCell => "table-cell"
    | #tableColumnGroup => "table-column-group"
    | #tableColumn => "table-column"
    | #tableCaption => "table-caption"
    }
}

module DisplayBox = {
  type t = [#contents | #none]

  let toString = x =>
    switch x {
    | #contents => "contents"
    | #none => "none"
    }
}

module DisplayLegacy = {
  type t = [#inlineBlock | #inlineFlex | #inlineGrid | #inlineTable]

  let toString = x =>
    switch x {
    | #inlineBlock => "inline-block"
    | #inlineFlex => "inline-flex"
    | #inlineGrid => "inline-grid"
    | #inlineTable => "inline-table"
    }
}

module JustifySelf = {
  type t = [#auto | #normal | #stretch]

  let toString = x =>
    switch x {
    | #auto => "auto"
    | #normal => "normal"
    | #stretch => "stretch"
    }
}

module PositionalAlignment = {
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

  let toString = x =>
    switch x {
    | #center => "center"
    | #start => "start"
    | #end_ => "end"
    | #flexStart => "flex-start"
    | #flexEnd => "flex-end"
    | #selfStart => "self-start"
    | #selfEnd => "self-end"
    | #left => "left"
    | #right => "right"
    }
}

module OverflowAlignment = {
  type t = [
    | #safe(PositionalAlignment.t)
    | #unsafe(PositionalAlignment.t)
  ]

  let toString = x =>
    switch x {
    | #safe(pa) => "safe " ++ PositionalAlignment.toString(pa)
    | #unsafe(pa) => "unsafe " ++ PositionalAlignment.toString(pa)
    }
}

module BaselineAlignment = {
  type t = [#baseline | #firstBaseline | #lastBaseline]

  let toString = x =>
    switch x {
    | #baseline => "baseline"
    | #firstBaseline => "first baseline"
    | #lastBaseline => "last baseline"
    }
}

module NormalAlignment = {
  type t = [#normal]

  let toString = x =>
    switch x {
    | #normal => "normal"
    }
}

module DistributedAlignment = {
  type t = [#spaceBetween | #spaceAround | #spaceEvenly | #stretch]

  let toString = x =>
    switch x {
    | #spaceBetween => "space-between"
    | #spaceAround => "space-around"
    | #spaceEvenly => "space-evenly"
    | #stretch => "stretch"
    }
}

module LegacyAlignment = {
  type t = [#legacy | #legacyRight | #legacyLeft | #legacyCenter]

  let toString = x =>
    switch x {
    | #legacy => "legacy"
    | #legacyRight => "legacy right"
    | #legacyLeft => "legacy left"
    | #legacyCenter => "legacy center"
    }
}

module TextAlign = {
  type t = [#left | #right | #center | #justify]

  let toString = x =>
    switch x {
    | #left => "left"
    | #right => "right"
    | #center => "center"
    | #justify => "justify"
    }
}

module WordBreak = {
  type t = [#normal | #breakAll | #keepAll]

  let toString = x =>
    switch x {
    | #normal => "normal"
    | #breakAll => "break-all"
    | #keepAll => "keep-all"
    }
}

module WhiteSpace = {
  type t = [#normal | #nowrap | #pre | #preLine | #preWrap | #breakSpaces]

  let toString = x =>
    switch x {
    | #normal => "normal"
    | #nowrap => "nowrap"
    | #pre => "pre"
    | #preLine => "pre-line"
    | #preWrap => "pre-wrap"
    | #breakSpaces => "break-spaces"
    }
}

module AlignItems = {
  type t = [#normal | #stretch]

  let toString = x =>
    switch x {
    | #normal => "normal"
    | #stretch => "stretch"
    }
}

module AlignSelf = {
  type t = [#auto | #normal | #stretch]

  let toString = x =>
    switch x {
    | #auto => "auto"
    | #normal => "normal"
    | #stretch => "stretch"
    }
}

module AlignContent = {
  type t = [#center | #start | #end_ | #flexStart | #flexEnd]

  let toString = x =>
    switch x {
    | #center => "center"
    | #start => "start"
    | #end_ => "end"
    | #flexStart => "flex-start"
    | #flexEnd => "flex-end"
    }
}

module ObjectFit = {
  type t = [#fill | #contain | #cover | #none | #scaleDown]

  let toString = x =>
    switch x {
    | #fill => "fill"
    | #contain => "contain"
    | #cover => "cover"
    | #none => "none"
    | #scaleDown => "scale-down"
    }
}

module Clear = {
  type t = [#none | #left | #right | #both | #inlineStart | #inlineEnd]

  let toString = x =>
    switch x {
    | #none => "none"
    | #left => "left"
    | #right => "right"
    | #both => "both"
    | #inlineStart => "inline-start"
    | #inlineEnd => "inline-end"
    }
}

module Float = {
  type t = [#left | #right | #none | #inlineStart | #inlineEnd]

  let toString = x =>
    switch x {
    | #left => "left"
    | #right => "right"
    | #none => "none"
    | #inlineStart => "inline-start"
    | #inlineEnd => "inline-end"
    }
}

module Visibility = {
  type t = [#visible | #hidden | #collapse]

  let toString = x =>
    switch x {
    | #visible => "visible"
    | #hidden => "hidden"
    | #collapse => "collapse"
    }
}

module TableLayout = {
  type t = [#auto | #fixed]

  let toString = x =>
    switch x {
    | #auto => "auto"
    | #fixed => "fixed"
    }
}

module BorderCollapse = {
  type t = [#collapse | #separate]

  let toString = x =>
    switch x {
    | #collapse => "collapse"
    | #separate => "separate"
    }
}

module FlexWrap = {
  type t = [#nowrap | #wrap | #wrapReverse]

  let toString = x =>
    switch x {
    | #nowrap => "nowrap"
    | #wrap => "wrap"
    | #wrapReverse => "wrap-reverse"
    }
}

module FlexDirection = {
  type t = [#row | #rowReverse | #column | #columnReverse]

  let toString = x =>
    switch x {
    | #row => "row"
    | #rowReverse => "row-reverse"
    | #column => "column"
    | #columnReverse => "column-reverse"
    }
}

module BoxSizing = {
  type t = [#contentBox | #borderBox]

  let toString = x =>
    switch x {
    | #contentBox => "content-box"
    | #borderBox => "border-box"
    }
}

module ColumnCount = {
  type t = [#auto | #count(int)]

  let toString = x =>
    switch x {
    | #auto => "auto"
    | #count(v) => Js.Int.toString(v)
    }
}

module UserSelect = {
  type t = [#none | #auto | #text | #contain | #all]

  let toString = x =>
    switch x {
    | #none => "none"
    | #auto => "auto"
    | #text => "text"
    | #contain => "contain"
    | #all => "all"
    }
}

module TextTransform = {
  type t = [#none | #capitalize | #uppercase | #lowercase]

  let toString = x =>
    switch x {
    | #none => "none"
    | #capitalize => "capitalize"
    | #uppercase => "uppercase"
    | #lowercase => "lowercase"
    }
}

module GridTemplateAreas = {
  type t = [#none | #areas(list<string>)]

  let areas = x => #areas(x)

  let toString = x =>
    switch x {
    | #none => "none"
    | #areas(l) =>
      String.trim(List.fold_left((carry, elem) => carry ++ ("'" ++ (elem ++ "' ")), "", l))
    }
}

module GridArea = {
  type t = [
    | #auto
    | #ident(string)
    | #num(int)
    | #numIdent(int, string)
    | #span([#num(int) | #ident(string)])
  ]

  let auto = #auto
  let ident = x => #ident(x)
  let num = x => #num(x)
  let numIdent = (x, y) => #numIdent(x, y)
  let span = x => #span(x)

  let toString = t =>
    switch t {
    | #auto => "auto"
    | #ident(s) => s
    | #num(i) => string_of_int(i)
    | #numIdent(i, s) => string_of_int(i) ++ (" " ++ s)
    | #span(e) =>
      "span " ++
      switch e {
      | #num(i) => string_of_int(i)
      | #ident(s) => s
      }
    }
}

module BackdropFilter = {
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

  let string_of_percent = p => Js.Float.toString(p) ++ "%"

  let toString = x =>
    switch x {
    | #blur(#...Length.t as b) => "blur(" ++ (Length.toString(b) ++ ")")
    | #brightness(#num(b)) => "brightness(" ++ (string_of_int(b) ++ ")")
    | #brightness(#percent(b)) => "brightness(" ++ (string_of_percent(b) ++ ")")
    | #contrast(#num(c)) => "contrast(" ++ (string_of_int(c) ++ ")")
    | #contrast(#percent(c)) => "contrast(" ++ (string_of_percent(c) ++ ")")
    | #dropShadow(#num(i)) => "drop-shadow(" ++ (string_of_int(i) ++ ")")
    | #dropShadow(#percent(i)) => "drop-shadow(" ++ (string_of_percent(i) ++ ")")
    | #grayscale(#num(i)) => "grayscale(" ++ (string_of_int(i) ++ ")")
    | #grayscale(#percent(i)) => "grayscale(" ++ (string_of_percent(i) ++ ")")
    | #hueRotate(#...Angle.t as h) => "hue-rotate(" ++ (Angle.toString(h) ++ ")")
    | #hueRotate(#zero) => "hue-rotate(0deg)"
    | #invert(#num(i)) => "invert(" ++ (string_of_int(i) ++ ")")
    | #invert(#percent(i)) => "invert(" ++ (string_of_percent(i) ++ ")")
    | #none => "none"
    | #opacity(#num(i)) => "opacity(" ++ (string_of_int(i) ++ ")")
    | #opacity(#percent(i)) => "opacity(" ++ (string_of_percent(i) ++ ")")
    | #saturate(#num(i)) => "saturate(" ++ (string_of_int(i) ++ ")")
    | #saturate(#percent(i)) => "saturate(" ++ (string_of_percent(i) ++ ")")
    | #sepia(#num(i)) => "sepia(" ++ (string_of_int(i) ++ ")")
    | #sepia(#percent(i)) => "sepia(" ++ (string_of_percent(i) ++ ")")
    }
}

module BackgroundAttachment = {
  type t = [#scroll | #fixed | #local]

  let toString = x =>
    switch x {
    | #scroll => "scroll"
    | #fixed => "fixed"
    | #local => "local"
    }
}

module BackgroundClip = {
  type t = [#borderBox | #paddingBox | #contentBox]

  let toString = x =>
    switch x {
    | #borderBox => "border-box"
    | #contentBox => "content-box"
    | #paddingBox => "padding-box"
    }
}

module BackgroundOrigin = {
  type t = [#borderBox | #paddingBox | #contentBox]

  let toString = x =>
    switch x {
    | #borderBox => "border-box"
    | #contentBox => "content-box"
    | #paddingBox => "padding-box"
    }
}

module BackgroundPosition = {
  module X = {
    type t = [#left | #right | #center]

    let toString = x =>
      switch x {
      | #left => "left"
      | #right => "right"
      | #center => "center"
      }
  }

  module Y = {
    type t = [#top | #bottom | #center]

    let toString = x =>
      switch x {
      | #top => "top"
      | #bottom => "bottom"
      | #center => "center"
      }
  }

  type t = [X.t | Y.t]

  let toString = x =>
    switch x {
    | #left => "left"
    | #right => "right"
    | #top => "top"
    | #bottom => "bottom"
    | #center => "center"
    }
}

module BackgroundRepeat = {
  type twoValue = [#repeat | #space | #round | #noRepeat]
  type t = [#repeatX | #repeatY | twoValue]
  type horizontal = twoValue
  type vertical = twoValue

  let toString = x =>
    switch x {
    | #repeatX => "repeat-x"
    | #repeatY => "repeat-y"
    | #repeat => "repeat"
    | #space => "space"
    | #round => "round"
    | #noRepeat => "no-repeat"
    }
}

module TextOverflow = {
  type t = [#clip | #ellipsis | #string(string)]

  let toString = x =>
    switch x {
    | #clip => "clip"
    | #ellipsis => "ellipsis"
    | #string(s) => s
    }
}

module TextDecorationLine = {
  type t = [#none | #underline | #overline | #lineThrough | #blink]

  let toString = x =>
    switch x {
    | #none => "none"
    | #underline => "underline"
    | #overline => "overline"
    | #lineThrough => "line-through"
    | #blink => "blink"
    }
}

module TextDecorationStyle = {
  type t = [#solid | #double | #dotted | #dashed | #wavy]

  let toString = x =>
    switch x {
    | #solid => "solid"
    | #double => "double"
    | #dotted => "dotted"
    | #dashed => "dashed"
    | #wavy => "wavy"
    }
}

module Width = {
  type t = [#auto | #fitContent]

  let toString = x =>
    switch x {
    | #auto => "auto"
    | #fitContent => "fit-content"
    }
}

module MaxWidth = {
  type t = [#none]

  let toString = x =>
    switch x {
    | #none => "none"
    }
}

module Height = {
  type t = [#auto]

  let toString = x =>
    switch x {
    | #auto => "auto"
    }
}

module MaxHeight = {
  type t = [#none]

  let toString = x =>
    switch x {
    | #none => "none"
    }
}

module OverflowWrap = {
  type t = [#normal | #breakWord | #anywhere]

  let toString = x =>
    switch x {
    | #normal => "normal"
    | #breakWord => "break-word"
    | #anywhere => "anywhere"
    }
}

module Gradient = {
  type t<'colorOrVar> = [
    | #linearGradient(Angle.t, list<(Length.t, [< Color.t | Var.t] as 'colorOrVar)>)
    | #repeatingLinearGradient(Angle.t, list<(Length.t, [< Color.t | Var.t] as 'colorOrVar)>)
    | #radialGradient(list<(Length.t, [< Color.t | Var.t] as 'colorOrVar)>)
    | #repeatingRadialGradient(list<(Length.t, [< Color.t | Var.t] as 'colorOrVar)>)
  ]

  let linearGradient = (angle, stops) => #linearGradient(angle, stops)
  let repeatingLinearGradient = (angle, stops) => #repeatingLinearGradient(angle, stops)
  let radialGradient = stops => #radialGradient(stops)
  let repeatingRadialGradient = stops => #repeatingRadialGradient(stops)

  let string_of_color = x =>
    switch x {
    | #...Color.t as co => Color.toString(co)
    | #...Var.t as va => Var.toString(va)
    }
  let string_of_stops = stops =>
    stops->Belt.List.map(((l, c)) => string_of_color(c) ++ (" " ++ Length.toString(l)))->join(", ")

  let toString = x =>
    switch x {
    | #linearGradient(angle, stops) =>
      "linear-gradient(" ++ (Angle.toString(angle) ++ (", " ++ (string_of_stops(stops) ++ ")")))
    | #repeatingLinearGradient(angle, stops) =>
      "repeating-linear-gradient(" ++
      (Angle.toString(angle) ++
      (", " ++ (string_of_stops(stops) ++ ")")))
    | #radialGradient(stops) => "radial-gradient(" ++ (string_of_stops(stops) ++ ")")
    | #repeatingRadialGradient(stops) =>
      "repeating-radial-gradient(" ++ (string_of_stops(stops) ++ ")")
    }
}

module BackgroundImage = {
  type t = [#none]

  let toString = x =>
    switch x {
    | #none => "none"
    }
}

module GeometyBox = {
  type t = [
    | #marginBox
    | #borderBox
    | #paddingBox
    | #contentBox
    | #fillBox
    | #strokeBox
    | #viewBox
  ]

  let marginBox = #marginBox
  let borderBox = #borderBox
  let paddingBox = #paddingBox
  let contentBox = #contentBox
  let fillBox = #fillBox
  let strokeBox = #strokeBox
  let viewBox = #viewBox

  let toString = x =>
    switch x {
    | #marginBox => "margin-box"
    | #borderBox => "border-box"
    | #paddingBox => "padding-box"
    | #contentBox => "content-box"
    | #fillBox => "fill-box"
    | #strokeBox => "stroke-box"
    | #viewBox => "view-box"
    }
}

module ClipPath = {
  type t = [#none]

  let toString = x =>
    switch x {
    | #none => "none"
    }
}

module BackfaceVisibility = {
  type t = [#visible | #hidden]

  let toString = x =>
    switch x {
    | #visible => "visible"
    | #hidden => "hidden"
    }
}

module Flex = {
  type t = [#auto | #initial | #none]

  let toString = x =>
    switch x {
    | #auto => "auto"
    | #initial => "initial"
    | #none => "none"
    }
}

module TransformStyle = {
  type t = [#preserve3d | #flat]

  let toString = x =>
    switch x {
    | #preserve3d => "preserve-3d"
    | #flat => "flat"
    }
}

module ListStyleImage = {
  type t = [#none]

  let toString = x =>
    switch x {
    | #none => "none"
    }
}

module FontFamilyName = {
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

  let custom = #custom
  let serif = #serif
  let sansSerif = #sansSerif
  let cursive = #cursive
  let fantasy = #fantasy
  let monospace = #monospace
  let systemUi = #systemUi
  let emoji = #emoji
  let math = #math
  let fangsong = #fangsong

  let toString = x =>
    switch x {
    | #custom(name) => name
    | #serif => "serif"
    | #sansSerif => "sans-serif"
    | #cursive => "cursive"
    | #fantasy => "fantasy"
    | #monospace => "monospace"
    | #systemUi => "system-ui"
    | #emoji => "emoji"
    | #math => "math"
    | #fangsong => "fangsong"
    }
}

module FontDisplay = {
  type t = [#auto | #block | #swap | #fallback | #optional]

  let auto = #auto
  let block = #block
  let swap = #swap
  let fallback = #fallback
  let optional = #optional

  let toString = x =>
    switch x {
    | #auto => "auto"
    | #block => "block"
    | #swap => "swap"
    | #fallback => "fallback"
    | #optional => "optional"
    }
}

module CounterStyleType = {
  type t = [ListStyleType.t]

  let toString = x =>
    switch x {
    | #...ListStyleType.t as c => ListStyleType.toString(c)
    }
}

module Counter = {
  type style = [CounterStyleType.t | #unset]
  type t = [#counter(string, style)]

  let counter = (~style=#unset, name) => #counter(name, style)

  let toString = x =>
    switch x {
    | #counter(counter, style) =>
      switch style {
      | #unset => "counter(" ++ (counter ++ ")")
      | #...CounterStyleType.t as t =>
        "counter(" ++ (counter ++ ("," ++ (CounterStyleType.toString(t) ++ ")")))
      }
    }
}

module Counters = {
  type style = [CounterStyleType.t | #unset]
  type t = [#counters(string, string, style)]

  let counters = (~style=#unset, ~separator="", name) => #counters(name, separator, style)

  let toString = x =>
    switch x {
    | #counters(name, separator, style) =>
      switch style {
      | #unset => "counters(" ++ (name ++ (",\"" ++ (separator ++ "\")")))
      | #...CounterStyleType.t as s =>
        "counters(" ++
        (name ++
        (",\"" ++ (separator ++ ("\"," ++ (CounterStyleType.toString(s) ++ ")")))))
      }
    }
}

module CounterIncrement = {
  type t = [#none | #increment(string, int)]

  let increment = (~value=1, name) => #increment(name, value)

  let toString = x =>
    switch x {
    | #none => "none"
    | #increment(name, value) => name ++ (" " ++ string_of_int(value))
    }
}

module CounterReset = {
  type t = [#none | #reset(string, int)]

  let reset = (~value=0, name) => #reset(name, value)

  let toString = x =>
    switch x {
    | #none => "none"
    | #reset(name, value) => name ++ (" " ++ string_of_int(value))
    }
}

module CounterSet = {
  type t = [#none | #set(string, int)]

  let set = (~value=0, name) => #set(name, value)

  let toString = x =>
    switch x {
    | #none => "none"
    | #set(name, value) => name ++ (" " ++ string_of_int(value))
    }
}

module Content = {
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

  let toString = x =>
    switch x {
    | #none => "none"
    | #normal => "normal"
    | #openQuote => "open-quote"
    | #closeQuote => "close-quote"
    | #noOpenQuote => "no-open-quote"
    | #noCloseQuote => "no-close-quote"
    | #attr(name) => "attr(" ++ (name ++ ")")
    | #text(string) => `"$string"`
    }
}

module SVG = {
  module Fill = {
    type t = [#none | #contextFill | #contextStroke]

    let contextFill = #contextFill
    let contextStroke = #contextStroke

    let toString = x =>
      switch x {
      | #none => "none"
      | #contextFill => "context-fill"
      | #contextStroke => "context-stroke"
      }
  }
}
