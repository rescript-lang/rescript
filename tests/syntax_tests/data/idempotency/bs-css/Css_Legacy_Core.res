module Types = Css_AtomicTypes
open Css_AtomicTypes

type rec rule =
  | D(string, string) // Declaration
  | S(string, list<rule>) // Selector
  | PseudoClass(string, list<rule>)
  | PseudoClassParam(string, string, list<rule>)

let rec ruleToDict = (dict, rule) => {
  switch rule {
  | D(name, value) if name == "content" =>
    dict->Js.Dict.set(name, Js.Json.string(value == "" ? "\"\"" : value))
  | D(name, value) => dict->Js.Dict.set(name, Js.Json.string(value))
  | S(name, ruleset) => dict->Js.Dict.set(name, toJson(ruleset))
  | PseudoClass(name, ruleset) => dict->Js.Dict.set(":" ++ name, toJson(ruleset))
  | PseudoClassParam(name, param, ruleset) =>
    dict->Js.Dict.set(":" ++ (name ++ ("(" ++ (param ++ ")"))), toJson(ruleset))
  }
  dict
}

and toJson = rules => rules->Belt.List.reduce(Js.Dict.empty(), ruleToDict)->Js.Json.object_

let addStop = (dict, (stop, rules)) => {
  Js.Dict.set(dict, Js.Int.toString(stop) ++ "%", toJson(rules))
  dict
}

module Make = (CssImplementation: Css_Core.CssImplementationIntf) => {
  let merge = stylenames => CssImplementation.mergeStyles(. stylenames->Array.of_list)

  let insertRule = s => CssImplementation.injectRaw(. s)

  let style = rules => CssImplementation.make(. rules->toJson)

  let global = (selector, rules: list<rule>) =>
    CssImplementation.injectRule(.
      list{(selector, toJson(rules))}->Js.Dict.fromList->Js.Json.object_,
    )

  let keyframes = frames =>
    CssImplementation.makeKeyFrames(. List.fold_left(addStop, Js.Dict.empty(), frames))
}

let join = (strings, separator) => {
  let rec run = (strings, acc) =>
    switch strings {
    | list{} => acc
    | list{x} => acc ++ x
    | list{x, ...xs} => run(xs, acc ++ (x ++ separator))
    }
  run(strings, "")
}

module Converter = {
  let string_of_stops = stops =>
    stops->Belt.List.map(((l, c)) => Color.toString(c) ++ (" " ++ Length.toString(l)))->join(", ")

  let string_of_time = t => Js.Int.toString(t) ++ "ms"

  let string_of_content = x =>
    switch x {
    | #...Content.t as c => Content.toString(c)
    | #...Counter.t as c => Counter.toString(c)
    | #...Counters.t as c => Counters.toString(c)
    | #...Gradient.t as g => Gradient.toString(g)
    | #...Url.t as u => Url.toString(u)
    | #...Var.t as va => Var.toString(va)
    | #...Cascading.t as c => Cascading.toString(c)
    }
  let string_of_counter_increment = x =>
    switch x {
    | #...CounterIncrement.t as o => CounterIncrement.toString(o)
    | #...Var.t as va => Var.toString(va)
    | #...Cascading.t as c => Cascading.toString(c)
    }
  let string_of_counter_reset = x =>
    switch x {
    | #...CounterReset.t as o => CounterReset.toString(o)
    | #...Var.t as va => Var.toString(va)
    | #...Cascading.t as c => Cascading.toString(c)
    }
  let string_of_counter_set = x =>
    switch x {
    | #...CounterSet.t as o => CounterSet.toString(o)
    | #...Var.t as va => Var.toString(va)
    | #...Cascading.t as c => Cascading.toString(c)
    }

  let string_of_column_gap = x =>
    switch x {
    | #...ColumnGap.t as gcg => ColumnGap.toString(gcg)
    | #...Percentage.t as p => Percentage.toString(p)
    | #...Length.t as l => Length.toString(l)
    | #...Var.t as va => Var.toString(va)
    | #...Cascading.t as c => Cascading.toString(c)
    }

  let string_of_position = x =>
    switch x {
    | #auto => "auto"
    | #...Length.t as l => Length.toString(l)
    | #...Var.t as va => Var.toString(va)
    | #...Cascading.t as c => Cascading.toString(c)
    }

  let string_of_color = x =>
    switch x {
    | #...Color.t as co => Color.toString(co)
    | #...Var.t as va => Var.toString(va)
    }

  let string_of_dasharray = x =>
    switch x {
    | #...Percentage.t as p => Percentage.toString(p)
    | #...Length.t as l => Length.toString(l)
    }
}

include Converter

type animationName = string

let important = v =>
  switch v {
  | D(name, value) => D(name, value ++ " !important")
  | S(_, _)
  | PseudoClass(_, _)
  | PseudoClassParam(_, _, _) => v
  }

let label = label => D("label", label)

/* Properties */

let alignContent = x => D(
  "alignContent",
  switch x {
  | #...AlignContent.t as ac => AlignContent.toString(ac)
  | #...NormalAlignment.t as na => NormalAlignment.toString(na)
  | #...BaselineAlignment.t as ba => BaselineAlignment.toString(ba)
  | #...DistributedAlignment.t as da => DistributedAlignment.toString(da)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let alignItems = x => D(
  "alignItems",
  switch x {
  | #...AlignItems.t as ai => AlignItems.toString(ai)
  | #...PositionalAlignment.t as pa => PositionalAlignment.toString(pa)
  | #...BaselineAlignment.t as ba => BaselineAlignment.toString(ba)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let alignSelf = x => D(
  "alignSelf",
  switch x {
  | #...AlignSelf.t as a => AlignSelf.toString(a)
  | #...PositionalAlignment.t as pa => PositionalAlignment.toString(pa)
  | #...BaselineAlignment.t as ba => BaselineAlignment.toString(ba)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let animationDelay = x => D("animationDelay", string_of_time(x))

let animationDirection = x => D("animationDirection", AnimationDirection.toString(x))

let animationDuration = x => D("animationDuration", string_of_time(x))

let animationFillMode = x => D("animationFillMode", AnimationFillMode.toString(x))

let animationIterationCount = x => D("animationIterationCount", AnimationIterationCount.toString(x))

let animationPlayState = x => D("animationPlayState", AnimationPlayState.toString(x))

let animationTimingFunction = x => D("animationTimingFunction", TimingFunction.toString(x))

let backfaceVisibility = x => D(
  "backfaceVisibility",
  switch x {
  | #...BackfaceVisibility.t as bv => BackfaceVisibility.toString(bv)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let backdropFilter = x => D(
  "backdrop-filter",
  x->Belt.List.map(Types.BackdropFilter.toString)->join(", "),
)

let backgroundAttachment = x => D(
  "backgroundAttachment",
  switch x {
  | #...BackgroundAttachment.t as ba => BackgroundAttachment.toString(ba)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let backgroundColor = x => D("backgroundColor", string_of_color(x))

let backgroundClip = x => D(
  "backgroundClip",
  switch x {
  | #...BackgroundClip.t as bc => BackgroundClip.toString(bc)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let backgroundImage = x => D(
  "backgroundImage",
  switch x {
  | #...BackgroundImage.t as bi => BackgroundImage.toString(bi)
  | #...Url.t as u => Url.toString(u)
  | #...Gradient.t as g => Gradient.toString(g)
  },
)

let backgroundOrigin = x => D(
  "backgroundOrigin",
  switch x {
  | #...BackgroundOrigin.t as bo => BackgroundOrigin.toString(bo)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let string_of_backgroundposition = x =>
  switch x {
  | #...BackgroundPosition.t as bp => BackgroundPosition.toString(bp)
  | #hv(h, v) =>
    switch h {
    | #...BackgroundPosition.X.t as h => BackgroundPosition.X.toString(h)
    | #...Length.t as l => Length.toString(l)
    } ++
    (" " ++
    switch v {
    | #...BackgroundPosition.Y.t as v => BackgroundPosition.Y.toString(v)
    | #...Length.t as l => Length.toString(l)
    })
  | #...Length.t as l => Length.toString(l)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  }

let backgroundPosition = x => D("backgroundPosition", string_of_backgroundposition(x))

let backgroundPositions = bp => D(
  "backgroundPosition",
  bp->Belt.List.map(string_of_backgroundposition)->join(", "),
)

let backgroundPosition4 = (~x, ~offsetX, ~y, ~offsetY) => D(
  "backgroundPosition",
  BackgroundPosition.X.toString(x) ++
  (" " ++
  (Length.toString(offsetX) ++
  (" " ++
  (BackgroundPosition.Y.toString(y) ++ (" " ++ Length.toString(offsetY)))))),
)

let backgroundRepeat = x => D(
  "backgroundRepeat",
  switch x {
  | #...BackgroundRepeat.t as br => BackgroundRepeat.toString(br)
  | #hv(#...BackgroundRepeat.horizontal as h, #...BackgroundRepeat.vertical as v) =>
    BackgroundRepeat.toString(h) ++ (" " ++ BackgroundRepeat.toString(v))
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let borderBottomColor = x => D("borderBottomColor", string_of_color(x))

let borderBottomLeftRadius = x => D("borderBottomLeftRadius", Length.toString(x))

let borderBottomRightRadius = x => D("borderBottomRightRadius", Length.toString(x))

let borderBottomWidth = x => D("borderBottomWidth", Length.toString(x))

let borderCollapse = x => D(
  "borderCollapse",
  switch x {
  | #...BorderCollapse.t as bc => BorderCollapse.toString(bc)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let borderColor = x => D("borderColor", string_of_color(x))

let borderLeftColor = x => D("borderLeftColor", string_of_color(x))

let borderLeftWidth = x => D("borderLeftWidth", Length.toString(x))

let borderSpacing = x => D("borderSpacing", Length.toString(x))

let borderRadius = x => D("borderRadius", Length.toString(x))

let borderRightColor = x => D("borderRightColor", string_of_color(x))

let borderRightWidth = x => D("borderRightWidth", Length.toString(x))

let borderTopColor = x => D("borderTopColor", string_of_color(x))

let borderTopLeftRadius = x => D("borderTopLeftRadius", Length.toString(x))

let borderTopRightRadius = x => D("borderTopRightRadius", Length.toString(x))

let borderTopWidth = x => D("borderTopWidth", Length.toString(x))

let borderWidth = x => D("borderWidth", Length.toString(x))

let bottom = x => D("bottom", string_of_position(x))

let boxSizing = x => D(
  "boxSizing",
  switch x {
  | #...BoxSizing.t as bs => BoxSizing.toString(bs)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let clear = x => D(
  "clear",
  switch x {
  | #...Clear.t as cl => Clear.toString(cl)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let clipPath = x => D(
  "clipPath",
  switch x {
  | #...ClipPath.t as cp => ClipPath.toString(cp)
  | #...Url.t as u => Url.toString(u)
  | #...GeometyBox.t as gb => GeometyBox.toString(gb)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let color = x => D("color", string_of_color(x))

let columnCount = x => D(
  "columnCount",
  switch x {
  | #...ColumnCount.t as cc => ColumnCount.toString(cc)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let columnGap = x => D("columnGap", string_of_column_gap(x))

let contentRule = x => D("content", string_of_content(x))
let contentRules = xs => D("content", xs->Belt.List.map(string_of_content)->join(" "))

let counterIncrement = x => D("counter-increment", string_of_counter_increment(x))
let countersIncrement = xs => D(
  "counter-increment",
  xs->Belt.List.map(string_of_counter_increment)->join(" "),
)

let counterReset = x => D("counter-reset", string_of_counter_reset(x))
let countersReset = xs => D("counter-reset", xs->Belt.List.map(string_of_counter_reset)->join(" "))

let counterSet = x => D("counter-set", string_of_counter_set(x))
let countersSet = xs => D("counter-set", xs->Belt.List.map(string_of_counter_set)->join(" "))

let cursor = x => D("cursor", Cursor.toString(x))

let direction = x => D(
  "direction",
  switch x {
  | #...Direction.t as d => Direction.toString(d)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let display = x => D(
  "display",
  switch x {
  | #...DisplayOutside.t as o => DisplayOutside.toString(o)
  | #...DisplayInside.t as i => DisplayInside.toString(i)
  | #...DisplayListItem.t as l => DisplayListItem.toString(l)
  | #...DisplayInternal.t as i' => DisplayInternal.toString(i')
  | #...DisplayBox.t as b => DisplayBox.toString(b)
  | #...DisplayLegacy.t as l' => DisplayLegacy.toString(l')
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let flex = x => D(
  "flex",
  switch x {
  | #...Flex.t as f => Flex.toString(f)
  | #num(n) => Js.Float.toString(n)
  },
)

let flexDirection = x => D(
  "flexDirection",
  switch x {
  | #...FlexDirection.t as fd => FlexDirection.toString(fd)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let flexGrow = x => D("flexGrow", Js.Float.toString(x))

let flexShrink = x => D("flexShrink", Js.Float.toString(x))

let flexWrap = x => D(
  "flexWrap",
  switch x {
  | #...FlexWrap.t as fw => FlexWrap.toString(fw)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let float = x => D(
  "float",
  switch x {
  | #...Float.t as f => Float.toString(f)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let fontFamily = x => D(
  "fontFamily",
  switch x {
  | #...FontFamilyName.t as n => FontFamilyName.toString(n)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let fontFamilies = xs => D("fontFamily", xs->Belt.List.map(FontFamilyName.toString)->join(", "))

let fontSize = x => D(
  "fontSize",
  switch x {
  | #...Length.t as l => Length.toString(l)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let fontStyle = x => D(
  "fontStyle",
  switch x {
  | #...FontStyle.t as f => FontStyle.toString(f)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let fontVariant = x => D(
  "fontVariant",
  switch x {
  | #...FontVariant.t as f => FontVariant.toString(f)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let fontWeight = x => D(
  "fontWeight",
  switch x {
  | #...FontWeight.t as f => FontWeight.toString(f)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let gridAutoFlow = x => D(
  "gridAutoFlow",
  switch x {
  | #...GridAutoFlow.t as f => GridAutoFlow.toString(f)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let gridColumn = (start, end') => D(
  "gridColumn",
  Js.Int.toString(start) ++ (" / " ++ Js.Int.toString(end')),
)

let gridColumnGap = x => D("gridColumnGap", string_of_column_gap(x))

let gridColumnStart = n => D("gridColumnStart", Js.Int.toString(n))

let gridColumnEnd = n => D("gridColumnEnd", Js.Int.toString(n))

let gridRow = (start, end') => D(
  "gridRow",
  Js.Int.toString(start) ++ (" / " ++ Js.Int.toString(end')),
)

let gridGap = x => D(
  "gridGap",
  switch x {
  | #...Percentage.t as p => Percentage.toString(p)
  | #...Length.t as l => Length.toString(l)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let gridRowGap = x => D(
  "gridRowGap",
  switch x {
  | #...Percentage.t as p => Percentage.toString(p)
  | #...Length.t as l => Length.toString(l)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let gridRowEnd = n => D("gridRowEnd", Js.Int.toString(n))

let gridRowStart = n => D("gridRowStart", Js.Int.toString(n))

let height = x => D(
  "height",
  switch x {
  | #...Height.t as h => Height.toString(h)
  | #...Percentage.t as p => Percentage.toString(p)
  | #...Length.t as l => Length.toString(l)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let justifyContent = x => D(
  "justifyContent",
  switch x {
  | #...PositionalAlignment.t as pa => PositionalAlignment.toString(pa)
  | #...NormalAlignment.t as na => NormalAlignment.toString(na)
  | #...DistributedAlignment.t as da => DistributedAlignment.toString(da)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let justifyItems = x => D(
  "justifyItems",
  switch x {
  | #...PositionalAlignment.t as pa => PositionalAlignment.toString(pa)
  | #...NormalAlignment.t as na => NormalAlignment.toString(na)
  | #...BaselineAlignment.t as ba => BaselineAlignment.toString(ba)
  | #...OverflowAlignment.t as oa => OverflowAlignment.toString(oa)
  | #...LegacyAlignment.t as la => LegacyAlignment.toString(la)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let left = x => D("left", string_of_position(x))

let letterSpacing = x => D(
  "letterSpacing",
  switch x {
  | #...LetterSpacing.t as s => LetterSpacing.toString(s)
  | #...Length.t as l => Length.toString(l)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let lineHeight = x => D(
  "lineHeight",
  switch x {
  | #...LineHeight.t as h => LineHeight.toString(h)
  | #...Length.t as l => Length.toString(l)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let listStyle = (style, position, image) => D(
  "listStyle",
  ListStyleType.toString(style) ++
  (" " ++
  (ListStylePosition.toString(position) ++
  (" " ++
  switch image {
  | #...ListStyleImage.t as lsi => ListStyleImage.toString(lsi)
  | #...Url.t as u => Url.toString(u)
  }))),
)

let listStyleImage = x => D(
  "listStyleImage",
  switch x {
  | #...ListStyleImage.t as lsi => ListStyleImage.toString(lsi)
  | #...Url.t as u => Url.toString(u)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let listStyleType = x => D(
  "listStyleType",
  switch x {
  | #...ListStyleType.t as lsp => ListStyleType.toString(lsp)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let listStylePosition = x => D(
  "listStylePosition",
  switch x {
  | #...ListStylePosition.t as lsp => ListStylePosition.toString(lsp)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let marginToString = x =>
  switch x {
  | #...Length.t as l => Length.toString(l)
  | #...Margin.t as m => Margin.toString(m)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  }

let margin = x => D("margin", marginToString(x))
let margin2 = (~v, ~h) => D("margin", marginToString(v) ++ (" " ++ marginToString(h)))
let margin3 = (~top, ~h, ~bottom) => D(
  "margin",
  marginToString(top) ++ (" " ++ (marginToString(h) ++ (" " ++ marginToString(bottom)))),
)
let margin4 = (~top, ~right, ~bottom, ~left) => D(
  "margin",
  marginToString(top) ++
  (" " ++
  (marginToString(right) ++ (" " ++ (marginToString(bottom) ++ (" " ++ marginToString(left)))))),
)
let marginLeft = x => D("marginLeft", marginToString(x))
let marginRight = x => D("marginRight", marginToString(x))
let marginTop = x => D("marginTop", marginToString(x))
let marginBottom = x => D("marginBottom", marginToString(x))

let maxHeight = x => D(
  "maxHeight",
  switch x {
  | #...MaxHeight.t as mh => MaxHeight.toString(mh)
  | #...Percentage.t as p => Percentage.toString(p)
  | #...Length.t as l => Length.toString(l)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let maxWidth = x => D(
  "maxWidth",
  switch x {
  | #...MaxWidth.t as mw => MaxWidth.toString(mw)
  | #...Percentage.t as p => Percentage.toString(p)
  | #...Length.t as l => Length.toString(l)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let minHeight = x => D(
  "minHeight",
  switch x {
  | #...Height.t as h => Height.toString(h)
  | #...Percentage.t as p => Percentage.toString(p)
  | #...Length.t as l => Length.toString(l)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let minWidth = x => D(
  "minWidth",
  switch x {
  | #...Width.t as w => Width.toString(w)
  | #...Percentage.t as p => Percentage.toString(p)
  | #...Length.t as l => Length.toString(l)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let objectFit = x => D(
  "objectFit",
  switch x {
  | #...ObjectFit.t as o => ObjectFit.toString(o)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let objectPosition = x => D("objectPosition", string_of_backgroundposition(x))

let opacity = x => D("opacity", Js.Float.toString(x))

let outline = (size, style, color) => D(
  "outline",
  Length.toString(size) ++
  (" " ++
  (OutlineStyle.toString(style) ++ (" " ++ string_of_color(color)))),
)
let outlineColor = x => D("outlineColor", string_of_color(x))
let outlineOffset = x => D("outlineOffset", Length.toString(x))
let outlineStyle = x => D("outlineStyle", OutlineStyle.toString(x))
let outlineWidth = x => D("outlineWidth", Length.toString(x))

let overflow = x => D("overflow", Overflow.toString(x))
let overflowX = x => D("overflowX", Overflow.toString(x))
let overflowY = x => D("overflowY", Overflow.toString(x))

let overflowWrap = x => D(
  "overflowWrap",
  switch x {
  | #...OverflowWrap.t as ow => OverflowWrap.toString(ow)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let padding = x => D("padding", Length.toString(x))
let padding2 = (~v, ~h) => D("padding", Length.toString(v) ++ (" " ++ Length.toString(h)))
let padding3 = (~top, ~h, ~bottom) => D(
  "padding",
  Length.toString(top) ++ (" " ++ (Length.toString(h) ++ (" " ++ Length.toString(bottom)))),
)
let padding4 = (~top, ~right, ~bottom, ~left) => D(
  "padding",
  Length.toString(top) ++
  (" " ++
  (Length.toString(right) ++ (" " ++ (Length.toString(bottom) ++ (" " ++ Length.toString(left)))))),
)

let paddingBottom = x => D("paddingBottom", Length.toString(x))

let paddingLeft = x => D("paddingLeft", Length.toString(x))

let paddingRight = x => D("paddingRight", Length.toString(x))

let paddingTop = x => D("paddingTop", Length.toString(x))

let perspective = x => D(
  "perspective",
  switch x {
  | #...Perspective.t as p => Perspective.toString(p)
  | #...Length.t as l => Length.toString(l)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let perspectiveOrigin = (x, y) => D(
  "perspectiveOrigin",
  switch x {
  | #...Perspective.t as p => Perspective.toString(p)
  | #...Length.t as l => Length.toString(l)
  } ++
  (" " ++
  switch y {
  | #...Perspective.t as p => Perspective.toString(p)
  | #...Length.t as l => Length.toString(l)
  }),
)

let pointerEvents = x => D(
  "pointerEvents",
  switch x {
  | #...PointerEvents.t as p => PointerEvents.toString(p)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let position = x => D(
  "position",
  switch x {
  | #...Position.t as p => Position.toString(p)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let justifySelf = x => D(
  "justifySelf",
  switch x {
  | #...JustifySelf.t as j => JustifySelf.toString(j)
  | #...PositionalAlignment.t as pa => PositionalAlignment.toString(pa)
  | #...BaselineAlignment.t as ba => BaselineAlignment.toString(ba)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let resize = x => D(
  "resize",
  switch x {
  | #...Resize.t as r => Resize.toString(r)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let right = x => D("right", string_of_position(x))

let tableLayout = x => D(
  "tableLayout",
  switch x {
  | #...TableLayout.t as tl => TableLayout.toString(tl)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let textAlign = x => D(
  "textAlign",
  switch x {
  | #...TextAlign.t as ta => TextAlign.toString(ta)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let textDecorationColor = x => D(
  "textDecorationColor",
  switch x {
  | #...Color.t as co => Color.toString(co)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let textDecorationLine = x => D(
  "textDecorationLine",
  switch x {
  | #...TextDecorationLine.t as tdl => TextDecorationLine.toString(tdl)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let textDecorationStyle = x => D(
  "textDecorationStyle",
  switch x {
  | #...TextDecorationStyle.t as tds => TextDecorationStyle.toString(tds)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let textIndent = x => D(
  "textIndent",
  switch x {
  | #...Percentage.t as p => Percentage.toString(p)
  | #...Length.t as l => Length.toString(l)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let textOverflow = x => D(
  "textOverflow",
  switch x {
  | #...TextOverflow.t as txo => TextOverflow.toString(txo)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let textTransform = x => D(
  "textTransform",
  switch x {
  | #...TextTransform.t as tt => TextTransform.toString(tt)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let top = x => D("top", string_of_position(x))

let transform = x => D(
  "transform",
  switch x {
  | #none => "none"
  | #...Transform.t as t => Transform.toString(t)
  },
)

let transforms = x => D("transform", x->Belt.List.map(Transform.toString)->join(" "))

let transformOrigin = (x, y) => D(
  "transformOrigin",
  Length.toString(x) ++ (" " ++ Length.toString(y)),
)

let transformOrigin3d = (x, y, z) => D(
  "transformOrigin",
  Length.toString(x) ++ (" " ++ (Length.toString(y) ++ (" " ++ (Length.toString(z) ++ " ")))),
)

let unsafe = (property, value) => D(property, value)

let userSelect = x => D(
  "userSelect",
  switch x {
  | #...UserSelect.t as us => UserSelect.toString(us)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let verticalAlign = x => D(
  "verticalAlign",
  switch x {
  | #...VerticalAlign.t as v => VerticalAlign.toString(v)
  | #...Length.t as l => Length.toString(l)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let visibility = x => D(
  "visibility",
  switch x {
  | #...Visibility.t as v => Visibility.toString(v)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let width = x => D(
  "width",
  switch x {
  | #...Width.t as w => Width.toString(w)
  | #...Percentage.t as p => Percentage.toString(p)
  | #...Length.t as l => Length.toString(l)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let whiteSpace = x => D(
  "whiteSpace",
  switch x {
  | #...WhiteSpace.t as w => WhiteSpace.toString(w)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let wordBreak = x => D(
  "wordBreak",
  switch x {
  | #...WordBreak.t as w => WordBreak.toString(w)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let wordSpacing = x => D(
  "wordSpacing",
  switch x {
  | #...WordSpacing.t as w => WordSpacing.toString(w)
  | #...Percentage.t as p => Percentage.toString(p)
  | #...Length.t as l => Length.toString(l)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let wordWrap = overflowWrap

let zIndex = x => D("zIndex", Js.Int.toString(x))

/* Selectors */

let media = (query, rules) => S("@media " ++ query, rules)
let selector = (selector, rules) => S(selector, rules)
let pseudoClass = (selector, rules) => PseudoClass(selector, rules)

let active = pseudoClass("active")
let checked = pseudoClass("checked")
let default = pseudoClass("default")
let defined = pseudoClass("defined")
let disabled = pseudoClass("disabled")
let empty = pseudoClass("empty")
let enabled = pseudoClass("enabled")
let first = pseudoClass("first")
let firstChild = pseudoClass("first-child")
let firstOfType = pseudoClass("first-of-type")
let focus = pseudoClass("focus")
let focusWithin = pseudoClass("focus-within")
let host = (~selector=?, rules) =>
  switch selector {
  | None => PseudoClass("host", rules)
  | Some(s) => PseudoClassParam("host", s, rules)
  }
let hover = pseudoClass("hover")
let indeterminate = pseudoClass("indeterminate")
let inRange = pseudoClass("in-range")
let invalid = pseudoClass("invalid")
let lang = (code, rules) => PseudoClassParam("lang", code, rules)
let lastChild = pseudoClass("last-child")
let lastOfType = pseudoClass("last-of-type")
//let left = selector(":left");
let link = pseudoClass("link")
let not__ = (selector, rules) => PseudoClassParam("not", selector, rules)
module Nth = {
  type t = [#odd | #even | #n(int) | #add(int, int)]
  let toString = x =>
    switch x {
    | #odd => "odd"
    | #even => "even"
    | #n(x) => Js.Int.toString(x) ++ "n"
    | #add(x, y) => Js.Int.toString(x) ++ ("n+" ++ Js.Int.toString(y))
    }
}
let nthChild = (x, rules) => PseudoClassParam("nth-child", Nth.toString(x), rules)
let nthLastChild = (x, rules) => PseudoClassParam("nth-last-child", Nth.toString(x), rules)
let nthLastOfType = (x, rules) => PseudoClassParam("nth-last-of-type", Nth.toString(x), rules)
let nthOfType = (x, rules) => PseudoClassParam("nth-of-type", Nth.toString(x), rules)
let onlyChild = pseudoClass("only-child")
let onlyOfType = pseudoClass("only-of-type")
let optional = pseudoClass("optional")
let outOfRange = pseudoClass("out-of-range")
let readOnly = pseudoClass("read-only")
let readWrite = pseudoClass("read-write")
let required = pseudoClass("required")
//let right = selector(":right");
let root = pseudoClass("root")
let scope = pseudoClass("scope")
let target = pseudoClass("target")
let valid = pseudoClass("valid")
let visited = pseudoClass("visited")

let after = selector("::after")
let before = selector("::before")
let firstLetter = selector("::first-letter")
let firstLine = selector("::first-line")
let selection = selector("::selection")

let child = x => selector(" > " ++ x)
let children = selector(" > *")
let directSibling = selector(" + ")
let placeholder = selector("::placeholder")
let siblings = selector(" ~ ")

let anyLink = selector(":any-link")

/* Type aliasing */

type angle = Angle.t
type animationDirection = AnimationDirection.t
type animationFillMode = AnimationFillMode.t
type animationIterationCount = AnimationIterationCount.t
type animationPlayState = AnimationPlayState.t
type cascading = Cascading.t
type color = Color.t
type fontStyle = FontStyle.t
type fontWeight = FontWeight.t
type length = Length.t
type listStyleType = ListStyleType.t
type repeatValue = RepeatValue.t
type outlineStyle = OutlineStyle.t
type transform = Transform.t
type gradient<'colorOrVar> = Gradient.t<'colorOrVar>

/* Constructor aliases */

let initial = Cascading.initial
let inherit_ = Cascading.inherit_
let unset = Cascading.unset

let var = Types.Var.var
let varDefault = Types.Var.varDefault

// shared
let auto = #auto
let none = #none
let text = #text

let pct = Percentage.pct

let ch = Length.ch
let cm = Length.cm
let em = Length.em
let ex = Length.ex
let mm = Length.mm
let pt = Length.pt
let px = Length.px
let pxFloat = Length.pxFloat
let rem = Length.rem
let vh = Length.vh
let vmin = Length.vmin
let vmax = Length.vmax
let zero = Length.zero

let deg = Angle.deg
let rad = Angle.rad
let grad = Angle.grad
let turn = Angle.turn

let ltr = Direction.ltr
let rtl = Direction.rtl

let absolute = Position.absolute
let relative = Position.relative
let static = Position.static
let fixed = #fixed
let sticky = Position.sticky

//let none = Resize.none;
//let both = Resize.both;
let horizontal = Resize.horizontal
let vertical = Resize.vertical
//let block = Resize.block;
//let inline = Resize.inline;

let smallCaps = FontVariant.smallCaps

//let normal = `normal;
let italic = FontStyle.italic
let oblique = FontStyle.oblique

let hidden = #hidden
let visible = #visible
let scroll = #scroll

let rgb = Color.rgb
let rgba = Color.rgba
let hsl = Color.hsl
let hsla = Color.hsla
let hex = Color.hex
let currentColor = Color.currentColor
let transparent = Color.transparent

let linear = TimingFunction.linear
let ease = TimingFunction.ease
let easeIn = TimingFunction.easeIn
let easeInOut = TimingFunction.easeInOut
let easeOut = TimingFunction.easeOut
let stepStart = TimingFunction.stepStart
let stepEnd = TimingFunction.stepEnd
let steps = TimingFunction.steps
let cubicBezier = TimingFunction.cubicBezier

let marginBox = GeometyBox.marginBox
//let borderBox = GeometyBox.borderBox;
//let paddingBox = GeometyBox.paddingBox;
//let contentBox = GeometyBox.contentBox;
let fillBox = GeometyBox.fillBox
let strokeBox = GeometyBox.strokeBox
let viewBox = GeometyBox.viewBox

let translate = Transform.translate
let translate3d = Transform.translate3d
let translateX = Transform.translateX
let translateY = Transform.translateY
let translateZ = Transform.translateZ
let scaleX = Transform.scaleX
let scaleY = Transform.scaleY
let scaleZ = Transform.scaleZ
let rotateX = Transform.rotateX
let rotateY = Transform.rotateY
let rotateZ = Transform.rotateZ
let scale = Transform.scale
let scale3d = Transform.scale3d
let skew = Transform.skew
let skewX = Transform.skewX
let skewY = Transform.skewY

let thin = FontWeight.thin
let extraLight = FontWeight.extraLight
let light = FontWeight.light
let medium = FontWeight.medium
let semiBold = FontWeight.semiBold
let bold = FontWeight.bold
let extraBold = FontWeight.extraBold
let lighter = FontWeight.lighter
let bolder = FontWeight.bolder

let linearGradient = Gradient.linearGradient
let repeatingLinearGradient = Gradient.repeatingLinearGradient
let radialGradient = Gradient.radialGradient
let repeatingRadialGradient = Gradient.repeatingRadialGradient

let areas = GridTemplateAreas.areas
let ident = GridArea.ident
let numIdent = GridArea.numIdent

// cursor aliases
//let auto = Cursor.auto;
//let default = Cursor.default;
//let none = Cursor.none;
let contextMenu = Cursor.contextMenu
let help = Cursor.help
let pointer = Cursor.pointer
let progress = Cursor.progress
let wait = Cursor.wait
let cell = Cursor.cell
let crosshair = Cursor.crosshair
//let text = Cursor.text;
let verticalText = Cursor.verticalText
let alias = Cursor.alias
let copy = Cursor.copy
let move = Cursor.move
let noDrop = Cursor.noDrop
let notAllowed = Cursor.notAllowed
let grab = Cursor.grab
let grabbing = Cursor.grabbing
let allScroll = Cursor.allScroll
let colResize = Cursor.colResize
let rowResize = Cursor.rowResize
let nResize = Cursor.nResize
let eResize = Cursor.eResize
let sResize = Cursor.sResize
let wResize = Cursor.wResize
let neResize = Cursor.neResize
let nwResize = Cursor.nwResize
let seResize = Cursor.seResize
let swResize = Cursor.swResize
let ewResize = Cursor.ewResize
let nsResize = Cursor.nsResize
let neswResize = Cursor.neswResize
let nwseResize = Cursor.nwseResize
let zoomIn = Cursor.zoomIn
let zoomOut = Cursor.zoomOut

/* *******************************************************
 ********************************************************
 ******************************************************* */

let vw = x => #vw(x)
let fr = x => #fr(x)

module Calc = {
  let \"-" = (a, b) => #calc(#sub, a, b)
  let \"+" = (a, b) => #calc(#add, a, b)
}
let size = (x, y) => #size(x, y)

let all = #all
let backwards = #backwards
let baseline = #baseline
let block = #block
let borderBox = #borderBox
let both = #both
let center = #center
let column = #column
let columnReverse = #columnReverse
let contain = #contain
let contentBox = #contentBox
let count = x => #count(x)
let cover = #cover
let dashed = #dashed
let dotted = #dotted
let flexBox = #flex
let grid = #grid
let inlineGrid = #inlineGrid
let flexEnd = #flexEnd
let flexStart = #flexStart
let forwards = #forwards
let infinite = #infinite

let inline = #inline
let contents = #contents
let inlineBlock = #inlineBlock
let inlineFlex = #inlineFlex
let inlineTable = #inlineTable
let listItem = #listItem
let runIn = #runIn
let table = #table
let tableCaption = #tableCaption
let tableColumnGroup = #tableColumnGroup
let tableHeaderGroup = #tableHeaderGroup
let tableFooterGroup = #tableFooterGroup
let tableRowGroup = #tableRowGroup
let tableCell = #tableCell
let tableColumn = #tableColumn
let tableRow = #tableRow

let local = #local
let localUrl = x => #localUrl(x)
let noRepeat = #noRepeat
let space = #space
let nowrap = #nowrap
let paddingBox = #paddingBox
let paused = #paused
let repeat = #repeat
let minmax = #minmax
let repeatX = #repeatX
let repeatY = #repeatY
let rotate = a => #rotate(a)
let rotate3d = (x, y, z, a) => #rotate3d(x, y, z, a)
let row = #row
let rowReverse = #rowReverse
let running = #running
let solid = #solid
let spaceAround = #spaceAround
let spaceBetween = #spaceBetween
let spaceEvenly = #spaceEvenly
let stretch = #stretch
let url = x => #url(x)
let wrap = #wrap
let wrapReverse = #wrapReverse

let inside = #inside
let outside = #outside

let underline = #underline
let overline = #overline
let lineThrough = #lineThrough
let clip = #clip
let ellipsis = #ellipsis

let wavy = #wavy
let double = #double

let uppercase = #uppercase
let lowercase = #lowercase
let capitalize = #capitalize

let sub = #sub
let super = #super
let textTop = #textTop
let textBottom = #textBottom
let middle = #middle

let normal = #normal

let breakAll = #breakAll
let keepAll = #keepAll
let breakWord = #breakWord

let reverse = #reverse
let alternate = #alternate
let alternateReverse = #alternateReverse

let fill = #fill
let content = #content
let maxContent = #maxContent
let minContent = #minContent
let fitContent = #fitContent

let round = #round
let miter = #miter
let bevel = #bevel
let butt = #butt
let square = #square

let flex3 = (~grow, ~shrink, ~basis) => D(
  "flex",
  Js.Float.toString(grow) ++
  (" " ++
  (Js.Float.toString(shrink) ++
  (" " ++
  switch basis {
  | #...FlexBasis.t as b => FlexBasis.toString(b)
  | #...Length.t as l => Length.toString(l)
  }))),
)
let flexBasis = x => D(
  "flexBasis",
  switch x {
  | #...FlexBasis.t as b => FlexBasis.toString(b)
  | #...Length.t as l => Length.toString(l)
  },
)

let order = x => D("order", Js.Int.toString(x))

let string_of_minmax = x =>
  switch x {
  | #auto => "auto"
  | #calc(#add, a, b) => "calc(" ++ (Length.toString(a) ++ (" + " ++ (Length.toString(b) ++ ")")))
  | #calc(#sub, a, b) => "calc(" ++ (Length.toString(a) ++ (" - " ++ (Length.toString(b) ++ ")")))
  | #ch(x) => Js.Float.toString(x) ++ "ch"
  | #cm(x) => Js.Float.toString(x) ++ "cm"
  | #em(x) => Js.Float.toString(x) ++ "em"
  | #ex(x) => Js.Float.toString(x) ++ "ex"
  | #mm(x) => Js.Float.toString(x) ++ "mm"
  | #percent(x) => Js.Float.toString(x) ++ "%"
  | #pt(x) => Js.Int.toString(x) ++ "pt"
  | #px(x) => Js.Int.toString(x) ++ "px"
  | #pxFloat(x) => Js.Float.toString(x) ++ "px"
  | #rem(x) => Js.Float.toString(x) ++ "rem"
  | #vh(x) => Js.Float.toString(x) ++ "vh"
  | #vmax(x) => Js.Float.toString(x) ++ "vmax"
  | #vmin(x) => Js.Float.toString(x) ++ "vmin"
  | #vw(x) => Js.Float.toString(x) ++ "vw"
  | #fr(x) => Js.Float.toString(x) ++ "fr"
  | #inch(x) => Js.Float.toString(x) ++ "in"
  | #pc(x) => Js.Float.toString(x) ++ "pc"
  | #zero => "0"
  | #minContent => "min-content"
  | #maxContent => "max-content"
  }

let string_of_dimension = x =>
  switch x {
  | #auto => "auto"
  | #none => "none"
  | #calc(#add, a, b) => "calc(" ++ (Length.toString(a) ++ (" + " ++ (Length.toString(b) ++ ")")))
  | #calc(#sub, a, b) => "calc(" ++ (Length.toString(a) ++ (" - " ++ (Length.toString(b) ++ ")")))
  | #ch(x) => Js.Float.toString(x) ++ "ch"
  | #cm(x) => Js.Float.toString(x) ++ "cm"
  | #em(x) => Js.Float.toString(x) ++ "em"
  | #ex(x) => Js.Float.toString(x) ++ "ex"
  | #mm(x) => Js.Float.toString(x) ++ "mm"
  | #percent(x) => Js.Float.toString(x) ++ "%"
  | #pt(x) => Js.Int.toString(x) ++ "pt"
  | #px(x) => Js.Int.toString(x) ++ "px"
  | #pxFloat(x) => Js.Float.toString(x) ++ "px"
  | #rem(x) => Js.Float.toString(x) ++ "rem"
  | #vh(x) => Js.Float.toString(x) ++ "vh"
  | #vmax(x) => Js.Float.toString(x) ++ "vmax"
  | #vmin(x) => Js.Float.toString(x) ++ "vmin"
  | #vw(x) => Js.Float.toString(x) ++ "vw"
  | #fr(x) => Js.Float.toString(x) ++ "fr"
  | #inch(x) => Js.Float.toString(x) ++ "in"
  | #pc(x) => Js.Float.toString(x) ++ "pc"
  | #zero => "0"
  | #fitContent => "fit-content"
  | #minContent => "min-content"
  | #maxContent => "max-content"
  | #minmax(a, b) => "minmax(" ++ (string_of_minmax(a) ++ ("," ++ (string_of_minmax(b) ++ ")")))
  }

type minmax = [#fr(float) | #minContent | #maxContent | #auto | Length.t]

type trackLength = [
  | Length.t
  | #auto
  | #fr(float)
  | #minContent
  | #maxContent
  | #minmax(minmax, minmax)
]
type gridLength = [trackLength | #repeat(RepeatValue.t, trackLength)]

let gridLengthToJs = x =>
  switch x {
  | #auto => "auto"
  | #calc(#add, a, b) => "calc(" ++ (Length.toString(a) ++ (" + " ++ (Length.toString(b) ++ ")")))
  | #calc(#sub, a, b) => "calc(" ++ (Length.toString(a) ++ (" - " ++ (Length.toString(b) ++ ")")))
  | #ch(x) => Js.Float.toString(x) ++ "ch"
  | #cm(x) => Js.Float.toString(x) ++ "cm"
  | #em(x) => Js.Float.toString(x) ++ "em"
  | #ex(x) => Js.Float.toString(x) ++ "ex"
  | #mm(x) => Js.Float.toString(x) ++ "mm"
  | #percent(x) => Js.Float.toString(x) ++ "%"
  | #pt(x) => Js.Int.toString(x) ++ "pt"
  | #px(x) => Js.Int.toString(x) ++ "px"
  | #pxFloat(x) => Js.Float.toString(x) ++ "px"
  | #rem(x) => Js.Float.toString(x) ++ "rem"
  | #vh(x) => Js.Float.toString(x) ++ "vh"
  | #inch(x) => Js.Float.toString(x) ++ "in"
  | #pc(x) => Js.Float.toString(x) ++ "pc"
  | #vmax(x) => Js.Float.toString(x) ++ "vmax"
  | #vmin(x) => Js.Float.toString(x) ++ "vmin"
  | #vw(x) => Js.Float.toString(x) ++ "vw"
  | #fr(x) => Js.Float.toString(x) ++ "fr"
  | #zero => "0"
  | #minContent => "min-content"
  | #maxContent => "max-content"
  | #repeat(n, x) =>
    "repeat(" ++ (RepeatValue.toString(n) ++ (", " ++ (string_of_dimension(x) ++ ")")))
  | #minmax(a, b) => "minmax(" ++ (string_of_minmax(a) ++ ("," ++ (string_of_minmax(b) ++ ")")))
  }

let string_of_dimensions = dimensions =>
  dimensions->List.map(gridLengthToJs)->String.concat(" ")

let gridTemplateColumns = dimensions => D("gridTemplateColumns", string_of_dimensions(dimensions))

let gridTemplateRows = dimensions => D("gridTemplateRows", string_of_dimensions(dimensions))

let gridAutoColumns = dimensions => D("gridAutoColumns", string_of_dimension(dimensions))

let gridAutoRows = dimensions => D("gridAutoRows", string_of_dimension(dimensions))

let gridArea = s => D(
  "gridArea",
  switch s {
  | #...GridArea.t as t => GridArea.toString(t)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let gridArea2 = (s, s2) => D("gridArea", GridArea.toString(s) ++ (" / " ++ GridArea.toString(s2)))

let gridArea3 = (s, s2, s3) => D(
  "gridArea",
  GridArea.toString(s) ++ (" / " ++ (GridArea.toString(s2) ++ (" / " ++ GridArea.toString(s3)))),
)

let gridArea4 = (s, s2, s3, s4) => D(
  "gridArea",
  GridArea.toString(s) ++
  (" / " ++
  (GridArea.toString(s2) ++
  (" / " ++
  (GridArea.toString(s3) ++ (" / " ++ GridArea.toString(s4)))))),
)

let gridTemplateAreas = l => D(
  "gridTemplateAreas",
  switch l {
  | #...GridTemplateAreas.t as t => GridTemplateAreas.toString(t)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

type filter = [
  | #blur(Length.t)
  | #brightness(float)
  | #contrast(float)
  | #dropShadow(Length.t, Length.t, Length.t, Color.t)
  | #grayscale(float)
  | #hueRotate(angle)
  | #invert(float)
  | #opacity(float)
  | #saturate(float)
  | #sepia(float)
  | #url(string)
  | #none
  | Var.t
  | Cascading.t
]

let string_of_filter = x =>
  switch x {
  | #blur(v) => "blur(" ++ (Length.toString(v) ++ ")")
  | #brightness(v) => "brightness(" ++ (Js.Float.toString(v) ++ "%)")
  | #contrast(v) => "contrast(" ++ (Js.Float.toString(v) ++ "%)")
  | #dropShadow(a, b, c, d) =>
    "drop-shadow(" ++
    (Length.toString(a) ++
    (" " ++
    (Length.toString(b) ++
    (" " ++ (Length.toString(c) ++ (" " ++ (Color.toString(d) ++ ")")))))))
  | #grayscale(v) => "grayscale(" ++ (Js.Float.toString(v) ++ "%)")
  | #hueRotate(v) => "hue-rotate(" ++ (Angle.toString(v) ++ ")")
  | #invert(v) => "invert(" ++ (Js.Float.toString(v) ++ "%)")
  | #opacity(v) => "opacity(" ++ (Js.Float.toString(v) ++ "%)")
  | #saturate(v) => "saturate(" ++ (Js.Float.toString(v) ++ "%)")
  | #sepia(v) => "sepia(" ++ (Js.Float.toString(v) ++ "%)")
  | #none => "none"
  | #...Url.t as u => Url.toString(u)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  }

let filter = x => D("filter", x->Belt.List.map(string_of_filter)->join(" "))

module Shadow = {
  type value<'a> = string
  type box
  type text
  type t<'a> = [#shadow(value<'a>) | #none]

  let box = (~x=zero, ~y=zero, ~blur=zero, ~spread=zero, ~inset=false, color) =>
    #shadow(
      Length.toString(x) ++
      (" " ++
      (Length.toString(y) ++
      (" " ++
      (Length.toString(blur) ++
      (" " ++
      (Length.toString(spread) ++
      (" " ++
      (string_of_color(color) ++ (inset ? " inset" : ""))))))))),
    )

  let text = (~x=zero, ~y=zero, ~blur=zero, color) =>
    #shadow(
      Length.toString(x) ++
      (" " ++
      (Length.toString(y) ++ (" " ++ (Length.toString(blur) ++ (" " ++ string_of_color(color)))))),
    )

  let toString: t<'a> => string = x =>
    switch x {
    | #shadow(x) => x
    | #none => "none"
    }
}

let boxShadow = x => D(
  "boxShadow",
  switch x {
  | #...Shadow.t as s => Shadow.toString(s)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let boxShadows = x => D("boxShadow", x->Belt.List.map(Shadow.toString)->join(", "))

let string_of_borderstyle = x =>
  switch x {
  | #...BorderStyle.t as b => BorderStyle.toString(b)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  }

let border = (px, style, color) => D(
  "border",
  Length.toString(px) ++ (" " ++ (string_of_borderstyle(style) ++ (" " ++ string_of_color(color)))),
)
let borderStyle = x => D("borderStyle", string_of_borderstyle(x))

let borderLeft = (px, style, color) => D(
  "borderLeft",
  Length.toString(px) ++ (" " ++ (string_of_borderstyle(style) ++ (" " ++ string_of_color(color)))),
)
let borderLeftStyle = x => D("borderLeftStyle", string_of_borderstyle(x))

let borderRight = (px, style, color) => D(
  "borderRight",
  Length.toString(px) ++ (" " ++ (string_of_borderstyle(style) ++ (" " ++ string_of_color(color)))),
)

let borderRightStyle = x => D("borderRightStyle", string_of_borderstyle(x))
let borderTop = (px, style, color) => D(
  "borderTop",
  Length.toString(px) ++ (" " ++ (string_of_borderstyle(style) ++ (" " ++ string_of_color(color)))),
)

let borderTopStyle = x => D("borderTopStyle", string_of_borderstyle(x))

let borderBottom = (px, style, color) => D(
  "borderBottom",
  Length.toString(px) ++ (" " ++ (string_of_borderstyle(style) ++ (" " ++ string_of_color(color)))),
)

let borderBottomStyle = x => D("borderBottomStyle", string_of_borderstyle(x))

let background = x => D(
  "background",
  switch x {
  | #...Color.t as c => Color.toString(c)
  | #...Url.t as u => Url.toString(u)
  | #...Gradient.t as g => Gradient.toString(g)
  | #none => "none"
  },
)

let backgrounds = x => D(
  "background",
  x
  ->Belt.List.map(item =>
    switch item {
    | #...Color.t as c => Color.toString(c)
    | #...Url.t as u => Url.toString(u)
    | #...Gradient.t as g => Gradient.toString(g)
    | #none => "none"
    }
  )
  ->join(", "),
)

let backgroundSize = x => D(
  "backgroundSize",
  switch x {
  | #size(x, y) => Length.toString(x) ++ (" " ++ Length.toString(y))
  | #auto => "auto"
  | #cover => "cover"
  | #contain => "contain"
  },
)

let fontFace = (~fontFamily, ~src, ~fontStyle=?, ~fontWeight=?, ~fontDisplay=?, ()) => {
  let fontStyle = Js.Option.map((. value) => FontStyle.toString(value), fontStyle)
  let src =
    src
    ->List.map(x =>
      switch x {
      | #localUrl(value) => `local("$(value)")`
      | #url(value) => `url("$(value)")`
      }
    )
    ->String.concat(", ")

  let fontStyle = Belt.Option.mapWithDefault(fontStyle, "", s => "font-style: " ++ (s ++ ";"))
  let fontWeight = Belt.Option.mapWithDefault(fontWeight, "", w =>
    "font-weight: " ++
    (switch w {
    | #...FontWeight.t as f => FontWeight.toString(f)
    | #...Var.t as va => Var.toString(va)
    | #...Cascading.t as c => Cascading.toString(c)
    } ++
    ";")
  )
  let fontDisplay = Belt.Option.mapWithDefault(fontDisplay, "", f =>
    "font-display: " ++ (FontDisplay.toString(f) ++ ";")
  )

  `@font-face {
     font-family: $fontFamily;
     src: $src;
     $(fontStyle)
     $(fontWeight)
     $(fontDisplay)
   }`
}

let textDecoration = x => D(
  "textDecoration",
  switch x {
  | #none => "none"
  | #underline => "underline"
  | #overline => "overline"
  | #lineThrough => "line-through"
  | #initial => "initial"
  | #inherit_ => "inherit"
  | #unset => "unset"
  | #...Var.t as va => Var.toString(va)
  },
)

let textShadow = x => D(
  "textShadow",
  switch x {
  | #...Shadow.t as s => Shadow.toString(s)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

let textShadows = x => D("textShadow", x->Belt.List.map(Shadow.toString)->join(", "))

let transformStyle = x => D(
  "transformStyle",
  switch x {
  | #...TransformStyle.t as ts => TransformStyle.toString(ts)
  | #...Var.t as va => Var.toString(va)
  | #...Cascading.t as c => Cascading.toString(c)
  },
)

@ocaml.doc("
 * Transition
 ")
module Transition = {
  type t = [#value(string)]

  let shorthand = (~duration=0, ~delay=0, ~timingFunction=#ease, property) =>
    #value(
      string_of_time(duration) ++
      (" " ++
      (TimingFunction.toString(timingFunction) ++
      (" " ++
      (string_of_time(delay) ++ (" " ++ property))))),
    )

  let toString = x =>
    switch x {
    | #value(v) => v
    }
}

let transitionValue = x => D("transition", Transition.toString(x))

let transitionList = x => D("transition", x->Belt.List.map(Transition.toString)->join(", "))
let transitions = transitionList

let transition = (~duration=?, ~delay=?, ~timingFunction=?, property) =>
  transitionValue(Transition.shorthand(~duration?, ~delay?, ~timingFunction?, property))

let transitionDelay = i => D("transitionDelay", string_of_time(i))

let transitionDuration = i => D("transitionDuration", string_of_time(i))

let transitionTimingFunction = x => D("transitionTimingFunction", TimingFunction.toString(x))

let transitionProperty = x => D("transitionProperty", x)

@ocaml.doc("
 * Animation
 ")
module Animation = {
  type t = [#value(string)]

  let shorthand = (
    ~duration=0,
    ~delay=0,
    ~direction=#normal,
    ~timingFunction=#ease,
    ~fillMode=#none,
    ~playState=#running,
    ~iterationCount=#count(1),
    name,
  ) =>
    #value(
      name ++
      (" " ++
      (string_of_time(duration) ++
      (" " ++
      (TimingFunction.toString(timingFunction) ++
      (" " ++
      (string_of_time(delay) ++
      (" " ++
      (AnimationIterationCount.toString(iterationCount) ++
      (" " ++
      (AnimationDirection.toString(direction) ++
      (" " ++
      (AnimationFillMode.toString(fillMode) ++
      (" " ++
      AnimationPlayState.toString(playState)))))))))))))),
    )

  let toString = x =>
    switch x {
    | #value(v) => v
    }
}

let animationValue = x => D("animation", Animation.toString(x))

let animation = (
  ~duration=?,
  ~delay=?,
  ~direction=?,
  ~timingFunction=?,
  ~fillMode=?,
  ~playState=?,
  ~iterationCount=?,
  name,
) =>
  animationValue(
    Animation.shorthand(
      ~duration?,
      ~delay?,
      ~direction?,
      ~timingFunction?,
      ~fillMode?,
      ~playState?,
      ~iterationCount?,
      name,
    ),
  )

let animations = x => D("animation", x->Belt.List.map(Animation.toString)->join(", "))

let animationName = x => D("animationName", x)

@ocaml.doc("
 * SVG
 ")
module SVG = {
  let fill = x => D(
    "fill",
    switch x {
    | #...Types.SVG.Fill.t as f => Types.SVG.Fill.toString(f)
    | #...Types.Color.t as c => Types.Color.toString(c)
    | #...Types.Var.t as v => Types.Var.toString(v)
    | #...Types.Url.t as u => Types.Url.toString(u)
    },
  )
  let fillOpacity = opacity => D("fillOpacity", Js.Float.toString(opacity))
  let fillRule = x => D(
    "fillRule",
    switch x {
    | #evenodd => "evenodd"
    | #nonzero => "nonzero"
    },
  )
  let stroke = x => D("stroke", string_of_color(x))
  let strokeDasharray = x => D(
    "stroke-dasharray",
    switch x {
    | #none => "none"
    | #dasharray(a) => a->Belt.List.map(string_of_dasharray)->join(" ")
    },
  )
  let strokeWidth = x => D("strokeWidth", Length.toString(x))
  let strokeOpacity = opacity => D("strokeOpacity", Js.Float.toString(opacity))
  let strokeMiterlimit = x => D("strokeMiterlimit", Js.Float.toString(x))
  let strokeLinecap = x => D(
    "strokeLinecap",
    switch x {
    | #butt => "butt"
    | #round => "round"
    | #square => "square"
    },
  )

  let strokeLinejoin = x => D(
    "strokeLinejoin",
    switch x {
    | #miter => "miter"
    | #round => "round"
    | #bevel => "bevel"
    },
  )
  let stopColor = x => D("stopColor", string_of_color(x))
  let stopOpacity = x => D("stopOpacity", Js.Float.toString(x))
}
