module CssForTest = {
  include Css_Colors
  include Css_Legacy_Core
  include Css_Legacy_Core.Make({
    exception NotImplemented

    let mergeStyles = (. _) => raise(NotImplemented)
    let make = (. _) => raise(NotImplemented)
    let injectRule = (. _) => raise(NotImplemented)
    let injectRaw = (. _) => raise(NotImplemented)
    let makeKeyFrames = (. _) => raise(NotImplemented)
  })
}

open Jest
open Expect
open CssForTest

let toBeJson = x => Expect.toBe(x->Js.Json.stringifyAny)
let r = x => toJson(list{x}) /* simple rule for more readable tests */

describe("Var", () => {
  test("test usage (limited)", () =>
    expect((r(color(var("foo"))), r(marginTop(var("--bar"))))->Js.Json.stringifyAny)->toBeJson((
      {"color": "var(--foo)"},
      {"marginTop": "var(--bar)"},
    ))
  )

  test("test usage with default (limited)", () =>
    expect(
      (
        r(textDecoration(varDefault("foo", "default"))),
        r(alignItems(varDefault("--bar", "default"))),
      )->Js.Json.stringifyAny,
    )->toBeJson(({"textDecoration": "var(--foo,default)"}, {"alignItems": "var(--bar,default)"}))
  )
})

describe("Color style", () =>
  test("test values", () =>
    expect(
      (
        r(color(rgb(1, 2, 3))),
        r(color(rgba(4, 5, 6, #num(0.3)))),
        r(color(hsl(deg(7.), pct(8.), pct(9.)))),
        r(color(hsla(deg(10.), pct(11.), pct(12.), #num(0.5)))),
        r(color(hsla(rad(4.7), pct(11.), pct(12.), pct(50.)))),
        r(color(transparent)),
        r(color(hex("FFF"))),
        r(color(currentColor)),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"color": "rgb(1, 2, 3)"},
      {"color": "rgba(4, 5, 6, 0.3)"},
      {"color": "hsl(7deg, 8%, 9%)"},
      {"color": "hsla(10deg, 11%, 12%, 0.5)"},
      {"color": "hsla(4.7rad, 11%, 12%, 50%)"},
      {"color": "transparent"},
      {"color": "#FFF"},
      {"color": "currentColor"},
    ))
  )
)

describe("Label", () =>
  test("test value", () => expect(r(label("a"))->Js.Json.stringifyAny)->toBeJson({"label": "a"}))
)
//  test("test classname", () =>
//    expect(style([label("theName")]))->toContainString("theName")
//  );

describe("Filter", () =>
  test("test values", () =>
    expect(
      (
        r(filter(list{#opacity(10.), #invert(20.)})),
        r(filter(list{#blur(#px(20)), #brightness(20.)})),
        r(filter(list{#contrast(30.), #dropShadow(#px(5), #px(6), #px(7), #rgb(255, 0, 0))})),
        r(filter(list{#grayscale(10.), #hueRotate(#deg(180.))})),
        r(filter(list{#saturate(10.), #sepia(100.)})),
        r(filter(list{#none})),
        r(filter(list{#inherit_})),
        r(filter(list{#initial})),
        r(filter(list{#unset})),
        r(filter(list{#url("myurl")})),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"filter": "opacity(10%) invert(20%)"},
      {"filter": "blur(20px) brightness(20%)"},
      {"filter": "contrast(30%) drop-shadow(5px 6px 7px rgb(255, 0, 0))"},
      {"filter": "grayscale(10%) hue-rotate(180deg)"},
      {"filter": "saturate(10%) sepia(100%)"},
      {"filter": "none"},
      {"filter": "inherit"},
      {"filter": "initial"},
      {"filter": "unset"},
      {"filter": "url(myurl)"},
    ))
  )
)

describe("Angle", () =>
  test("test values", () =>
    expect(
      (
        r(transform(rotate(deg(1.)))),
        r(transform(rotate(rad(6.28)))),
        r(transform(rotate(grad(38.8)))),
        r(transform(rotate(turn(0.25)))),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"transform": "rotate(1deg)"},
      {"transform": "rotate(6.28rad)"},
      {"transform": "rotate(38.8grad)"},
      {"transform": "rotate(0.25turn)"},
    ))
  )
)

describe("Direction", () =>
  test("test values", () =>
    expect(
      (
        r(direction(#ltr)),
        r(direction(ltr)),
        r(direction(rtl)),
        r(direction(inherit_)),
        r(direction(unset)),
        r(direction(initial)),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"direction": "ltr"},
      {"direction": "ltr"},
      {"direction": "rtl"},
      {"direction": "inherit"},
      {"direction": "unset"},
      {"direction": "initial"},
    ))
  )
)

describe("Resize", () =>
  test("test values", () =>
    expect(
      (
        r(resize(none)),
        r(resize(both)),
        r(resize(horizontal)),
        r(resize(vertical)),
        r(resize(block)),
        r(resize(inline)),
        r(resize(inherit_)),
        r(resize(unset)),
        r(resize(initial)),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"resize": "none"},
      {"resize": "both"},
      {"resize": "horizontal"},
      {"resize": "vertical"},
      {"resize": "block"},
      {"resize": "inline"},
      {"resize": "inherit"},
      {"resize": "unset"},
      {"resize": "initial"},
    ))
  )
)

describe("Backdrop filter", () =>
  test("test values", () =>
    expect(
      (
        r(backdropFilter(list{#none})),
        r(backdropFilter(list{#blur(#px(10)), #brightness(#percent(42.0))})),
        r(backdropFilter(list{#contrast(#num(10)), #dropShadow(#percent(0.5))})),
        r(backdropFilter(list{#grayscale(#percent(99.9)), #hueRotate(#deg(90.0))})),
        r(backdropFilter(list{#invert(#num(30)), #opacity(#percent(10.0))})),
        r(backdropFilter(list{#saturate(#num(30)), #sepia(#percent(10.0))})),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"backdrop-filter": "none"},
      {"backdrop-filter": "blur(10px), brightness(42%)"},
      {"backdrop-filter": "contrast(10), drop-shadow(0.5%)"},
      {"backdrop-filter": "grayscale(99.9%), hue-rotate(90deg)"},
      {"backdrop-filter": "invert(30), opacity(10%)"},
      {"backdrop-filter": "saturate(30), sepia(10%)"},
    ))
  )
)

describe("Gradient background", () =>
  test("test values", () =>
    expect(
      (
        r(background(linearGradient(deg(45.), list{(zero, red), (pct(100.), blue)}))),
        r(background(repeatingLinearGradient(deg(45.), list{(zero, red), (px(10), blue)}))),
        r(background(radialGradient(list{(zero, red), (pct(100.), blue)}))),
        r(
          background(
            repeatingRadialGradient(list{
              (zero, red),
              (
                {
                  open Calc
                  pct(20.) + px(5)
                },
                blue,
              ),
            }),
          ),
        ),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"background": "linear-gradient(45deg, #FF0000 0, #0000FF 100%)"},
      {
        "background": "repeating-linear-gradient(45deg, #FF0000 0, #0000FF 10px)",
      },
      {"background": "radial-gradient(#FF0000 0, #0000FF 100%)"},
      {
        "background": "repeating-radial-gradient(#FF0000 0, #0000FF calc(20% + 5px))",
      },
    ))
  )
)

describe("Position", () => {
  test("should use length", () =>
    expect(
      (
        r(top(px(10))),
        r(right(rem(1.))),
        r(bottom(pct(20.))),
        r(left(vh(4.))),
      )->Js.Json.stringifyAny,
    )->toBeJson(({"top": "10px"}, {"right": "1rem"}, {"bottom": "20%"}, {"left": "4vh"}))
  )

  test("should allow cascading", () =>
    expect(
      (
        r(top(initial)),
        r(right(inherit_)),
        r(bottom(unset)),
        r(left(initial)),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"top": "initial"},
      {"right": "inherit"},
      {"bottom": "unset"},
      {"left": "initial"},
    ))
  )
})

describe("object-fit", () =>
  test("test values", () =>
    expect(
      (
        r(objectFit(#fill)),
        r(objectFit(#contain)),
        r(objectFit(#cover)),
        r(objectFit(#none)),
        r(objectFit(#scaleDown)),
        r(objectFit(#inherit_)),
        r(objectFit(#initial)),
        r(objectFit(#unset)),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"objectFit": "fill"},
      {"objectFit": "contain"},
      {"objectFit": "cover"},
      {"objectFit": "none"},
      {"objectFit": "scale-down"},
      {"objectFit": "inherit"},
      {"objectFit": "initial"},
      {"objectFit": "unset"},
    ))
  )
)

describe("box-shadow", () => {
  test("should allow single or list definition", () =>
    expect(
      (
        r(boxShadow(Shadow.box(green))),
        r(boxShadows(list{Shadow.box(yellow), Shadow.box(red)})),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"boxShadow": "0 0 0 0 #008000"},
      {"boxShadow": "0 0 0 0 #FFFF00, 0 0 0 0 #FF0000"},
    ))
  )

  test("should use options when present", () =>
    expect(
      (
        r(boxShadow(Shadow.box(~x=px(1), ~y=px(2), red))),
        r(boxShadow(Shadow.box(~inset=true, red))),
      )->Js.Json.stringifyAny,
    )->toBeJson(({"boxShadow": "1px 2px 0 0 #FF0000"}, {"boxShadow": "0 0 0 0 #FF0000 inset"}))
  )

  test("should allow special values", () =>
    expect(
      (
        r(boxShadow(none)),
        r(boxShadow(inherit_)),
        r(boxShadow(initial)),
        r(boxShadow(unset)),
        r(important(boxShadow(none))),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"boxShadow": "none"},
      {"boxShadow": "inherit"},
      {"boxShadow": "initial"},
      {"boxShadow": "unset"},
      {"boxShadow": "none !important"},
    ))
  )
})

describe("text-shadow", () => {
  test("should allow single or list definition", () =>
    expect(
      (
        r(textShadow(Shadow.text(green))),
        r(textShadows(list{Shadow.text(yellow), Shadow.text(red)})),
      )->Js.Json.stringifyAny,
    )->toBeJson(({"textShadow": "0 0 0 #008000"}, {"textShadow": "0 0 0 #FFFF00, 0 0 0 #FF0000"}))
  )

  test("should use options when present", () =>
    expect(
      (
        r(textShadow(Shadow.text(~x=px(1), ~y=px(2), red))),
        r(textShadow(Shadow.text(~blur=vh(1.), red))),
      )->Js.Json.stringifyAny,
    )->toBeJson(({"textShadow": "1px 2px 0 #FF0000"}, {"textShadow": "0 0 1vh #FF0000"}))
  )

  test("should allow special values", () =>
    expect(
      (
        r(textShadow(none)),
        r(textShadow(inherit_)),
        r(textShadow(initial)),
        r(textShadow(unset)),
        r(important(textShadow(none))),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"textShadow": "none"},
      {"textShadow": "inherit"},
      {"textShadow": "initial"},
      {"textShadow": "unset"},
      {"textShadow": "none !important"},
    ))
  )
})

describe("transitions", () => {
  test("should allow single or list definition", () =>
    expect(
      (
        r(transition("transform")),
        r(transitions(list{Transition.shorthand("height"), Transition.shorthand("top")})),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"transition": "0ms ease 0ms transform"},
      {"transition": "0ms ease 0ms height, 0ms ease 0ms top"},
    ))
  )

  test("should use options when present", () =>
    expect(
      r(transition(~duration=3, ~delay=4, ~timingFunction=easeOut, "top"))->Js.Json.stringifyAny,
    )->toBeJson({"transition": "3ms ease-out 4ms top"})
  )
})

external toAnimationName: string => animationName = "%identity"

describe("animation", () => {
  test("should allow single or list definition", () =>
    expect(
      (
        r(animation(toAnimationName("a"))),
        r(
          animations(list{
            Animation.shorthand(toAnimationName("a1")),
            Animation.shorthand(toAnimationName("a2")),
          }),
        ),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"animation": "a 0ms ease 0ms 1 normal none running"},
      {
        "animation": "a1 0ms ease 0ms 1 normal none running, a2 0ms ease 0ms 1 normal none running",
      },
    ))
  )

  test("should use options when present", () =>
    expect(
      r(
        animation(
          ~duration=300,
          ~delay=400,
          ~direction=reverse,
          ~timingFunction=linear,
          ~fillMode=forwards,
          ~playState=running,
          ~iterationCount=infinite,
          toAnimationName("a"),
        ),
      )->Js.Json.stringifyAny,
    )->toBeJson({
      "animation": "a 300ms linear 400ms infinite reverse forwards running",
    })
  )
})

describe("Word spacing", () =>
  test("test values", () =>
    expect(
      (
        r(wordSpacing(#normal)),
        r(wordSpacing(vh(1.))),
        r(wordSpacing(pct(50.))),
        r(wordSpacing(inherit_)),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"wordSpacing": "normal"},
      {"wordSpacing": "1vh"},
      {"wordSpacing": "50%"},
      {"wordSpacing": "inherit"},
    ))
  )
)

describe("gridTemplateAreas", () => {
  test("takes acceptable types & cascades", () =>
    expect(
      (
        r(gridTemplateAreas(#none)),
        r(gridTemplateAreas(#areas(list{"a"}))),
        r(gridTemplateAreas(#inherit_)),
        r(gridTemplateAreas(#initial)),
        r(gridTemplateAreas(#unset)),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"gridTemplateAreas": "none"},
      {"gridTemplateAreas": "'a'"},
      {"gridTemplateAreas": "inherit"},
      {"gridTemplateAreas": "initial"},
      {"gridTemplateAreas": "unset"},
    ))
  )

  test("sucessfully combines list", () =>
    expect(r(gridTemplateAreas(#areas(list{"a a a", "b b b"})))->Js.Json.stringifyAny)->toBeJson({
      "gridTemplateAreas": "'a a a' 'b b b'",
    })
  )
})

describe("GridArea", () => {
  test("gridArea takes values & cascades", () =>
    expect(
      (
        r(gridArea(#auto)),
        r(gridArea(#ident("a"))),
        r(gridArea(#num(1))),
        r(gridArea(#numIdent(1, "a"))),
        r(gridArea(#span(#num(1)))),
        r(gridArea(#span(#ident("a")))),
        r(gridArea(#inherit_)),
        r(gridArea(#initial)),
        r(gridArea(#unset)),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"gridArea": "auto"},
      {"gridArea": "a"},
      {"gridArea": "1"},
      {"gridArea": "1 a"},
      {"gridArea": "span 1"},
      {"gridArea": "span a"},
      {"gridArea": "inherit"},
      {"gridArea": "initial"},
      {"gridArea": "unset"},
    ))
  )

  test("multi-arg functions add in slashes", () =>
    expect(
      (
        r(gridArea2(#auto, #num(1))),
        r(gridArea3(#ident("a"), #numIdent(1, "a"), #auto)),
        r(gridArea4(#num(5), #span(#num(16)), #span(#ident("b")), #auto)),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"gridArea": "auto / 1"},
      {"gridArea": "a / 1 a / auto"},
      {"gridArea": "5 / span 16 / span b / auto"},
    ))
  )
})

describe("gridTemplateCoumns", () => {
  test("concatenates list", () =>
    expect(
      r(gridTemplateColumns(list{#fr(1.), #px(100), #auto}))->Js.Json.stringifyAny,
    )->toBeJson({"gridTemplateColumns": "1fr 100px auto"})
  )

  test("unfolds repeats", () =>
    expect(
      (
        r(gridTemplateColumns(list{#repeat(#num(4), #fr(1.))})),
        r(gridTemplateColumns(list{#repeat(#num(4), #auto)})),
        r(gridTemplateColumns(list{#repeat(#num(4), #minContent)})),
        r(gridTemplateColumns(list{#repeat(#num(4), #maxContent)})),
        r(gridTemplateColumns(list{#repeat(#num(4), #minmax(#px(100), #fr(1.)))})),
        // r(gridTemplateColumns([`repeat(`num(4), `fitContent(`px(200)))])),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"gridTemplateColumns": "repeat(4, 1fr)"},
      {"gridTemplateColumns": "repeat(4, auto)"},
      {"gridTemplateColumns": "repeat(4, min-content)"},
      {"gridTemplateColumns": "repeat(4, max-content)"},
      {"gridTemplateColumns": "repeat(4, minmax(100px,1fr))"},
      //  {"gridTemplateColumns": "repeat(4, fit-content(200px))"},
    ))
  )
})

describe("backgroundPosition", () => {
  test("test single values", () =>
    expect(
      (
        r(backgroundPosition(#left)),
        r(backgroundPosition(#right)),
        r(backgroundPosition(#top)),
        r(backgroundPosition(#bottom)),
        r(backgroundPosition(center)),
        r(backgroundPosition(pct(50.))),
        r(backgroundPosition(initial)),
        r(backgroundPosition(inherit_)),
        r(backgroundPosition(unset)),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"backgroundPosition": "left"},
      {"backgroundPosition": "right"},
      {"backgroundPosition": "top"},
      {"backgroundPosition": "bottom"},
      {"backgroundPosition": "center"},
      {"backgroundPosition": "50%"},
      {"backgroundPosition": "initial"},
      {"backgroundPosition": "inherit"},
      {"backgroundPosition": "unset"},
    ))
  )

  test("test two values", () =>
    expect(
      (
        r(backgroundPosition(#hv(#left, center))),
        r(backgroundPosition(#hv(#right, pct(50.)))),
        r(backgroundPosition(#hv(pct(50.), #top))),
        r(backgroundPosition(#hv(pct(50.), pct(50.)))),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"backgroundPosition": "left center"},
      {"backgroundPosition": "right 50%"},
      {"backgroundPosition": "50% top"},
      {"backgroundPosition": "50% 50%"},
    ))
  )

  test("test multiple positions", () =>
    expect(
      r(backgroundPositions(list{#hv(px(0), px(0)), center}))->Js.Json.stringifyAny,
    )->toBeJson({"backgroundPosition": "0px 0px, center"})
  )

  test("test edge offsets values", () =>
    expect(
      r(
        backgroundPosition4(~y=#top, ~offsetY=px(10), ~x=#right, ~offsetX=px(50)),
      )->Js.Json.stringifyAny,
    )->toBeJson({"backgroundPosition": "right 50px top 10px"})
  )
})

describe("backgroundRepeat", () => {
  test("test single values", () =>
    expect(
      (
        r(backgroundRepeat(repeatX)),
        r(backgroundRepeat(repeatY)),
        r(backgroundRepeat(repeat)),
        r(backgroundRepeat(space)),
        r(backgroundRepeat(round)),
        r(backgroundRepeat(noRepeat)),
        r(backgroundRepeat(inherit_)),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"backgroundRepeat": "repeat-x"},
      {"backgroundRepeat": "repeat-y"},
      {"backgroundRepeat": "repeat"},
      {"backgroundRepeat": "space"},
      {"backgroundRepeat": "round"},
      {"backgroundRepeat": "no-repeat"},
      {"backgroundRepeat": "inherit"},
    ))
  )

  test("test two values", () =>
    expect(
      (
        r(backgroundRepeat(#hv(repeat, space))),
        r(backgroundRepeat(#hv(repeat, repeat))),
        r(backgroundRepeat(#hv(round, space))),
        r(backgroundRepeat(#hv(noRepeat, round))),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"backgroundRepeat": "repeat space"},
      {"backgroundRepeat": "repeat repeat"},
      {"backgroundRepeat": "round space"},
      {"backgroundRepeat": "no-repeat round"},
    ))
  )
})

describe("backgroundImage", () =>
  test("test values", () =>
    expect(
      (
        r(backgroundImage(none)),
        r(backgroundImage(url("x"))),
        r(backgroundImage(linearGradient(deg(5.), list{(pct(10.), red)}))),
        r(backgroundImage(repeatingLinearGradient(rad(6.), list{(pct(20.), black)}))),
        r(backgroundImage(radialGradient(list{(pct(30.), yellow)}))),
        r(backgroundImage(repeatingRadialGradient(list{(pct(30.), yellow)}))),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"backgroundImage": "none"},
      {"backgroundImage": "url(x)"},
      {"backgroundImage": "linear-gradient(5deg, #FF0000 10%)"},
      {"backgroundImage": "repeating-linear-gradient(6rad, #000000 20%)"},
      {"backgroundImage": "radial-gradient(#FFFF00 30%)"},
      {"backgroundImage": "repeating-radial-gradient(#FFFF00 30%)"},
    ))
  )
)

describe("background shorhand", () =>
  test("test values", () =>
    expect(
      (
        r(background(rgb(1, 2, 3))),
        r(background(url("x"))),
        r(background(linearGradient(deg(5.), list{(pct(10.), red)}))),
        r(background(none)),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"background": "rgb(1, 2, 3)"},
      {"background": "url(x)"},
      {"background": "linear-gradient(5deg, #FF0000 10%)"},
      {"background": "none"},
    ))
  )
)

describe("clipPath", () =>
  test("test values", () =>
    expect(
      (
        r(clipPath(none)),
        r(clipPath(url("x"))),
        r(clipPath(marginBox)),
        r(clipPath(borderBox)),
        r(clipPath(paddingBox)),
        r(clipPath(contentBox)),
        r(clipPath(fillBox)),
        r(clipPath(strokeBox)),
        r(clipPath(viewBox)),
        r(clipPath(inherit_)),
        r(clipPath(initial)),
        r(clipPath(unset)),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"clipPath": "none"},
      {"clipPath": "url(x)"},
      {"clipPath": "margin-box"},
      {"clipPath": "border-box"},
      {"clipPath": "padding-box"},
      {"clipPath": "content-box"},
      {"clipPath": "fill-box"},
      {"clipPath": "stroke-box"},
      {"clipPath": "view-box"},
      {"clipPath": "inherit"},
      {"clipPath": "initial"},
      {"clipPath": "unset"},
    ))
  )
)

describe("columnGap", () =>
  test("test values", () =>
    expect(
      (
        r(columnGap(normal)),
        r(columnGap(px(3))),
        r(columnGap(em(2.5))),
        r(columnGap(pct(3.))),
        r(columnGap(inherit_)),
        r(columnGap(initial)),
        r(columnGap(unset)),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"columnGap": "normal"},
      {"columnGap": "3px"},
      {"columnGap": "2.5em"},
      {"columnGap": "3%"},
      {"columnGap": "inherit"},
      {"columnGap": "initial"},
      {"columnGap": "unset"},
    ))
  )
)

describe("cursor", () =>
  test("test values", () =>
    expect(
      (
        //let auto: [> Types.Cursor.t];
        //let default: [> Types.Cursor.t];
        //let none: [> Types.Cursor.t];
        r(cursor(contextMenu)),
        r(cursor(help)),
        r(cursor(pointer)),
        r(cursor(progress)),
        r(cursor(wait)),
        r(cursor(cell)),
        r(cursor(crosshair)),
        r(cursor(text)),
        r(cursor(verticalText)),
        r(cursor(alias)),
        r(cursor(copy)),
        r(cursor(move)),
        r(cursor(noDrop)),
        r(cursor(notAllowed)),
        r(cursor(grab)),
        r(cursor(grabbing)),
        r(cursor(allScroll)),
        r(cursor(colResize)),
        r(cursor(rowResize)),
        r(cursor(nResize)),
        r(cursor(eResize)),
        r(cursor(sResize)),
        r(cursor(wResize)),
        r(cursor(neResize)),
        r(cursor(nwResize)),
        r(cursor(seResize)),
        r(cursor(swResize)),
        r(cursor(ewResize)),
        r(cursor(nsResize)),
        r(cursor(neswResize)),
        r(cursor(nwseResize)),
        r(cursor(zoomIn)),
        r(cursor(zoomOut)),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"cursor": "context-menu"},
      {"cursor": "help"},
      {"cursor": "pointer"},
      {"cursor": "progress"},
      {"cursor": "wait"},
      {"cursor": "cell"},
      {"cursor": "crosshair"},
      {"cursor": "text"},
      {"cursor": "vertical-text"},
      {"cursor": "alias"},
      {"cursor": "copy"},
      {"cursor": "move"},
      {"cursor": "no-drop"},
      {"cursor": "not-allowed"},
      {"cursor": "grab"},
      {"cursor": "grabbing"},
      {"cursor": "all-scroll"},
      {"cursor": "col-resize"},
      {"cursor": "row-resize"},
      {"cursor": "n-resize"},
      {"cursor": "e-resize"},
      {"cursor": "s-resize"},
      {"cursor": "w-resize"},
      {"cursor": "ne-resize"},
      {"cursor": "nw-resize"},
      {"cursor": "se-resize"},
      {"cursor": "sw-resize"},
      {"cursor": "ew-resize"},
      {"cursor": "ns-resize"},
      {"cursor": "nesw-resize"},
      {"cursor": "nwse-resize"},
      {"cursor": "zoom-in"},
      {"cursor": "zoom-out"},
    ))
  )
)
