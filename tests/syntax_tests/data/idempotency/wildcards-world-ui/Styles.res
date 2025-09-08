// module Styles = {
/* Open the Css module, so we can access the style properties below without prefixing them with Css. */
open Css

let backgroundImageGorilla = "/img/wildcardsimages/wild-jungle-background.jpg"

global(
  "body",
  list{
    margin(#px(0)),
    fontFamily(#custom("Lato, Helvetica, sans-serif")),
    overflowX(hidden),
    width(#percent(100.)),
    position(#relative),
  },
)
global("a", list{color(hex("303030")), textDecoration(none)})
global("a:hover", list{textDecoration(underline)})
global("a:active", list{color(hex("303030"))})
global("a:visited", list{color(hex("303030"))})

let app = style(list{
  boxSizing(#borderBox),
  // Make a global default font size
  fontSize(px(16)),
})

let textOnlyModalText = style(list{padding(em(2.))})
let totalRaisedText = size => style(list{fontSize(em(size))})

let copyButton = style(list{Css.float(#right), zIndex(50)})

// Use flex box to center
let centerItems = style(list{display(#flex), alignItems(center), width(#percent(100.))})
// Use margin to center
let centerItemsMargin = style(list{display(block), margin(auto)})

let translationSwitch = style(list{padding(rem(0.))})

let topBody = style(list{
  // zIndex(50),
  // display(`flex),
  // flexDirection(`row),
  justifyContent(#spaceBetween),
  // alignItems(center),
  media("(max-width: 831px)", list{textAlign(center)}),
  // flexWrap(`wrap),
  paddingLeft(px(15)),
  paddingRight(px(15)),
  // paddingBottom(px(120)),
  maxWidth(px(1200)),
  margin(auto),
})

let header = style(list{position(relative)})
let noMarginBottom = style(list{marginBottom(px(0))})
let noMarginTop = style(list{marginTop(px(0))})

let headerLogo = style(list{media("(max-width: 630px)", list{textAlign(center), display(#block)})})

let nav = style(list{
  position(absolute),
  maxWidth(px(1200)),
  zIndex(200),
  top(px(0)),
  right(px(0)),
  left(px(0)),
  margin2(~v=em(0.), ~h=auto),
})

let navBox = style(list{display(#flex), justifyContent(#spaceBetween), alignItems(center)})

let navList = style(list{
  display(#flex),
  alignItems(#center),
  media("(max-width: 630px)", list{display(none)}),
  marginLeft(#auto),
  listStyle(#none, #inside, #none),
})

let positionRelative = style(list{position(relative)})
let overlayImg = (topPosition, leftPosition) =>
  style(list{
    position(absolute),
    zIndex(2),
    top(#percent(topPosition)),
    left(#percent(leftPosition)),
    width(#percent(20.)),
    height(#percent(20.)),
    minWidth(px(50)),
    minHeight(px(50)),
  })
let overlayFlameImg = overlayImg(20., 0.)
let prettyTransparent = rgba(255, 255, 255, 0.5)
let imageHoverStyle = hover(list{
  filter(list{#saturate(150.), #brightness(110.)}),
  overflow(visible),
  backgroundColor(prettyTransparent),
  boxShadow(Shadow.box(~blur=px(20), ~spread=px(20), prettyTransparent)),
  transform(scale(1.3, 1.3)),
  transition(~duration=100, ~delay=0, ~timingFunction=ease, "all"),
})
let overlayBadgeImg = (~x, ~y) =>
  Cn.make(list{overlayImg(x, y), style(list{borderRadius(#percent(100.)), imageHoverStyle})})

let streakText = style(list{
  position(absolute),
  zIndex(100),
  bottom(#percent(-30.)),
  right(#percent(50.)),
  transform(translateX(#percent(50.))),
})
let flameImg = style(list{width(#percent(100.)), maxWidth(em(2.1))})

let navListItem = style(list{display(inlineBlock)})

let navListItemToggle = style(list{display(#flex), justifyContent(center), alignItems(center)})

let someMarginRight = style(list{marginRight(px(6))})

let navListText = style(list{color(rgb(136, 136, 136)), padding(rem(1.)), fontWeight(bold)})

let leftTopHeader = style(list{
  position(relative),
  width(px(550)),
  maxWidth(px(550)),
  paddingTop(px(70)),
  paddingBottom(px(70)),
  // boxSizing(`borderBox),
  // margin(px(0)),
  // padding(px(0)),
})

let centerText = style(list{textAlign(center)})

/// NOTE: the padding of 4% is calculated as 12/3 since there is a right padding of 12% and 3 animal images shown.
let dappImagesCounteractOffset = style(list{marginLeft(#percent(4.))})

let heading = style(list{fontSize(em(3.)), media("(min-width: 768px)", list{paddingTop(#rem(3.))})})

let subHeading = style(list{fontSize(em(1.8)), fontWeight(#num(200))})

let wildCardGreen = rgb(107, 173, 62)
let wildCardBlue = rgb(114, 199, 215)
let wildCardGrey = rgb(100, 100, 100)
let colorGreen = style(list{color(wildCardGreen)})
let colorBlue = style(list{color(wildCardBlue)})
let colorGrey = style(list{color(wildCardGrey)})

let animalBox = style(list{marginRight(#percent(12.))})

let clickableLink = style(list{
  media("(max-width: 630px)", list{width(#percent(100.))}),
  cursor(#pointer),
})

let mainImageHoverStyle = scalar =>
  hover(list{
    filter(list{#saturate(150.), #brightness(110.)}),
    zIndex(2),
    overflow(visible),
    transform(scale(1.1 *. scalar, 1.1 *. scalar)),
    transition(~duration=100, ~delay=0, ~timingFunction=ease, "all"),
  })
let headerImg = (enlargement, scalar) =>
  style(list{
    position(#relative),
    zIndex(1),
    maxHeight(px(500)),
    width(#percent(100. *. enlargement)),
    left(#percent(-50. *. (enlargement -. 1.))),
    transform(scale(scalar, scalar)),
    textAlign(center),
    transition(~duration=1000, ~delay=0, ~timingFunction=ease, "all"),
    mainImageHoverStyle(scalar),
  })

let horizantalBlueTile = style(list{
  width(#percent(100.)),
  padding2(~v=em(2.), ~h=em(0.)),
  backgroundColor(#hex("73c7d7ff")),
  // media("(max-width: 831px)", [padding2(~v=rem(2.), ~h=rem(2.4))]),
  textAlign(#center),
})

let explainerLargeText = style(list{
  fontSize(rem(2.)),
  color(#hex("486269")),
  padding2(~v=em(0.), ~h=em(6.)),
  margin2(~v=em(0.), ~h=auto),
  maxWidth(px(1200)),
  media("(max-width: 760px)", list{fontSize(rem(1.2)), padding2(~v=em(0.), ~h=em(2.))}),
})
let explainerMediumText = style(list{
  media("(max-width: 760px)", list{fontSize(rem(1.1)), padding2(~v=em(0.), ~h=em(2.))}),
  padding2(~v=em(0.), ~h=em(3.)),
  margin2(~v=em(0.), ~h=auto),
  maxWidth(px(1200)),
  fontSize(rem(1.6)),
  color(#hex("486269")),
  textAlign(#center),
})

let boldExplainerText = style(list{color(#hex("303030")), fontWeight(#bold)})

let infoBackground = style(list{
  backgrounds(list{
    #linearGradient(
      deg(0.),
      list{(zero, #rgba(255, 255, 255, 0.2)), (zero, #rgba(255, 255, 255, 0.2))},
    ),
    #url(backgroundImageGorilla),
  }),
  backgroundSize(#cover),
  backgroundRepeat(#noRepeat),
  height(#percent(100.)),
})

let infoCardContainer = style(list{padding(#rem(1.))})

let infoCardStyles = style(list{
  margin(#rem(4.)),
  media("(max-width: 831px)", list{margin(#rem(0.)), textAlign(center)}),
})

let animalImage = style(list{media("(max-width: 831px)", list{display(none)})})

let redDisclaimer = style(list{fontSize(#rem(0.9)), color(#hex("e85723"))})

let floatingSignupBox = style(list{
  maxWidth(px(900)),
  margin2(~v=em(0.), ~h=auto),
  // backgroundColor(`hex("fff")),
  // boxShadow10(px(0), px(2), px(4), px(0), `rgba((136,144,195,0.2)), px(0), px(5), px(15), px(0), `rgba((37,44,97,0.15)))
})
let floatingSignupBoxInner = style(list{padding(em(3.))})
let emailSignupHeader = style(list{fontSize(px(24))})
let emailTextBox = style(list{})

let inputElements = style(list{padding(em(0.1))})

let extraInfoFooterBox = style(list{
  textAlign(center),
  maxWidth(px(600)),
  backgroundColor(rgb(107, 173, 62)),
  margin2(~v=em(6.), ~h=auto),
  position(relative),
})

let emoticonHeader = style(list{
  position(absolute),
  left(px(0)),
  right(px(0)),
  top(px(0)),
  margin(auto),
  transform(translateY(#percent(-50.))),
  textAlign(center),
})

let emojiStyles = style(list{
  height(px(55)),
  maxHeight(#percent(100.)),
  position(relative),
  verticalAlign(middle),
  bottom(px(1)),
})

let finalNoteContent = style(list{padding(rem(1.5)), margin(ex(4.))})

let whiteText = style(list{important(color(white))})

let linkPillBox = style(list{
  marginTop(px(25)),
  boxSizing(borderBox),
  minHeight(px(25)),
  fontSize(px(14)),
  borderRadius(px(25)),
  color(hex("fff")),
  backgroundColor(rgba(48, 48, 48, 0.12)),
})
let linkPillText = style(list{margin(px(10)), marginBottom(px(1)), color(hex("fff"))})

let betaBanner = style(list{position(#absolute), left(#px(0)), top(#px(0)), width(#px(80))})

let loginButton = style(list{paddingLeft(#px(10))})

let forwardBackButton = style(list{width(#percent(100.)), height(#em(5.))})

// let mapStylesFunction = [%raw (prev, next) => "`${prev} ${next}`"];
let mergeStyles: list<string> => string = styles =>
  /* styles->Belt.List.reduce("", mapStylesFunction); */
  styles->Belt.List.reduce("", (prev, next) => prev ++ (" " ++ next))

let wrapText = style(list{overflowWrap(#breakWord), wordWrap(#breakWord)})

let fiftyPercentWidth = style(list{width(#percent(50.))})

let carousel = style(list{position(#relative), width(#percent(150.)), left(#percent(-25.))})

let invisibleGorilla = style(list{display(#none)})

let fadeOut = targetOpacity =>
  style(
    list{
      width(#percent(100.)),
      transition(~duration=2000, ~delay=0, ~timingFunction=ease, "opacity"),
    }->List.concat(list{opacity(targetOpacity)}),
  )

let carouselArrow = (~absolutePosition=true, onLeft) =>
  style(
    \"@"(
      list{
        cursor(#pointer),
        padding(#px(20)),
        color(white),
        backgroundColor(hex("72c7d7")),
        hover(
          \"@"(
            list{backgroundColor(hex("40b2c9"))},
            onLeft
              ? list{paddingLeft(#px(15)), paddingRight(#px(25))}
              : list{paddingRight(#px(15)), paddingLeft(#px(25))},
          ),
        ),
        borderRadius(px(4)),
        zIndex(3),
      },
      absolutePosition
        ? list{
            position(absolute),
            transform(translateX(#percent(-50.))),
            onLeft ? left(#percent(20.)) : left(#percent(80.)),
          }
        : list{},
    ),
  )
