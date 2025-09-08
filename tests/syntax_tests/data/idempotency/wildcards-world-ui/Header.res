let hambergerMenu = {
  open Css
  style(list{
    display(#none),
    textAlign(#center),
    media("(max-width: 1126px)", list{display(#block)}),
  })
}

let fullScreenMenu = {
  open Css
  style(list{
    display(#block),
    textAlign(#center),
    media("(max-width: 1126px)", list{display(#none)}),
  })
}

let headerNav = {
  open Css
  style(list{textDecoration(none), marginRight(em(2.))})
}

let navItemStyles = {
  open Css
  style(list{
    display(#inlineBlock),
    margin2(~h=rem(0.8), ~v=px(0)),
    borderBottom(px(1), #solid, #transparent),
    selector(":hover", list{borderBottom(px(1), #solid, Styles.wildCardGreen)}),
    selector(
      "a",
      list{
        fontStyle(#italic),
        fontSize(rem(1.4)),
        color(Styles.wildCardGreen),
        fontWeight(#num(200)),
        selector(":hover", list{borderBottom(px(1), #solid, #transparent)}),
      },
    ),
  })
}

let floatingMenu = shouldDisplay => {
  open Css
  style(list{
    position(#fixed),
    top(px(0)),
    left(px(0)),
    width(#percent(100.)),
    height(vh(100.)),
    visibility(shouldDisplay ? #visible : #hidden),
    backgroundColor(rgba(255, 255, 255, shouldDisplay ? 0.5 : 0.)),
    display(#flex),
    alignItems(#center),
    justifyContent(#center),
    overflow(#hidden),
    zIndex(1000),
    transition(~duration=600, ~delay=0, ~timingFunction=ease, "all"),
    selector(
      ".zoom-in-effect",
      list{
        background(rgba(107, 173, 62, 0.3)),
        width(vw(100.)),
        height(vh(100.)),
        borderRadius(#percent(50.)),
        border(px(1), #solid, Styles.wildCardGreen),
        display(#flex),
        flex(#none),
        alignItems(#center),
        justifyContent(#center),
        transform(shouldDisplay ? scale(1., 1.) : scale(0., 0.)),
        transition(~duration=300, ~delay=0, ~timingFunction=ease, "all"),
      },
    ),
  })
}

let hamburgerSvg = () =>
  <svg
    className={
      open Css
      style(list{
        zIndex(1001),
        transition(~duration=500, ~delay=0, ~timingFunction=ease, "transform"),
        selector(":hover", list{transform(rotate(deg(180.)))}),
      })
    }
    height="32px"
    id="Layer_1"
    version="1.1"
    fill={"#" ++ "555555"}
    width="32px">
    <path
      d="M4,10h24c1.104,0,2-0.896,2-2s-0.896-2-2-2H4C2.896,6,2,6.896,2,8S2.896,10,4,10z M28,14H4c-1.104,0-2,0.896-2,2  s0.896,2,2,2h24c1.104,0,2-0.896,2-2S29.104,14,28,14z M28,22H4c-1.104,0-2,0.896-2,2s0.896,2,2,2h24c1.104,0,2-0.896,2-2  S29.104,22,28,22z"
    />
  </svg>

let closeSvg = () =>
  <svg
    height="32px"
    className={
      open Css
      style(list{
        zIndex(1002),
        transition(~duration=500, ~delay=0, ~timingFunction=ease, "transform"),
        selector(":hover", list{transform(rotate(deg(180.)))}),
      })
    }
    viewBox="0 0 512 512"
    fill={"#" ++ "222222"}
    width="32px">
    <path
      d="M437.5,386.6L306.9,256l130.6-130.6c14.1-14.1,14.1-36.8,0-50.9c-14.1-14.1-36.8-14.1-50.9,0L256,205.1L125.4,74.5  c-14.1-14.1-36.8-14.1-50.9,0c-14.1,14.1-14.1,36.8,0,50.9L205.1,256L74.5,386.6c-14.1,14.1-14.1,36.8,0,50.9  c14.1,14.1,36.8,14.1,50.9,0L256,306.9l130.6,130.6c14.1,14.1,36.8,14.1,50.9,0C451.5,423.4,451.5,400.6,437.5,386.6z"
    />
  </svg>

type closeModal = unit => unit
type isMobile = bool
type navItem = {
  shouldDisplay: bool,
  shouldDisplayMobile: bool,
  component: (closeModal, isMobile) => React.element,
}

@react.component
let make = (~navItems: array<navItem>) => {
  let (isOpen, setIsOpen) = React.useState(_ => false)

  let menuItems = isMobile =>
    <ul
      className={isMobile
        ? {
            open Css
            style(list{
              display(#flex),
              flexDirection(#column),
              alignItems(#center),
              padding(px(0)),
              margin(px(0)),
            })
          }
        : Styles.navList}>
      {navItems
      ->Array.mapWithIndex((index, {shouldDisplay, shouldDisplayMobile, component}) =>
        (shouldDisplay && !isMobile) || (shouldDisplayMobile && isMobile)
          ? <li
              key={index->string_of_int}
              className={
                open Cn
                make(list{
                  ifTrue(navItemStyles, isMobile),
                  ifTrue(
                    {
                      open Css
                      style(list{
                        backgroundColor(white),
                        borderBottom(px(1), #solid, Styles.wildCardGreen),
                        display(#block),
                        width(#percent(100.)),
                        selector(":hover", list{backgroundColor(Styles.wildCardBlue)}),
                        selector(":focus", list{backgroundColor(Styles.wildCardBlue)}),
                        selector(
                          ":active",
                          list{
                            backgroundColor(Styles.wildCardGreen),
                            selector("a", list{color(white)}),
                          },
                        ),
                        // I have to put the padding inside the child element here because the whole surface of the button needs to be clickable
                        selector(" > *", list{display(#block), width(#auto), padding(em(2.))}),
                      })
                    },
                    isMobile,
                  ),
                })
              }>
              {component(() => setIsOpen(_ => false), isMobile)}
            </li>
          : React.null
      )
      ->React.array}
    </ul>

  let clearAndPush = RootProvider.useClearNonUrlStateAndPushRoute()

  <header className=Styles.header>
    <div className=Styles.navBox>
      <a
        className={Cn.make(list{
          Styles.clickableLink,
          {
            open Css
            style(list{marginLeft(#px(80)), zIndex(1001)})
          },
        })}
        onClick={event => {
          ReactEvent.Mouse.preventDefault(event)
          clearAndPush("#")
        }}>
        <div className=Styles.headerLogo> <WildcardsLogo maxWidth="258px" /> </div>
      </a>
      <nav className={Cn.make(list{headerNav, fullScreenMenu})}> {menuItems(false)} </nav>
      <nav className={Cn.make(list{headerNav, hambergerMenu})}>
        <div
          className={
            open Css
            style(list{
              zIndex(1010),
              // NOTE: this needs to have absolute position so that it appears on top of the 'fixed' positioned overlay.
              position(#absolute),
              top(px(0)),
              right(px(0)),
              padding(px(30)),
            })
          }
          onClick={_ => setIsOpen(isOpen => !isOpen)}>
          {isOpen ? <> {closeSvg()} </> : hamburgerSvg()}
        </div>
        <div className={floatingMenu(isOpen)}>
          <div className="zoom-in-effect"> {menuItems(true)} </div>
        </div>
      </nav>
    </div>
  </header>
}
