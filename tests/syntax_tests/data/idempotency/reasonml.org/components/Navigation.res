open Util.ReactStuff
module Link = Next.Link

let link = "no-underline block text-inherit hover:cursor-pointer hover:text-white text-white-80 mb-px"
let activeLink = "text-inherit font-normal text-fire border-b border-fire"

let linkOrActiveLink = (~target, ~route) => target === route ? activeLink : link

module CollapsibleLink = {
  // KeepOpen = Menu has been opened and should stay open
  type state =
    | KeepOpen
    | HoverOpen
    | Closed

  @react.component
  let make = (
    ~title: string,
    ~onStateChange: (~id: string, state) => unit,
    ~allowHover=true,
    ~allowInteraction=true,
    ~id: string,
    ~state: state,
    ~active=false,
    ~children,
  ) => {
    // This is not onClick, because we want to prevent
    // text selection on multiple clicks
    let onMouseDown = evt => {
      ReactEvent.Mouse.preventDefault(evt)
      ReactEvent.Mouse.stopPropagation(evt)

      onStateChange(
        ~id,
        switch state {
        | Closed => KeepOpen
        | HoverOpen => Closed
        | KeepOpen => Closed
        },
      )
    }

    let onMouseEnter = evt => {
      ReactEvent.Mouse.preventDefault(evt)
      if allowHover {
        onStateChange(~id, HoverOpen)
      }
    }

    let isOpen = switch state {
    | Closed => false
    | KeepOpen
    | HoverOpen => true
    }

    // This onClick is required for iOS12 safari.
    // There seems to be a bug where mouse events
    // won't be registered, unless an onClick event
    // is attached
    // DO NOT REMOVE, OTHERWISE THE COLLAPSIBLE WON'T WORK
    let onClick = _ => ()

    let direction = isOpen ? #Up : #Down

    <div className="relative" onMouseEnter>
      <div className="flex items-center">
        <a
          onMouseDown
          onClick
          className={(active ? activeLink : link) ++
          (" border-none flex items-center hover:cursor-pointer " ++
          (isOpen ? " text-white" : ""))}>
          <span className={active ? "border-b border-fire" : ""}> {title->s} </span>
          <span className="fill-current flex-no-wrap inline-block ml-2 w-2">
            <Icon.Caret direction className={active ? "text-inherit" : "text-night-light"} />
          </span>
        </a>
      </div>
      <div
        className={(
          isOpen ? "flex" : "hidden"
        ) ++ " fixed left-0 mt-4 border-night border-t bg-night-dark min-w-20 w-full h-full sm:h-auto sm:justify-center"}>
        <div className="max-w-xl w-full"> children </div>
      </div>
    </div>
  }
}

let useOutsideClick: (ReactDOMRe.Ref.t, unit => unit) => unit = %raw(
  (outerRef, trigger) =>
    `{
      function handleClickOutside(event) {
        if (outerRef.current && !outerRef.current.contains(event.target)) {
          trigger();
        }
      }

      React.useEffect(() => {
        document.addEventListener("mousedown", handleClickOutside);
        return () => {
          document.removeEventListener("mousedown", handleClickOutside);
        };
      });

    }`
)

let useWindowWidth: unit => option<int> = %raw(
  () =>
    `{
  const isClient = typeof window === 'object';

  function getSize() {
    return {
      width: isClient ? window.innerWidth : undefined,
      height: isClient ? window.innerHeight : undefined
    };
  }

  const [windowSize, setWindowSize] = React.useState(getSize);

  React.useEffect(() => {
    if (!isClient) {
      return false;
    }

    function handleResize() {
      setWindowSize(getSize());
    }

    window.addEventListener('resize', handleResize);
    return () => window.removeEventListener('resize', handleResize);
  }, []); // Empty array ensures that effect is only run on mount and unmount

  if(windowSize) {
    return windowSize.width;
  }
  return null;
  }`
)

type collapsible = {
  title: string,
  children: string => React.element,
  href: string,
  state: CollapsibleLink.state,
}

module SubNav = {
  module DocsLinks = {
    @react.component
    let make = (~route: string) => {
      let jsTheme = ColorTheme.toCN(#Js)
      let reTheme = ColorTheme.toCN(#Reason)

      let languageItems = [
        ("Introduction", "/docs/manual/latest/introduction"),
        ("Cheatsheet", "/docs/manual/latest/syntax-cheatsheet"),
      ]

      let recompItems = [
        ("Reason Compiler", "/docs/reason-compiler/latest/introduction"),
        ("ReasonReact", "/docs/reason-react/latest/introduction"),
        ("GenType", "/docs/gentype/latest/introduction"),
      ]

      let activeThemeLink = "font-normal text-primary border-b border-primary"

      let sectionClass = "pb-12 mt-12 border-b border-night last:border-b-0 lg:w-1/3"
      let overlineClass = "font-black uppercase text-sm tracking-wide text-primary-80"

      let sectionUl = "flex flex-wrap mt-8 list-primary list-inside lg:w-auto max-w-md"

      <div className="lg:flex lg:flex-row px-4 max-w-xl">
        <div className={reTheme ++ (" " ++ sectionClass)}>
          <Link href="/docs/manual/latest/introduction">
            <a className=overlineClass> {"Language Manual"->s} </a>
          </Link>
          <ul className=sectionUl>
            {languageItems
            ->Belt.Array.mapWithIndex((idx, (title, href)) => {
              let active = route == href ? activeThemeLink ++ " hover:text-primary cursor-auto" : ""
              <li className="w-1/2 xs:w-1/2 h-10" key={Belt.Int.toString(idx)}>
                <Link href>
                  <a className={"text-white-80 hover:text-white hover:cursor-pointer " ++ active}>
                    {title->s}
                  </a>
                </Link>
              </li>
            })
            ->ate}
          </ul>
        </div>
        <div className={jsTheme ++ (" " ++ sectionClass)}>
          <Link href="/docs/reason-compiler/latest/introduction">
            <a className=overlineClass> {"JavaScript"->s} </a>
          </Link>
          <ul className=sectionUl>
            {recompItems
            ->Belt.Array.mapWithIndex((idx, (title, href)) => {
              let active = route == href ? activeThemeLink ++ " hover:text-primary cursor-auto" : ""
              <li className="w-1/2 xs:w-1/2 h-10" key={Belt.Int.toString(idx)}>
                <Link href>
                  <a className={"text-white-80 hover:text-white hover:cursor-pointer " ++ active}>
                    {title->s}
                  </a>
                </Link>
              </li>
            })
            ->ate}
          </ul>
        </div>
      </div>
    }
  }

  module ApiLinks = {
    @react.component
    let make = (~route: string) => {
      let jsTheme = ColorTheme.toCN(#Js)
      let reTheme = ColorTheme.toCN(#Reason)

      let jsItems = [
        ("Belt Stdlib", "/apis/javascript/latest/belt"),
        ("Js Module", "/apis/javascript/latest/js"),
        /* ("Module 3", "/apis/javascript/latest/mod3"), */
        /* ("Module 4", "/apis/javascript/latest/mod4"), */
      ]

      let sectionClass = "pb-12 mt-12 border-b border-night last:border-b-0 lg:w-1/4"
      let overlineClass = "font-black uppercase text-sm tracking-wide text-primary-80"

      let sectionUl = "flex flex-wrap mt-8 list-primary list-inside lg:w-auto max-w-md"

      <div className="lg:flex lg:flex-row px-4 max-w-xl">
        <div className={reTheme ++ (" " ++ sectionClass)}>
          <Link href="/apis"> <a className=overlineClass> {"Overview"->s} </a> </Link>
        </div>
        <div className={jsTheme ++ (" " ++ sectionClass)}>
          <Link href="/apis/javascript/latest">
            <a className=overlineClass> {"JavaScript"->s} </a>
          </Link>
          <ul className=sectionUl>
            {jsItems
            ->Belt.Array.mapWithIndex((idx, (title, href)) => {
              let active = Js.String2.startsWith(route, href) ? "text-primary" : ""
              <li className="w-1/2 xs:w-1/2 h-10" key={Belt.Int.toString(idx)}>
                <Link href>
                  <a className={"text-white-80 hover:text-white hover:cursor-pointer " ++ active}>
                    {title->s}
                  </a>
                </Link>
              </li>
            })
            ->ate}
          </ul>
        </div>
      </div>
    }
  }
}

module MobileNav = {
  @react.component
  let make = (~route: string) => {
    let base = "font-light mx-4 py-5 text-white-80 border-b border-night"
    let extLink = "block hover:cursor-pointer hover:text-white text-night-light"
    <div className="border-night border-t">
      <ul>
        <li className=base>
          <Link href="/try">
            <a className={linkOrActiveLink(~target="/try", ~route)}> {"Playground"->s} </a>
          </Link>
        </li>
        <li className=base>
          <Link href="/blog">
            <a className={linkOrActiveLink(~target="/blog", ~route)}> {"Blog"->s} </a>
          </Link>
        </li>
        <li className=base>
          <Link href="/community">
            <a className={linkOrActiveLink(~target="/community", ~route)}> {"Community"->s} </a>
          </Link>
        </li>
        <li className=base>
          <a
            href="https://twitter.com/reasonml"
            rel="noopener noreferrer"
            target="_blank"
            className=extLink>
            {"Twitter"->s}
          </a>
        </li>
        <li className=base>
          <a
            href="https://discord.gg/reasonml"
            rel="noopener noreferrer"
            target="_blank"
            className=extLink>
            {"Discord"->s}
          </a>
        </li>
        <li className=base>
          <a
            href="https://github.com/reason-association/reasonml.org"
            rel="noopener noreferrer"
            target="_blank"
            className=extLink>
            {"Github"->s}
          </a>
        </li>
      </ul>
    </div>
  }
}

/* isOverlayOpen: if the mobile overlay is toggled open */
@react.component
let make = (~overlayState: (bool, (bool => bool) => unit)) => {
  let minWidth = "20rem"
  let router = Next.Router.useRouter()

  let route = router.route

  let (collapsibles, setCollapsibles) = React.useState(_ => [
    {
      title: "Docs",
      href: "/docs",
      children: route => <SubNav.DocsLinks route />,
      state: Closed,
    },
    {
      title: "API",
      href: "/apis",
      children: route => <SubNav.ApiLinks route />,
      state: Closed,
    },
  ])

  let (isOverlayOpen, setOverlayOpen) = overlayState

  let toggleOverlay = () => setOverlayOpen(prev => !prev)

  let resetCollapsibles = () =>
    setCollapsibles(prev => Belt.Array.map(prev, c => {...c, state: Closed}))

  let outerRef = React.useRef(Js.Nullable.null)
  useOutsideClick(ReactDOMRe.Ref.domRef(outerRef), resetCollapsibles)

  let windowWidth = useWindowWidth()

  // Don't allow hover behavior for collapsibles if mobile navigation is on
  let allowHover = switch windowWidth {
  | Some(width) => width > 576 // Value noted in tailwind config
  | None => true
  }

  let nonCollapsibleOnMouseEnter = evt => {
    ReactEvent.Mouse.preventDefault(evt)
    resetCollapsibles()
  }

  // Client side navigation requires us to reset the collapsibles
  // whenever a route change had occurred, otherwise the collapsible
  // will stay open, even though you clicked a link
  React.useEffect1(() => {
    open Next.Router.Events
    let {Next.Router.events: events} = router

    let onChangeComplete = _url => {
      resetCollapsibles()
      setOverlayOpen(_ => false)
    }

    events->on(#routeChangeComplete(onChangeComplete))
    events->on(#hashChangeComplete(onChangeComplete))

    Some(
      () => {
        events->off(#routeChangeComplete(onChangeComplete))
        events->off(#hashChangeComplete(onChangeComplete))
      },
    )
  }, [])

  <nav
    ref={ReactDOMRe.Ref.domRef(outerRef)}
    id="header"
    style={Style.make(~minWidth, ())}
    className="fixed flex justify-center z-20 top-0 w-full h-16 bg-night-dark shadow text-white-80 text-base">
    <div className="flex justify-between pl-4 items-center h-full w-full max-w-xl">
      <div className="h-8 w-8 md:w-20 md:mb-3 ">
        <a href="/">
          <picture>
            <source srcSet="/static/reason_logo_full.svg" media="(min-width: 768px)" />
            <img className="h-8 w-auto inline-block" src="/static/reason_logo.svg" />
          </picture>
        </a>
      </div>
      /* Desktop horizontal navigation */
      <div
        className="flex sm:justify-between bg-night-dark w-10/12 sm:w-9/12 sm:h-auto sm:relative">
        <div
          className="flex justify-between w-2/4 xs:w-3/4 sm:w-full max-w-sm"
          style={Style.make(~minWidth="12rem", ())}>
          <button className="sm:hidden px-4 flex items-center justify-center h-full">
            <Icon.MagnifierGlass className="w-5 h-5 hover:text-white" />
          </button>
          {Belt.Array.mapWithIndex(collapsibles, (idx, c) => {
            let {href, title, children, state} = c
            let onStateChange = (~id, state) =>
              setCollapsibles(prev => {
                /* This is important to close the nav overlay, before showing the subnavigation */
                if isOverlayOpen {
                  toggleOverlay()
                }
                Belt.Array.map(prev, c =>
                  if c.title === id {
                    {
                      ...c,
                      state: state,
                    }
                  } else {
                    {
                      ...c,
                      state: Closed,
                    }
                  }
                )
              })
            <CollapsibleLink
              id=title
              onStateChange
              key={idx->Belt.Int.toString}
              allowHover
              title
              active={Js.String2.startsWith(route, href)}
              state>
              {children(route)}
            </CollapsibleLink>
          })->ate}
          <Link href="/try">
            <a
              className={"hidden xs:block " ++ linkOrActiveLink(~target="/try", ~route)}
              onMouseEnter=nonCollapsibleOnMouseEnter>
              {"Playground"->s}
            </a>
          </Link>
          <Link href="/blog">
            <a
              className={"hidden sm:block " ++ linkOrActiveLink(~target="/blog", ~route)}
              onMouseEnter=nonCollapsibleOnMouseEnter>
              {"Blog"->s}
            </a>
          </Link>
          <Link href="/community">
            <a
              className={"hidden sm:block " ++ linkOrActiveLink(~target="/community", ~route)}
              onMouseEnter=nonCollapsibleOnMouseEnter>
              {"Community"->s}
            </a>
          </Link>
        </div>
        <div className="hidden lg:-mr-6 lg:flex lg:justify-between lg:w-20 ">
          <a
            href="https://github.com/reason-association/reasonml.org"
            rel="noopener noreferrer"
            target="_blank"
            className=link
            onMouseEnter=nonCollapsibleOnMouseEnter>
            <Icon.Github className="w-5 h-5" />
          </a>
          <a
            href="https://twitter.com/reasonml"
            rel="noopener noreferrer"
            target="_blank"
            className=link
            onMouseEnter=nonCollapsibleOnMouseEnter>
            <Icon.Twitter className="w-5 h-5" />
          </a>
          <a
            href="https://discord.gg/reasonml"
            rel="noopener noreferrer"
            target="_blank"
            className=link
            onMouseEnter=nonCollapsibleOnMouseEnter>
            <Icon.Discord className="w-5 h-5" />
          </a>
        </div>
      </div>
      <button
        className="hidden sm:flex sm:px-4 sm:items-center sm:justify-center sm:border-l sm:border-r sm:border-night sm:h-full">
        <Icon.MagnifierGlass className="w-5 h-5 hover:text-white" />
      </button>
    </div>
    /* Burger Button */
    <button
      className="h-full px-4 sm:hidden flex items-center hover:text-white"
      onClick={evt => {
        ReactEvent.Mouse.preventDefault(evt)
        resetCollapsibles()
        toggleOverlay()
      }}>
      <Icon.DrawerDots className={"h-1 w-auto block " ++ (isOverlayOpen ? "text-fire" : "")} />
    </button>
    /* Mobile overlay */
    <div
      style={Style.make(~minWidth, ~top="4rem", ())}
      className={(
        isOverlayOpen ? "flex" : "hidden"
      ) ++ " sm:hidden flex-col fixed top-0 left-0 h-full w-full sm:w-9/12 bg-night-dark sm:h-auto sm:flex sm:relative sm:flex-row sm:justify-between"}>
      <MobileNav route />
    </div>
  </nav>
}
