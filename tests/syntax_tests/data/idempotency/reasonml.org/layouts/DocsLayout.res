// This module is used for all plain prose text related
// Docs, mostly /docs/manual/latest and similar sections

open Util.ReactStuff
module Link = Next.Link

module Toc = {
  type entry = {
    header: string,
    href: string,
  }

  type t = {
    title: string,
    entries: array<entry>,
  }

  @react.component
  let make = (~entries: array<entry>) =>
    <ul className="mt-2 mb-6 border-l border-primary">
      {Belt.Array.map(entries, ({header, href}) =>
        <li key=header className="pl-2 mt-1">
          <Link href>
            <a className="font-medium block text-sm text-night-light hover:text-primary">
              {header->s}
            </a>
          </Link>
        </li>
      )->ate}
    </ul>
}

module Sidebar = {
  module Title = {
    @react.component
    let make = (~children) => {
      let className = "font-sans font-black text-night-light tracking-wide text-xs uppercase mt-5"

      <div className> children </div>
    }
  }

  module NavItem = {
    // Navigation point information
    type t = {
      name: string,
      href: string,
    }
    @react.component
    let make = (
      ~getActiveToc: option<t => option<Toc.t>>=?,
      ~isItemActive: t => bool=_nav => false,
      ~isHidden=false,
      ~items: array<t>,
    ) =>
      <ul className="mt-2 text-sm font-medium">
        {Belt.Array.map(items, m => {
          let hidden = isHidden ? "hidden" : "block"
          let active = isItemActive(m)
            ? ` bg-primary-15 text-primary-dark rounded -mx-2 px-2 font-bold block `
            : ""

          let activeToc = switch getActiveToc {
          | Some(getActiveToc) => getActiveToc(m)
          | None => None
          }

          <li key=m.name className={hidden ++ " mt-2 leading-5 w-4/5"}>
            <Link href=m.href>
              <a
                className={"truncate block py-1 md:h-auto text-night-darker hover:text-primary " ++
                active}>
                {m.name->s}
              </a>
            </Link>
            {switch activeToc {
            | Some({entries}) =>
              if Belt.Array.length(entries) === 0 {
                React.null
              } else {
                <Toc entries />
              }
            | None => React.null
            }}
          </li>
        })->ate}
      </ul>
  }

  module Category = {
    type t = {
      name: string,
      items: array<NavItem.t>,
    }

    @react.component
    let make = (~getActiveToc=?, ~isItemActive: option<NavItem.t => bool>=?, ~category: t) =>
      <div key=category.name className="my-12">
        <Title> {category.name->s} </Title>
        <NavItem ?isItemActive ?getActiveToc items=category.items />
      </div>
  }

  module ToplevelNav = {
    @react.component
    let make = (~title="", ~backHref=?, ~version=?) => {
      let back = switch backHref {
      | Some(href) =>
        <Link href>
          <a className="w-5 h-5"> <Icon.CornerLeftUp className="w-full h-full" /> </a>
        </Link>
      | None => React.null
      }

      let versionTag = switch version {
      | Some(version) => <Tag kind=#Subtle text=version />
      | None => React.null
      }

      <div className="flex items-center justify-between my-4 w-full">
        <div className="flex items-center w-2/3">
          back
          <span className="ml-2 font-sans font-black text-night-dark text-xl"> {title->s} </span>
        </div>
        <div className="ml-auto"> versionTag </div>
      </div>
    }
  }

  module CollapsibleSection = {
    module NavUl = {
      // Navigation point information
      type t = {
        name: string,
        href: string,
      }

      @react.component
      let make = (
        ~onItemClick: option<ReactEvent.Mouse.t => unit>=?,
        ~isItemActive: t => bool=_nav => false,
        ~items: array<t>,
      ) =>
        <ul className="mt-3 text-night">
          {Belt.Array.map(items, m => {
            let active = isItemActive(m)
              ? ` bg-primary-15 text-primary-dark -ml-1 px-2 font-bold block `
              : ""
            <li key=m.name className="leading-5 w-4/5">
              <Link href=m.href>
                <a
                  onClick=?onItemClick
                  className={"truncate block pl-3 h-8 md:h-auto border-l-2 border-night-10 block text-night hover:pl-4 hover:text-night-dark" ++
                  active}>
                  {m.name->s}
                </a>
              </Link>
            </li>
          })->ate}
        </ul>
    }
    @react.component
    let make = (
      ~onHeaderClick: option<ReactEvent.Mouse.t => unit>=?,
      ~isItemActive=?,
      ~headers: array<string>,
      ~moduleName: string,
    ) => {
      let (collapsed, setCollapsed) = React.useState(() => false)
      let items = Belt.Array.map(headers, header => {
        open NavUl
        {name: header, href: "#" ++ header}
      })

      let direction = collapsed ? #Down : #Up

      <div className="py-3 px-3 bg-primary-15 rounded-lg">
        <a
          className="flex justify-between items-center cursor-pointer text-primary hover:text-primary text-night-dark text-base"
          href="#"
          onClick={evt => {
            ReactEvent.Mouse.preventDefault(evt)
            setCollapsed(isCollapsed => !isCollapsed)
          }}>
          {moduleName->s}
          <span className="ml-2 block text-primary"> <Icon.Caret size=#Md direction /> </span>
        </a>
        {if !collapsed {
          <NavUl ?isItemActive onItemClick=?onHeaderClick items />
        } else {
          React.null
        }}
      </div>
    }
  }

  // subitems: list of functions inside given module (defined by route)
  @react.component
  let make = (
    ~categories: array<Category.t>,
    ~route: string,
    ~toplevelNav=React.null,
    ~title: option<string>=?,
    ~preludeSection=React.null,
    ~activeToc: option<Toc.t>=?,
    ~isOpen: bool,
    ~toggle: unit => unit,
  ) => {
    let isItemActive = (navItem: NavItem.t) => navItem.href === route

    let getActiveToc = (navItem: NavItem.t) =>
      if navItem.href === route {
        activeToc
      } else {
        None
      }

    <>
      <div
        className={(
          isOpen ? "fixed w-full left-0 h-full z-10 min-w-20" : "hidden "
        ) ++ " md:block md:w-1/4 md:h-auto md:relative overflow-y-visible bg-white md:relative"}>
        <aside
          className="relative top-0 px-4 w-full block md:top-16 md:pt-10 md:sticky border-r border-snow-dark overflow-y-auto scrolling-touch pb-24"
          style={Style.make(~height="calc(100vh - 4rem", ())}>
          <div className="flex justify-between">
            <div className="w-3/4 md:w-full"> toplevelNav </div>
            <button
              onClick={evt => {
                ReactEvent.Mouse.preventDefault(evt)
                toggle()
              }}
              className="md:hidden h-16">
              <Icon.Close />
            </button>
          </div>
          preludeSection
          /* Firefox ignores padding in scroll containers, so we need margin
               to make a bottom gap for the sidebar.
               See https://stackoverflow.com/questions/29986977/firefox-ignores-padding-when-using-overflowscroll
 */
          <div className="mb-56">
            {categories
            ->Belt.Array.map(category =>
              <div key=category.name> <Category getActiveToc isItemActive category /> </div>
            )
            ->ate}
          </div>
        </aside>
      </div>
    </>
  }
}

module UrlPath = SidebarLayout.UrlPath
module NavItem = Sidebar.NavItem
module Category = Sidebar.Category

@react.component
let make = (
  ~breadcrumbs: option<list<UrlPath.breadcrumb>>=?,
  ~title: string,
  ~version: option<string>=?,
  ~activeToc: option<Toc.t>=?,
  ~categories: array<Category.t>,
  ~components=Markdown.default,
  ~theme=#Reason,
  ~children,
) => {
  let router = Next.Router.useRouter()
  let route = router.route

  let (isSidebarOpen, setSidebarOpen) = React.useState(_ => false)
  let toggleSidebar = () => setSidebarOpen(prev => !prev)

  React.useEffect1(() => {
    open Next.Router.Events
    let {Next.Router.events: events} = router

    let onChangeComplete = _url => setSidebarOpen(_ => false)

    events->on(#routeChangeComplete(onChangeComplete))
    events->on(#hashChangeComplete(onChangeComplete))

    Some(
      () => {
        events->off(#routeChangeComplete(onChangeComplete))
        events->off(#hashChangeComplete(onChangeComplete))
      },
    )
  }, [])

  let preludeSection =
    <div className="flex justify-between text-primary font-medium items-baseline">
      {title->s}
      {switch version {
      | Some(version) => <span className="font-mono text-sm"> {version->s} </span>
      | None => React.null
      }}
    </div>

  let sidebar =
    <Sidebar
      isOpen=isSidebarOpen toggle=toggleSidebar preludeSection title ?activeToc categories route
    />

  <SidebarLayout theme components sidebarState=(isSidebarOpen, setSidebarOpen) sidebar ?breadcrumbs>
    children
  </SidebarLayout>
}
