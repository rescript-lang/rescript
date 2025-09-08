module Link = Next.Link

// Structure defined by `scripts/extract-tocs.js`
let tocData: dict<{
  "title": string,
  "headers": array<{
    "name": string,
    "href": string,
  }>,
}> = %raw("require('../index_data/reason_react_toc.json')")

module UrlPath = DocsLayout.UrlPath
module NavItem = DocsLayout.NavItem
module Category = DocsLayout.Category
module Toc = DocsLayout.Toc

let overviewNavs = [
  {
    open NavItem
    {
      name: "Introduction",
      href: "/docs/reason-react/latest/introduction",
    }
  },
  {name: "Installation", href: "/docs/reason-react/latest/installation"},
  {name: "Intro Example", href: "/docs/reason-react/latest/intro-example"},
]

let coreNavs = [
  {
    open NavItem
    {name: "Components", href: "/docs/reason-react/latest/components"}
  },
  {name: "JSX (Version 3)", href: "/docs/reason-react/latest/jsx"},
  {name: "Event", href: "/docs/reason-react/latest/event"},
  {name: "Style", href: "/docs/reason-react/latest/style"},
  {name: "Router", href: "/docs/reason-react/latest/router"},
  {name: "Working with DOM", href: "/docs/reason-react/latest/dom"},
  {name: "Refs in React", href: "/docs/reason-react/latest/refs"},
]

let idiomNavs = [
  {
    open NavItem
    {
      name: "Invalid Prop Name",
      href: "/docs/reason-react/latest/invalid-prop-name",
    }
  },
  {name: "Props Spread", href: "/docs/reason-react/latest/props-spread"},
  {
    name: "Component as Prop",
    href: "/docs/reason-react/latest/component-as-prop",
  },
  {
    name: "Ternary Shortcut",
    href: "/docs/reason-react/latest/ternary-shortcut",
  },
  {
    name: "Context & Mixins",
    href: "/docs/reason-react/latest/context-mixins",
  },
  {
    name: "Custom Class / Component Property",
    href: "/docs/reason-react/latest/custom-class-component-property",
  },
]

let recordApiNavs = [
  {
    open NavItem
    {name: "JSX (Old, v2)", href: "/docs/reason-react/latest/jsx-2"}
  },
  {
    name: "Creation, Props & Self",
    href: "/docs/reason-react/latest/creation-props-self",
  },
  {name: "Render", href: "/docs/reason-react/latest/render"},
  {
    name: "Callback Handlers",
    href: "/docs/reason-react/latest/callback-handlers",
  },
  {
    name: "State, Action & Reducer",
    href: "/docs/reason-react/latest/state-actions-reducer",
  },
  {name: "Lifecycles", href: "/docs/reason-react/latest/lifecycles"},
  {
    name: "Instance Variables",
    href: "/docs/reason-react/latest/instance-variables",
  },
  {name: "React Ref", href: "/docs/reason-react/latest/react-ref"},
  {
    name: "Talk to Existing ReactJS Code",
    href: "/docs/reason-react/latest/interop",
  },
  {name: "cloneElement", href: "/docs/reason-react/latest/clone-element"},
  {name: "Children", href: "/docs/reason-react/latest/children"},
  {
    name: "Subscriptions Helper",
    href: "/docs/reason-react/latest/subscriptions-helper",
  },
  {name: "Router", href: "/docs/reason-react/latest/router-2"},
]

let miscNavs = [
  {
    open NavItem
    {name: "FAQ", href: "/docs/reason-react/latest/faq"}
  },
]

let categories = [
  {
    open Category
    {name: "Getting Started", items: overviewNavs}
  },
  {name: "Core", items: coreNavs},
  {name: "ReactJS Idiom Equivalents", items: idiomNavs},
  {name: "Record API (deprecated)", items: recordApiNavs},
  {name: "Miscellaneous", items: miscNavs},
]

@react.component
let make = (~components=Markdown.default, ~children) => {
  let router = Next.Router.useRouter()
  let route = router.route

  let activeToc: option<Toc.t> = {
    open Belt.Option
    Js.Dict.get(tocData, route)->map(data => {
      let title = data["title"]
      let entries = Belt.Array.map(data["headers"], header => {
        Toc.header: header["name"],
        href: "#" ++ header["href"],
      })
      {Toc.title: title, entries: entries}
    })
  }

  let urlPath = UrlPath.parse(~base="/docs/reason-react", route)

  let breadcrumbs = Belt.Option.map(urlPath, v => {
    let {UrlPath.version: version} = v
    let prefix = {
      open UrlPath
      list{
        {name: "Docs", href: "/docs"},
        {
          name: "ReasonReact",
          href: "/docs/reason-react/" ++ (version ++ "/introduction"),
        },
      }
    }
    UrlPath.toBreadCrumbs(~prefix, v)
  })

  let title = "ReasonReact"
  let version = "v0.7"

  <DocsLayout theme=#Js components categories version title ?activeToc ?breadcrumbs>
    children
  </DocsLayout>
}
