module Link = Next.Link

// Structure defined by `scripts/extract-indices.js`
let indexData: dict<{
  "moduleName": string,
  "headers": array<{
    "name": string,
    "href": string,
  }>,
}> = %raw("require('../index_data/belt_api_index.json')")

// Retrieve package.json to access the version of bs-platform.
let package: Js.t<{"dependencies": {"bs-platform": string}}> = %raw("require('../package.json')")

module Sidebar = SidebarLayout.Sidebar
module UrlPath = SidebarLayout.UrlPath
module NavItem = Sidebar.NavItem
module Category = Sidebar.Category

let overviewNavs = [
  {
    open NavItem
    {name: "Introduction", href: "/apis/javascript/latest/belt"}
  },
]

let setNavs = [
  {
    open NavItem
    {name: "HashSet", href: "/apis/javascript/latest/belt/hash-set"}
  },
  {name: "HashSetInt", href: "/apis/javascript/latest/belt/hash-set-int"},
  {
    name: "HashSetString",
    href: "/apis/javascript/latest/belt/hash-set-string",
  },
  {name: "Set", href: "/apis/javascript/latest/belt/set"},
  {name: "SetDict", href: "/apis/javascript/latest/belt/set-dict"},
  {name: "SetInt", href: "/apis/javascript/latest/belt/set-int"},
  {name: "SetString", href: "/apis/javascript/latest/belt/set-string"},
]

let mapNavs = [
  {
    open NavItem
    {name: "HashMap", href: "/apis/javascript/latest/belt/hash-map"}
  },
  {name: "HashMapInt", href: "/apis/javascript/latest/belt/hash-map-int"},
  {
    name: "HashMapString",
    href: "/apis/javascript/latest/belt/hash-map-string",
  },
  {name: "Map", href: "/apis/javascript/latest/belt/map"},
  {name: "MapDict", href: "/apis/javascript/latest/belt/map-dict"},
  {name: "MapInt", href: "/apis/javascript/latest/belt/map-int"},
  {name: "MapString", href: "/apis/javascript/latest/belt/map-string"},
]

let mutableCollectionsNavs = [
  {
    open NavItem
    {
      name: "MutableMap",
      href: "/apis/javascript/latest/belt/mutable-map",
    }
  },
  {
    name: "MutableMapInt",
    href: "/apis/javascript/latest/belt/mutable-map-int",
  },
  {
    name: "MutableMapString",
    href: "/apis/javascript/latest/belt/mutable-map-string",
  },
  {name: "MutableQueue", href: "/apis/javascript/latest/belt/mutable-queue"},
  {name: "MutableSet", href: "/apis/javascript/latest/belt/mutable-set"},
  {
    name: "MutableSetInt",
    href: "/apis/javascript/latest/belt/mutable-set-int",
  },
  {
    name: "MutableSetString",
    href: "/apis/javascript/latest/belt/mutable-set-string",
  },
  {name: "MutableStack", href: "/apis/javascript/latest/belt/mutable-stack"},
]

let basicNavs = [
  {
    open NavItem
    {name: "List", href: "/apis/javascript/latest/belt/list"}
  },
  {name: "Array", href: "/apis/javascript/latest/belt/array"},
  {name: "Float", href: "/apis/javascript/latest/belt/float"},
  {name: "Int", href: "/apis/javascript/latest/belt/int"},
  {name: "Range", href: "/apis/javascript/latest/belt/range"},
  {name: "Id", href: "/apis/javascript/latest/belt/id"},
  {name: "Option", href: "/apis/javascript/latest/belt/option"},
  {name: "Result", href: "/apis/javascript/latest/belt/result"},
]

let sortNavs = [
  {
    open NavItem
    {
      name: "SortArray",
      href: "/apis/javascript/latest/belt/sort-array",
    }
  },
  {name: "SortArrayInt", href: "/apis/javascript/latest/belt/sort-array-int"},
  {
    name: "SortArrayString",
    href: "/apis/javascript/latest/belt/sort-array-string",
  },
]

let utilityNavs = [
  {
    open NavItem
    {name: "Debug", href: "/apis/javascript/latest/belt/debug"}
  },
]

let categories = [
  {
    open Category
    {name: "Overview", items: overviewNavs}
  },
  {name: "Basics", items: basicNavs},
  {name: "Set", items: setNavs},
  {name: "Map", items: mapNavs},
  {name: "Mutable Collections", items: mutableCollectionsNavs},
  {name: "Sort Collections", items: sortNavs},
  {name: "Utilities", items: utilityNavs},
]

module Docs = {
  @react.component
  let make = (~components=ApiMarkdown.default, ~children) => {
    let router = Next.Router.useRouter()
    let route = router.route

    // Gather data for the CollapsibleSection
    let headers = {
      open Belt.Option
      Js.Dict.get(indexData, route)
      ->map(data =>
        data["headers"]->Belt.Array.map(header => (header["name"], "#" ++ header["href"]))
      )
      ->getWithDefault([])
    }

    let moduleName = {
      open Belt.Option
      Js.Dict.get(indexData, route)->map(data => data["moduleName"])->getWithDefault("?")
    }

    let (isSidebarOpen, setSidebarOpen) = React.useState(_ => false)
    let toggleSidebar = () => setSidebarOpen(prev => !prev)

    let urlPath = UrlPath.parse(~base="/apis/javascript", route)

    let breadcrumbs = Belt.Option.map(urlPath, v => {
      let {UrlPath.version: version} = v
      let prefix = {
        open UrlPath
        list{
          {name: "API", href: "/apis"},
          {name: "JavaScript", href: "/apis/javascript/" ++ version},
        }
      }
      UrlPath.toBreadCrumbs(~prefix, v)
    })

    let toplevelNav = switch urlPath {
    | Some(urlPath) =>
      let version = {
        open UrlPath
        urlPath.version
      }
      let backHref = Some(UrlPath.fullUpLink(urlPath))
      <Sidebar.ToplevelNav title="Belt" version ?backHref />
    | None => React.null
    }

    // Todo: We need to introduce router state to be able to
    //       listen to anchor changes (#get, #map,...)
    let preludeSection =
      route !== "/apis/javascript/latest/belt"
        ? <Sidebar.CollapsibleSection headers moduleName />
        : React.null

    let sidebar =
      <Sidebar
        isOpen=isSidebarOpen
        toggle=toggleSidebar
        categories
        route=router.route
        toplevelNav
        preludeSection
      />

    <SidebarLayout
      theme=#Js components sidebarState=(isSidebarOpen, setSidebarOpen) sidebar ?breadcrumbs>
      children
    </SidebarLayout>
  }
}

module Prose = {
  @react.component
  let make = (~children) => <Docs components=Markdown.default> children </Docs>
}
