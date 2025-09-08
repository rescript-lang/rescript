module Link = Next.Link

// Structure defined by `scripts/extract-indices.js`
let indexData: dict<{
  "moduleName": string,
  "headers": array<{
    "name": string,
    "href": string,
  }>,
}> = %raw("require('../index_data/js_api_index.json')")

// Retrieve package.json to access the version of bs-platform.
let package: Js.t<{"dependencies": {"bs-platform": string}}> = %raw("require('../package.json')")

module Sidebar = SidebarLayout.Sidebar
module UrlPath = SidebarLayout.UrlPath
module NavItem = Sidebar.NavItem
module Category = Sidebar.Category

let overviewNavs = [
  {
    open NavItem
    {name: "Introduction", href: "/apis/javascript/latest/js"}
  },
]

let apiNavs = [
  {
    open NavItem
    {name: "Array2", href: "/apis/javascript/latest/js/array-2"}
  },
  {name: "Array", href: "/apis/javascript/latest/js/array"},
  {name: "Console", href: "/apis/javascript/latest/js/console"},
  {name: "Date", href: "/apis/javascript/latest/js/date"},
  {name: "Dict", href: "/apis/javascript/latest/js/dict"},
  {name: "Exn", href: "/apis/javascript/latest/js/exn"},
  {name: "Float", href: "/apis/javascript/latest/js/float"},
  {name: "Global", href: "/apis/javascript/latest/js/global"},
  {name: "Int", href: "/apis/javascript/latest/js/int"},
  {name: "Json", href: "/apis/javascript/latest/js/json"},
  {name: "List", href: "/apis/javascript/latest/js/list"},
  {name: "Math", href: "/apis/javascript/latest/js/math"},
  {name: "NullUndefined", href: "/apis/javascript/latest/js/null-undefined"},
  {name: "Null", href: "/apis/javascript/latest/js/null"},
  {name: "Nullable", href: "/apis/javascript/latest/js/nullable"},
  {name: "Obj", href: "/apis/javascript/latest/js/obj"},
  {name: "Option", href: "/apis/javascript/latest/js/option"},
  {name: "Promise", href: "/apis/javascript/latest/js/promise"},
  {name: "Re", href: "/apis/javascript/latest/js/re"},
  {name: "Result", href: "/apis/javascript/latest/js/result"},
  {name: "String2", href: "/apis/javascript/latest/js/string-2"},
  {name: "String", href: "/apis/javascript/latest/js/string"},
  {
    name: "TypedArrayArrayBuffer",
    href: "/apis/javascript/latest/js/typed-array_array-buffer",
  },
  {
    name: "TypedArrayDataView",
    href: "/apis/javascript/latest/js/typed-array_data-view",
  },
  {
    name: "TypedArrayFloat32Array",
    href: "/apis/javascript/latest/js/typed-array_float-32-array",
  },
  {
    name: "TypedArrayFloat64Array",
    href: "/apis/javascript/latest/js/typed-array_float-64-array",
  },
  {
    name: "TypedArrayInt8Array",
    href: "/apis/javascript/latest/js/typed-array_int-8-array",
  },
  {
    name: "TypedArrayInt16Array",
    href: "/apis/javascript/latest/js/typed-array_int-16-array",
  },
  {
    name: "TypedArrayInt32Array",
    href: "/apis/javascript/latest/js/typed-array_int-32-array",
  },
  {
    name: "TypedArrayTypeS",
    href: "/apis/javascript/latest/js/typed-array_type-s",
  },
  {
    name: "TypedArrayUint8Array",
    href: "/apis/javascript/latest/js/typed-array_uint-8-array",
  },
  {
    name: "TypedArrayUint8ClampedArray",
    href: "/apis/javascript/latest/js/typed-array_uint-8-clamped-array",
  },
  {
    name: "TypedArrayUint16Array",
    href: "/apis/javascript/latest/js/typed-array_uint-16-array",
  },
  {
    name: "TypedArrayUint32Array",
    href: "/apis/javascript/latest/js/typed-array_uint-32-array",
  },
  {
    name: "TypedArray2ArrayBuffer",
    href: "/apis/javascript/latest/js/typed-array-2_array-buffer",
  },
  {
    name: "TypedArray2DataView",
    href: "/apis/javascript/latest/js/typed-array-2_data-view",
  },
  {
    name: "TypedArray2Float32Array",
    href: "/apis/javascript/latest/js/typed-array-2_float-32-array",
  },
  {
    name: "TypedArray2Float64Array",
    href: "/apis/javascript/latest/js/typed-array-2_float-64-array",
  },
  {
    name: "TypedArray2Int8Array",
    href: "/apis/javascript/latest/js/typed-array-2_int-8-array",
  },
  {
    name: "TypedArray2Int16Array",
    href: "/apis/javascript/latest/js/typed-array-2_int-16-array",
  },
  {
    name: "TypedArray2Int32Array",
    href: "/apis/javascript/latest/js/typed-array-2_int-32-array",
  },
  {
    name: "TypedArray2Uint8Array",
    href: "/apis/javascript/latest/js/typed-array-2_uint-8-array",
  },
  {
    name: "TypedArray2Uint8ClampedArray",
    href: "/apis/javascript/latest/js/typed-array-2_uint-8-clamped-array",
  },
  {
    name: "TypedArray2Uint16Array",
    href: "/apis/javascript/latest/js/typed-array-2_uint-16-array",
  },
  {
    name: "TypedArray2Uint32Array",
    href: "/apis/javascript/latest/js/typed-array-2_uint-32-array",
  },
  {name: "TypedArray2", href: "/apis/javascript/latest/js/typed-array-2"},
  {name: "TypedArray", href: "/apis/javascript/latest/js/typed-array"},
  {name: "Types", href: "/apis/javascript/latest/js/types"},
  {name: "Undefined", href: "/apis/javascript/latest/js/undefined"},
  {name: "Vector", href: "/apis/javascript/latest/js/vector"},
]

let categories = [
  {
    open Category
    {name: "Overview", items: overviewNavs}
  },
  {name: "API", items: apiNavs},
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
      <Sidebar.ToplevelNav title="Js Module" version ?backHref />
    | None => React.null
    }

    // Todo: We need to introduce router state to be able to
    //       listen to anchor changes (#get, #map,...)
    let preludeSection =
      route !== "/apis/javascript/latest/js"
        ? <Sidebar.CollapsibleSection headers moduleName />
        : React.null

    let sidebar =
      <Sidebar
        isOpen=isSidebarOpen
        toggle=toggleSidebar
        categories
        route=router.route
        preludeSection
        toplevelNav
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
