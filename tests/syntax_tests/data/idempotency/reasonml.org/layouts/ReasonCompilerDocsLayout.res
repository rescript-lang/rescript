module Link = Next.Link

// Structure defined by `scripts/extract-tocs.js`
let tocData: dict<{
  "title": string,
  "headers": array<{
    "name": string,
    "href": string,
  }>,
}> = %raw("require('../index_data/reason_compiler_toc.json')")

module UrlPath = DocsLayout.UrlPath
module NavItem = DocsLayout.NavItem
module Category = DocsLayout.Category
module Toc = DocsLayout.Toc

let overviewNavs = [
  {
    open NavItem
    {
      name: "Introduction",
      href: "/docs/reason-compiler/latest/introduction",
    }
  },
  {name: "Installation", href: "/docs/reason-compiler/latest/installation"},
  {name: "New Project", href: "/docs/reason-compiler/latest/new-project"},
  {name: "Try", href: "/docs/reason-compiler/latest/try"},
  {
    name: "Concepts Overview",
    href: "/docs/reason-compiler/latest/concepts-overview",
  },
  {
    name: "Upgrade Guide to v7",
    href: "/docs/reason-compiler/latest/upgrade-to-v7",
  },
]

let interopNavs = [
  {
    open NavItem
    {
      name: "Overview",
      href: "/docs/reason-compiler/latest/interop-overview",
    }
  },
  {
    name: "Cheatsheet",
    href: "/docs/reason-compiler/latest/interop-cheatsheet",
  },
  {
    name: "Embed Raw JavaScript",
    href: "/docs/reason-compiler/latest/embed-raw-javascript",
  },
  {
    name: "Common Data Types",
    href: "/docs/reason-compiler/latest/common-data-types",
  },
  {
    name: "Intro to External",
    href: "/docs/reason-compiler/latest/intro-to-external",
  },
  {
    name: "Bind to Global Values",
    href: "/docs/reason-compiler/latest/bind-to-global-values",
  },
  {
    name: "Null, Undefined & Option",
    href: "/docs/reason-compiler/latest/null-undefined-option",
  },
  {name: "Object", href: "/docs/reason-compiler/latest/object"},
  {name: "Object 2", href: "/docs/reason-compiler/latest/object-2"},
  {name: "Class", href: "/docs/reason-compiler/latest/class"},
  {name: "Function", href: "/docs/reason-compiler/latest/function"},
  {
    name: "Property access",
    href: "/docs/reason-compiler/latest/property-access",
  },
  {
    name: "Return Value Wrapping",
    href: "/docs/reason-compiler/latest/return-value-wrapping",
  },
  {
    name: "Import & Export",
    href: "/docs/reason-compiler/latest/import-export",
  },
  {
    name: "Regular Expression",
    href: "/docs/reason-compiler/latest/regular-expression",
  },
  {name: "Exceptions", href: "/docs/reason-compiler/latest/exceptions"},
  {name: "JSON", href: "/docs/reason-compiler/latest/json"},
  {name: "Pipe First", href: "/docs/reason-compiler/latest/pipe-first"},
  {
    name: "Generate Converters & Helpers",
    href: "/docs/reason-compiler/latest/generate-converters-accessors",
  },
  {
    name: "Better Data Structures Printing (Debug Mode)",
    href: "/docs/reason-compiler/latest/better-data-structures-printing-debug-mode",
  },
  {
    name: "NodeJS Special Variables",
    href: "/docs/reason-compiler/latest/nodejs-special-variables",
  },
  {name: "Miscellaneous", href: "/docs/reason-compiler/latest/interop-misc"},
  {
    name: "Browser Support & Polyfills",
    href: "/docs/reason-compiler/latest/browser-support-polyfills",
  },
  {name: "Decorators", href: "/docs/reason-compiler/latest/decorators"},
]

let buildsystemNavs = [
  {
    open NavItem
    {
      name: "Overview",
      href: "/docs/reason-compiler/latest/build-overview",
    }
  },
  {
    name: "Configuration",
    href: "/docs/reason-compiler/latest/build-configuration",
  },
  {
    name: "Automatic Interface Generation",
    href: "/docs/reason-compiler/latest/automatic-interface-generation",
  },
  {
    name: "Interop with Other Build System",
    href: "/docs/reason-compiler/latest/interop-with-js-build-systems",
  },
  {
    name: "Performance",
    href: "/docs/reason-compiler/latest/build-performance",
  },
  {name: "Advanced", href: "/docs/reason-compiler/latest/build-advanced"},
]

let stdlibNavs = [
  {
    open NavItem
    {
      name: "Overview",
      href: "/docs/reason-compiler/latest/stdlib-overview",
    }
  },
]

let advancedNavs = [
  {
    open NavItem
    {
      name: "Conditional Compilation",
      href: "/docs/reason-compiler/latest/conditional-compilation",
    }
  },
  {
    name: "Extended Compiler Options",
    href: "/docs/reason-compiler/latest/extended-compiler-options",
  },
  {
    name: "Use Existing OCaml Libraries",
    href: "/docs/reason-compiler/latest/use-existing-ocaml-libraries",
  },
  {
    name: "Difference from Native OCaml",
    href: "/docs/reason-compiler/latest/difference-from-native-ocaml",
  },
  {
    name: "Compiler Architecture & Principles",
    href: "/docs/reason-compiler/latest/compiler-architecture-principles",
  },
  {
    name: "Comparison to Js_of_ocaml",
    href: "/docs/reason-compiler/latest/comparison-to-jsoo",
  },
]

let categories = [
  {
    open Category
    {name: "Overview", items: overviewNavs}
  },
  {name: "Interop", items: interopNavs},
  {name: "Build System", items: buildsystemNavs},
  {name: "Standard Library", items: stdlibNavs},
  {name: "Advanced", items: advancedNavs},
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

  let urlPath = UrlPath.parse(~base="/docs/reason-compiler", route)

  let breadcrumbs = Belt.Option.map(urlPath, v => {
    let {UrlPath.version: version} = v
    let prefix = {
      open UrlPath
      list{
        {name: "Docs", href: "/docs"},
        {
          name: "BuckleScript",
          href: "/docs/reason-compiler/" ++ (version ++ "/introduction"),
        },
      }
    }
    UrlPath.toBreadCrumbs(~prefix, v)
  })

  let title = "BuckleScript"
  let version = "v7"

  <DocsLayout theme=#Js components categories version title ?activeToc ?breadcrumbs>
    children
  </DocsLayout>
}
