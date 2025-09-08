module Link = Next.Link

// Structure defined by `scripts/extract-tocs.js`
let tocData: dict<{
  "title": string,
  "headers": array<{
    "name": string,
    "href": string,
  }>,
}> = %raw("require('../index_data/community_toc.json')")

module UrlPath = DocsLayout.UrlPath
module NavItem = DocsLayout.NavItem
module Category = DocsLayout.Category
module Toc = DocsLayout.Toc

let overviewNavs = [
  {
    open NavItem
    {name: "Overview", href: "/community"}
  },
  {name: "Code of Conduct", href: "/community/code-of-conduct"},
  {name: "Events & Meetups", href: "/community/events"},
  {name: "Articles & Videos", href: "/community/articles-and-videos"},
  {name: "Get involved", href: "/community/get-involved"},
]

let categories = [
  {
    open Category
    {name: "Resources", items: overviewNavs}
  },
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

  // Todo: improve this bit (UrlPath should go away at some point)
  let breadcrumbs = if route === "/community" {
    Some({
      open UrlPath
      list{{name: "Community", href: "/community"}, {name: "Overview", href: ""}}
    })
  } else {
    Belt.Option.map(urlPath, v => UrlPath.toBreadCrumbs(~prefix=list{}, v))
  }

  Js.log(breadcrumbs)

  let title = "Community"

  <DocsLayout theme=#Reason components categories title ?activeToc ?breadcrumbs>
    children
  </DocsLayout>
}
