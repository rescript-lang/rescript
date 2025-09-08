let str = React.string

open StudentTopNav__Types

let handleToggle = (setLinksVisible, _) => setLinksVisible(linksVisible => !linksVisible)

let additionalLinks = (linksVisible, links) =>
  linksVisible
    ? <div
        className="dropdown__list dropdown__list-right bg-white shadow-lg rounded mt-3 border absolute w-40 z-50">
        {links
        ->List.mapi((index, link) =>
          <div key={index->string_of_int} className="">
            <a
              className="cursor-pointer block p-3 text-xs font-semibold text-gray-900 border-b border-gray-200 bg-white hover:text-primary-500 hover:bg-gray-200"
              href={link->NavLink.url}>
              {link->NavLink.title->str}
            </a>
          </div>
        )
        ->Array.of_list
        ->ReasonReact.array}
      </div>
    : ReasonReact.null

@react.component
let make = (~links) => {
  let (linksVisible, setLinksVisible) = React.useState(() => false)
  switch links {
  | list{} => ReasonReact.null
  | moreLinks =>
    <div
      title="Show more links"
      className="ml-6 font-semibold text-sm cursor-pointer relative text-black"
      onClick={handleToggle(setLinksVisible)}
      key="more-links">
      <span> {"More"->str} </span>
      <i className="fas fa-caret-down ml-1" />
      {additionalLinks(linksVisible, moreLinks)}
    </div>
  }
}
