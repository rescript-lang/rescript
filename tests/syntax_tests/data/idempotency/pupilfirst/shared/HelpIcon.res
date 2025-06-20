let str = React.string

%raw(`require("./HelpIcon.css")`)

let onWindowClick = (helpVisible, setHelpVisible, _event) =>
  if helpVisible {
    setHelpVisible(_ => false)
  } else {
    ()
  }

let toggleHelp = (setHelpVisible, _event) => setHelpVisible(helpVisible => !helpVisible)

type rec responsiveAlignment =
  | NonResponsive(alignment)
  | Responsive(alignment, alignment)
and alignment =
  | AlignLeft
  | AlignRight
  | AlignCenter

let alignmentClass = alignment =>
  switch alignment {
  | AlignLeft => " left-0"
  | AlignRight => " right-0"
  | AlignCenter => " help-icon__help-container--center"
  }

let responsiveAlignmentClass = responsiveAlignment =>
  switch responsiveAlignment {
  | NonResponsive(alignment) => alignmentClass(alignment)
  | Responsive(mobileAlignment, desktopAlignment) =>
    let mobileClass = mobileAlignment->alignmentClass

    let desktopClass = switch desktopAlignment {
    | AlignLeft => " md:right-auto md:left-0"
    | AlignRight => " md:left-auto md:right-0"
    | AlignCenter => " help-icon__help-container--md-center"
    }

    mobileClass ++ (" " ++ desktopClass)
  }

@react.component
let make = (~className="", ~link=?, ~responsiveAlignment=NonResponsive(AlignCenter), ~children) => {
  let (helpVisible, setHelpVisible) = React.useState(() => false)

  React.useEffect1(() => {
    let curriedFunction = onWindowClick(helpVisible, setHelpVisible)
    let window = Webapi.Dom.window

    let removeEventListener = () =>
      Webapi.Dom.Window.removeEventListener("click", curriedFunction, window)

    if helpVisible {
      Webapi.Dom.Window.addEventListener("click", curriedFunction, window)
      Some(removeEventListener)
    } else {
      removeEventListener()
      None
    }
  }, [helpVisible])

  <div className={"inline-block relative " ++ className} onClick={toggleHelp(setHelpVisible)}>
    <FaIcon classes="fas fa-question-circle hover:text-gray-700 cursor-pointer" />
    {helpVisible
      ? <div
          onClick={event => event->ReactEvent.Mouse.stopPropagation}
          className={"help-icon__help-container overflow-y-auto mt-1 border border-gray-900 absolute z-50 px-4 py-3 shadow-lg leading-snug rounded-lg bg-gray-900 text-white max-w-xs text-center" ++
          (responsiveAlignment->responsiveAlignmentClass)}>
          children
          {link
          ->OptionUtils.map(link =>
            <a href=link target="_blank" className="block mt-1 text-blue-300 hover:text-blue:200">
              <FaIcon classes="fas fa-external-link-square-alt" />
              <span className="ml-1"> {"Read more"->str} </span>
            </a>
          )
          ->OptionUtils.default(React.null)}
        </div>
      : React.null}
  </div>
}
