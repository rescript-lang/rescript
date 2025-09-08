let str = React.string

%raw(`require("./Tooltip.css")`)

let bubbleClasses = position => {
  let positionClass = switch position {
  | #Top => "tooltip__bubble--top"
  | #Right => "tooltip__bubble--right"
  | #Bottom => "tooltip__bubble--bottom"
  | #Left => "tooltip__bubble--left"
  }

  "tooltip__bubble " ++ positionClass
}

@react.component
let make = (~tip, ~className="", ~position=#Top, ~disabled=false, ~children) =>
  disabled
    ? children
    : <div className={"tooltip " ++ className}>
        <div className="tooltip__trigger"> children </div>
        <div className={bubbleClasses(position)}>
          <div
            className="text-white text-xs p-2 text-center leading-snug rounded bg-gray-900 whitespace-no-wrap">
            tip
          </div>
        </div>
      </div>
