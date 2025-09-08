let handleClick = (href, event) =>
  if !ReactEvent.Mouse.defaultPrevented(event) {
    ReactEvent.Mouse.preventDefault(event)
    ReasonReact.Router.push(href)
  }

@react.component
let make = (~href, ~className="", ~children) =>
  <a href className onClick={event => handleClick(href, event)}> children </a>

<Animated> ...{x => <div />} </Animated>

<div> ...element </div>
<div> ...{a => 1} </div>
<div> ...<span /> </div>
<div> ...[a, b] </div>
<div> ...{(1, 2)} </div>
