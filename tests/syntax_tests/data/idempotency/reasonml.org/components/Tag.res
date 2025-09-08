open Util.ReactStuff

type kind = [#Subtle]

@react.component
let make = (~text, ~kind=#Subtle) => {
  let className = switch kind {
  | #Subtle => "px-1 bg-snow-dark text-night-60 font-semibold rounded text-sm"
  }
  <div> <span className> {text->s} </span> </div>
}
