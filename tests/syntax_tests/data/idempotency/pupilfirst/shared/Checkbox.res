%raw(`require("./Checkbox.css")`)

let str = React.string

@react.component
let make = (~id, ~label, ~onChange, ~checked=false) =>
  <div>
    <input className="hidden checkbox__input" id type_="checkbox" onChange checked />
    <label className="checkbox__label flex items-center" htmlFor=id>
      <span>
        <svg width="11px" height="11px" viewBox="0 0 13 13">
          <polyline points="1.5 6 4.5 9 10.5 1" />
        </svg>
      </span>
      <span className="text-sm flex-1 font-semibold leading-loose"> {label->str} </span>
    </label>
  </div>
