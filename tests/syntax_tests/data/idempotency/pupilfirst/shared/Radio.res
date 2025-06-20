%raw(`require("./Radio.css")`)

let str = React.string

@react.component
let make = (~id, ~label, ~onChange, ~checked=false) =>
  <div>
    <input className="hidden radio-input" id type_="radio" onChange checked />
    <label className="radio-label flex items-center" htmlFor=id>
      <span>
        <svg width="14px" height="14px" viewBox="0 0 14 14">
          <circle cx="8" cy="8" r="3" fill="white" />
        </svg>
      </span>
      <span className="text-sm flex-1 font-semibold leading-loose"> {label->str} </span>
    </label>
  </div>
