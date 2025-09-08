@@config({
  flags: [
    "-bs-jsx",
    "4", // "-dsource",
    // "-w","A",
    // "-warn-error", "a"
  ],
})

@react.component
let rec make = (~foo, ()) => React.createElement(make, {foo: foo})
