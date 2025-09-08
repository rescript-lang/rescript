@@jsxConfig({version: 4})

module User = {
  type t = {firstName: string, lastName: string}

  let format = user => "Dr." ++ user.lastName

  @react.component
  let make = (~doctor) => {
    <h1 id="h1"> {React.string(format(doctor))} </h1>
  }
}
