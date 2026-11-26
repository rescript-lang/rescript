// Since the React transform isn't active in the tests, mimic what the transform outputs.
module Component = {
  @res.jsxComponentProps
  type props<'name> = {name: 'name}

  let make = (): props<'name> => {children: ""}
}
