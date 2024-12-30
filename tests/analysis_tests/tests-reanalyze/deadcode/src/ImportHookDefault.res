type person = {
  name: string,
  age: int,
}

@genType.import(("./hookExample", "default")) @react.component
external make: (
  ~person: person,
  ~children: React.element,
  ~renderMe: ImportHooks.renderMe<string>,
) => React.element = "make"
