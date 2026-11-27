@@jsxConfig({version: 4, module_: "Jsx"})

type props<'a> = {value: 'a}

@react.componentWithProps
let make = (props: props<int>) => props.value
