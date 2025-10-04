@@jsxConfig({version: 4})

type props<'a> = {value: 'a}

@react.componentWithProps
let make = (props: props<int>) => props.value
