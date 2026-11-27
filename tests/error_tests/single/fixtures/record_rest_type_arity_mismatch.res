type source<'a> = {id: string, value: 'a}
type rest<'a> = {value: 'a}
let {id: _, ...rest<int, string> as value} = ({id: "x", value: 1}: source<int>)
