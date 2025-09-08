@genType
type withRenaming = [@genType.as("type") #type_ | #b]

@genType
let testWithRenaming = (x: withRenaming) => x

@genType
type withoutRenaming = [#type_ | #b]

@genType
let testWithoutRenaming = (x: withoutRenaming) => x
