type allOptRec = {n?: int, s?: string}

let construct = b => b ? {n: 0} : {}

// let z = {}
// Error: Empty record literal {} should be type annotated or used in a record context.

type emptyrec = {}

let er: emptyrec = {}
