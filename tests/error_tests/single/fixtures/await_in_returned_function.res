let a = async () => 3

let foo = async x => y => (await a()) + x + y
