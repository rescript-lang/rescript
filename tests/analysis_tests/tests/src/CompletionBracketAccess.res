type person = {name: string, age: int}

let people: array<person> = []

// Pipe completion after array index access - resolves to option<person>
// people[0]->
//            ^com

// Inlay hint for let binding of array index access - resolves to option<person>
let firstPerson = people[0]

//^hin
