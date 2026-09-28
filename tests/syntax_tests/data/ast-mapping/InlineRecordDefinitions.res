type person = {details: {name: string, age: int}}

external find: {id: string} => {person: person} = "find"
