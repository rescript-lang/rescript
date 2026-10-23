type person = {details: {name: string}}

let getName = (person: person) => person.details.name
//                                       ^hov
