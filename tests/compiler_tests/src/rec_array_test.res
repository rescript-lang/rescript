type rec student = {taughtBy: teacher}
and teacher = {students: array<student>}

let rec vicky = {taughtBy: teacher}
and teacher = {students: [vicky]}
