module Test1 = {
  type status = Vacations(int) | Sabbatical(int) | Sick
  type person =
    | Teacher({age: int})
    | Student({status: status})

  let person1 = Teacher({age: 12345})

  let message = switch person1 {
  | Student({status: Vacations(_) | Sick}) => "a"
  | _ => "b"
  }
}

module Test2 = {
  type status = Vacations(int) | Sabbatical(int) | Sick | Present
  type reportCard = {passing: bool, gpa: float}
  type person =
    | Teacher({name: string, age: int})
    | Student({name: string, status: status, reportCard: reportCard})

  let person2 = Teacher({name: "Jane", age: 12345})

  let message = switch person2 {
  | Teacher({name: "Mary" | "Joe"}) => `Hey, still going to the party on Saturday?`
  | Teacher({name}) =>
    // this is matched only if `name` isn't "Mary" or "Joe"
    `Hello ${name}.`
  | Student({name, reportCard: {passing: true, gpa}}) =>
    `Congrats ${name}, nice GPA of ${Js.Float.toString(gpa)} you got there!`
  | Student({reportCard: {gpa: 0.0}, status: Vacations(daysLeft) | Sabbatical(daysLeft)}) =>
    `Come back in ${Js.Int.toString(daysLeft)} days!`
  | Student({status: Sick}) => `How are you feeling?`
  | Student({name}) => `Good luck next semester ${name}!`
  }
}
