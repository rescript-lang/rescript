switch x {
| A => ()
| B => ()
}

// parens optional
switch (a + b) {
| _ => ()
}

switch a + b {
| _ => ()
}

switch (a, b) {
| (Some(a), Some(b)) => a + b + c
| _ => 3
}

switch person1 {
| Teacher(_) => () // do nothing
| Student({reportCard: {gpa}}) if gpa < 0.5 =>
  Js.log("What's happening")
| Student({reportCard: {gpa}}) if gpa > 0.9 =>
  Js.log("Take more free time, you study too much.")
| Student(_) =>
  // fall-through, catch-all case
  Js.log("Heyo")
}
