let standalone = @attr (value): (int => int) => other => value + other
consume(@attr (value): (int => int) => other => value + other)
consume(@attr (value): (int => int) => other => value + other, argument)
consume(argument, @attr (value): (int => int) => other => value + other)

let fragment = value => <>
  <First value />
  <Second value />
</>
consume(value => <>
  <First value />
  <Second value />
</>)
consume(value => <>
  <First value />
  <Second value />
</>, argument)
consume(argument, value => <>
  <First value />
  <Second value />
</>)

let block = @attr async (~first, ~second=defaultValue): result => {
  // Keep the body comment.
  await compute(first, second)
}
consume(argument, @attr async (~first, ~second=defaultValue): result => {
  // Keep the callback comment.
  await compute(first, second)
})
consume(firstVeryLongArgument, secondVeryLongArgument, (firstParameter, secondParameter) =>
  compute(firstParameter, secondParameter)
)
