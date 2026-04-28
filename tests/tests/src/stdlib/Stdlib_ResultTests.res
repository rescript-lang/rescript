let eq = (a, b) => a == b

// =======
// forEach
// =======

let forEachIfOkCallFunction = () => {
  let called = ref([])
  Ok(3)->Result.forEach(i => called.contents->Array.push(i))
  Test.run(__POS_OF__("forEach: if ok, call function with ok value once"), called.contents, eq, [3])
}
forEachIfOkCallFunction()

let forEachIfErrorDoNotCallFunction = () => {
  let called = ref([])
  Error(3)->Result.forEach(i => called.contents->Array.push(i))
  Test.run(__POS_OF__("forEach: if error, do not call function"), called.contents, eq, [])
}
forEachIfErrorDoNotCallFunction()

// ========
// mapError
// ========

Test.run(__POS_OF__("mapError: if ok, return it"), Ok(5)->Result.mapError(i => i * 3), eq, Ok(5))

Test.run(
  __POS_OF__("mapError: if error, apply f"),
  Error(5)->Result.mapError(i => i * 3),
  eq,
  Error(15),
)

Test.run(__POS_OF__("all"), Result.all([]), eq, Ok([]))
Test.run(__POS_OF__("all"), Result.all([Ok(1), Ok(2), Ok(3)]), eq, Ok([1, 2, 3]))
Test.run(__POS_OF__("all"), Result.all([Ok(1), Error(2)]), eq, Error(2))
Test.run(__POS_OF__("all"), Result.all2((Ok(1), Ok(2))), eq, Ok((1, 2)))
Test.run(__POS_OF__("all"), Result.all2((Ok(1), Error(2))), eq, Error(2))
Test.run(__POS_OF__("all"), Result.all3((Ok(1), Ok(2), Ok(3))), eq, Ok((1, 2, 3)))
Test.run(__POS_OF__("all"), Result.all3((Ok(1), Error(2), Ok(3))), eq, Error(2))
Test.run(__POS_OF__("all"), Result.all4((Ok(1), Ok(2), Ok(3), Ok(4))), eq, Ok((1, 2, 3, 4)))
Test.run(__POS_OF__("all"), Result.all4((Ok(1), Error(2), Ok(3), Ok(4))), eq, Error(2))
