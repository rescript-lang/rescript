let eq = (a, b) => a == b

Test.run(__POS_OF__("includes - present"), list{1, 2, 3}->List.includes(2), eq, true)
Test.run(__POS_OF__("includes - missing"), list{1, 2, 3}->List.includes(4), eq, false)
Test.run(__POS_OF__("includes - empty"), list{}->List.includes(1), eq, false)

{
  let item = {"language": "ReScript"}
  let items = list{item}

  Test.run(__POS_OF__("includes - same object"), items->List.includes(item), eq, true)
  Test.run(
    __POS_OF__("includes - structurally equal object"),
    items->List.includes({"language": "ReScript"}),
    eq,
    false,
  )
}
