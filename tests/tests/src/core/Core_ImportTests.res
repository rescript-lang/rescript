let main = async () => {
  let eq = await import(Core_IntTests.eq)
  Test.run(__POS_OF__("dynamic import - Int tests - eq"), 1, eq, 1)
}

main->ignore
