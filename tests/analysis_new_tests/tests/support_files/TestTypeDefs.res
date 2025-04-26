type nestedTestRecord = {
  test: bool,
  nested: {
    name: string,
    oneMoreLevel: {here: bool},
  },
}

let nestedTestRecord = {
  test: true,
  nested: {
    name: "test",
    oneMoreLevel: {
      here: true,
    },
  },
}
