type options = {
  extra?: {
    name: string,
    superExtra?: {age: int},
    otherExtra: option<{test: bool, anotherInlined: {record: bool}}>,
  },
}

let options = {
  //   ^hov
  extra: {
    name: "test",
    superExtra: {
      age: 2222,
    },
    otherExtra: Some({test: true, anotherInlined: {record: true}}),
  },
}
