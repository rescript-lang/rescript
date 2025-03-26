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
    //^hov
    superExtra: {
      age: 2222,
      //^hov
    },
    otherExtra: Some({
      test: true,
      // ^hov
      anotherInlined: {
        record: true,
        // ^hov
      },
    }),
  },
}
