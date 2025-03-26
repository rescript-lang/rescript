//
type options = {
  extra: {
    name: string,
    superExtra: {age: int},
  },
}

let options = {
  extra: {
    name: "test",
    superExtra: {
      age: 2222,
    },
  },
}

// options
//   ^hov

// options.extra
//           ^hov

// options.extra.superExtra
//                        ^hov

// options.extra.superExtra.age
//                           ^hov
