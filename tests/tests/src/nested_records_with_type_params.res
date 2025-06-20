type options<'age> = {
  extra?: {
    mutable extraField: {theField: bool},
    name: string,
    superExtra?: {age: 'age},
    otherExtra: option<{test: bool, anotherInlined: {record: bool}}>,
  },
}

let options = {
  extra: {
    extraField: {theField: true},
    name: "test",
    superExtra: {
      age: 2222,
    },
    otherExtra: Some({test: true, anotherInlined: {record: true}}),
  },
}

let opts2: options<string> = {
  extra: {
    extraField: {theField: true},
    name: "test",
    superExtra: {
      age: "1234",
    },
    otherExtra: Some({test: true, anotherInlined: {record: true}}),
  },
}
