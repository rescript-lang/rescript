type t<'a>

type value<'a> = {
  done: bool,
  value: option<'a>,
}

let value = v => {
  done: false,
  value: Some(v),
}

let done = (~finalValue=?) => {
  done: true,
  value: finalValue,
}

@send external next: t<'a> => promise<value<'a>> = "next"

let forEach = async (iterator, f) => {
  let iteratorDone = ref(false)

  while !iteratorDone.contents {
    let {done, value} = await iterator->next
    f(value)
    iteratorDone := done
  }
}

let make: (unit => promise<value<'value>>) => t<'value> = %raw(`function makeAsyncIterator(next) {
  return {
    next,
    [Symbol.asyncIterator]() {
      return this;
    }
  }
}`)

external ignore: t<'a> => unit = "%ignore"
