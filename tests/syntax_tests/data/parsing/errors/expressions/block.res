let findThreadByIdLinearScan = (~threads, ~id) => {
  Array.findWithIndex(ThreadsModel.threads, (thread, i) => {
    let thisId = switch (thread) {
    | ServerData.OneToOne({otherPersonIDWhichIsAlsoThreadID}) =>
      otherPersonIDWhichIsAlsoThreadID
    | Group({id}) =>
      id
    | Unknown({id}) =>
      // TODO: this is stupidly dangerous
      unknown.id->String.make->FBID.ofStringUnsafe
    }

    thisId === id
  }
}

let x = {
  loop(0, Nil->push(doc)
} // closing ) above is missing

switch stack {
| Empty =>
| Cons(doc, rest) => ()
| Join(doc1, doc2) =>
	buffer->Buffer.add_string(indentation)
	loop(
}

let pipeline = switch scheduler {
| Some =>
| None => ()
}
