module Firebase = {
  module Firestore = {
    type firestore

    type documentReference<'documentdata> = {
      id: string,
      path: string,
    }

    type documentSnapshot<'documentdata> = {
      id: string,
      ref: documentReference<'documentdata>,
    }

    @module("firebase/firestore") @variadic
    external doc: (firestore, string, array<string>) => documentReference<'documentdata> = "doc"

    @module("firebase/firestore")
    external getDoc: documentReference<'documentdata> => Promise.t<
      documentSnapshot<'documentdata>,
    > = "getDoc"
  }
}

module Sample = {
  open Firebase

  external store: Firestore.firestore = "store"

  let ref = store->Firestore.doc("some_id", [])
  // ref.
  //     ^com
}
