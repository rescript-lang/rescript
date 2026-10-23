/* A closed read-only field cannot be coerced to a mutable field. Promotion is
   available only while the object row is open, so this coercion cannot add
   write capability to the source type. The target annotation does not change
   that restriction. */
type t = {"x": int}
let p = (v: t) => (v :> {@set "x": int})
