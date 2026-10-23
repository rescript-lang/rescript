/* Coercing a settable open source to a closed immutable target forgets
   write capability: the coercion result is read-only. (The enlarged
   approximation used by the coercion fast path has its own mutability
   cells, so checking against it cannot promote the declared target.) */
let writeAfterForgetting = (o: {..@set "x": int}) => {
  let forgotten = (o :> {"x": int})
  forgotten["x"] = 2
}
