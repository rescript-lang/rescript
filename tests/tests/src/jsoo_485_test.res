let f = () => ref(None) := Some(3)

/* Uncaught ReferenceError: Invalid left-hand side in assignment */
/*
function f() {
  /* None */0 = /* Some */[3];
  return /* () */0;
}*/

let () = f()
