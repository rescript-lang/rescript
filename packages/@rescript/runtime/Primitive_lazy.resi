exception Undefined

/** [force x] forces the suspension [x] and returns its result.
   If [x] has already been forced, [Lazy.force x] returns the
   same value again without recomputing it.  If it raised an exception,
   the same exception is raised again.
   Raise {!Undefined} if the forcing of [x] tries to force [x] itself
   recursively.
*/
let force: lazy_t<'a> => 'a

/** [force_val x] forces the suspension [x] and returns its
    result.  If [x] has already been forced, [force_val x]
    returns the same value again without recomputing it.
    Raise {!Undefined} if the forcing of [x] tries to force [x] itself
    recursively.
    If the computation of [x] raises an exception, it is unspecified
    whether [force_val x] raises the same exception or {!Undefined}.
*/
let force_val: lazy_t<'a> => 'a

/** [from_fun f] is the same as [lazy (f ())] but slightly more efficient.

    [from_fun] should only be used if the function [f] is already defined.
    In particular it is always less efficient to write
    [from_fun (fun () => expr)] than [lazy expr].
*/
let from_fun: (unit => 'a) => lazy_t<'a>

/** [from_val v] returns an already-forced suspension of [v].
    This is for special purposes only and should not be confused with
    [lazy (v)].
*/
let from_val: 'a => lazy_t<'a>

/** [is_val x] returns [true] if [x] has already been forced and
    did not raise an exception.
*/
let is_val: lazy_t<'a> => bool
