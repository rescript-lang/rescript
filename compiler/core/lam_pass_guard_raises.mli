val guard_raises : Lambda.t -> Lambda.t
(** Rewrite [if a then raise e else c] into [(if a then raise e else ()); c],
    so the continuation stops being nested inside a branch - the guard clause
    idiom in the emitted JavaScript.

    This is code motion rather than normalization: it changes the shape that
    surrounding code matches on, so it cannot live in [Lambda.if_]. Matching
    inspects the terms it has built after the fact, and rewriting them as they
    are constructed leaves static raises without their catch.

    Run it late. The opportunities come from conversion and from four
    different passes ([exits], [remove_alias], [lets_dce], [deep_flatten]), so
    a traversal scheduled after all of them catches every case without having
    to know which pass produced it. *)
