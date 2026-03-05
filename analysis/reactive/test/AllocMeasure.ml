(** Zero-overhead allocation measurement.

    [Gc.minor_words ()] boxes a float (2 words per call). This module
    subtracts that overhead so callers see only their own allocations.

    Usage:
    {[
      ignore (AllocMeasure.words_since ());   (* reset *)
      (* ... code under test ... *)
      let words = AllocMeasure.words_since () in  (* read delta *)
    ]} *)

let words_since =
  let last = ref 0. in
  fun () ->
    let now = Gc.minor_words () in
    let delta = now -. !last -. 2. in
    last := now;
    int_of_float delta
