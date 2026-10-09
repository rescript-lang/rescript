(** Shared test helpers for Reactive tests *)

open Reactive

(** {1 Compatibility helpers} *)

(* subscribe takes collection first in V2, but we want handler first for compatibility *)
let subscribe handler t = t.subscribe handler

(* emit_batch: emit a batch delta to a source *)
let emit_batch entries emit_fn = emit_fn (Batch entries)

(** {1 Common set modules} *)

module Int_set = Set.Make (Int)
module String_map = Map.Make (String)
