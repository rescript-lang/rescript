(* Reactive File Collection - Implementation *)

type event =
  | Added of string
  | Removed of string
  | Modified of string

type 'v t = {
  data : (string, 'v) Hashtbl.t;
  process : 'a. 'a -> 'v;
}

(* We need to use Obj.magic to make the polymorphic process function work
   with Marshal_cache which returns 'a. This is safe because the user
   guarantees the file contains data of the expected type. *)
type 'v process_fn = Obj.t -> 'v

type 'v t_internal = {
  data_internal : (string, 'v) Hashtbl.t;
  process_internal : 'v process_fn;
}

let create (type a v) ~(process : a -> v) : v t =
  let process_internal : v process_fn = fun obj -> process (Obj.obj obj) in
  let t = {
    data_internal = Hashtbl.create 256;
    process_internal;
  } in
  (* Safe cast - same representation *)
  Obj.magic t

let to_internal (t : 'v t) : 'v t_internal = Obj.magic t

let add t path =
  let t = to_internal t in
  let value = Marshal_cache.with_unmarshalled_file path (fun data ->
    t.process_internal (Obj.repr data)
  ) in
  Hashtbl.replace t.data_internal path value
  [@@alert "-unsafe"]

let remove t path =
  let t = to_internal t in
  Hashtbl.remove t.data_internal path

let update t path =
  (* Just reload - Marshal_cache handles the file reading efficiently *)
  add t path

let apply t events =
  List.iter (function
    | Added path -> add t path
    | Removed path -> remove t path
    | Modified path -> update t path
  ) events

let get t path =
  let t = to_internal t in
  Hashtbl.find_opt t.data_internal path

let find t path =
  let t = to_internal t in
  Hashtbl.find t.data_internal path

let mem t path =
  let t = to_internal t in
  Hashtbl.mem t.data_internal path

let length t =
  let t = to_internal t in
  Hashtbl.length t.data_internal

let is_empty t =
  length t = 0

let iter f t =
  let t = to_internal t in
  Hashtbl.iter f t.data_internal

let fold f t init =
  let t = to_internal t in
  Hashtbl.fold f t.data_internal init

let to_list t =
  fold (fun k v acc -> (k, v) :: acc) t []

let paths t =
  fold (fun k _ acc -> k :: acc) t []

let values t =
  fold (fun _ v acc -> v :: acc) t []




