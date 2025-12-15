(* Reactive File Collection - Implementation *)

type event =
  | Added of string
  | Removed of string
  | Modified of string

(* We need to use Obj.t to make the polymorphic process function work
   with Marshal_cache which returns ['a]. This is safe because the user
   guarantees the file contains data of the expected type. *)
type 'v process_fn = Obj.t -> 'v

type 'v t = {
  data : (string, 'v) Hashtbl.t;
  process : 'v process_fn;
}

let create (type a v) ~(process : a -> v) : v t =
  let process_fn : v process_fn = fun obj -> process (Obj.obj obj) in
  {
    data = Hashtbl.create 256;
    process = process_fn;
  }

let add t path =
  let value = Marshal_cache.with_unmarshalled_file path (fun data ->
    t.process (Obj.repr data)
  ) in
  Hashtbl.replace t.data path value
  [@@alert "-unsafe"]

let remove t path =
  Hashtbl.remove t.data path

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
  Hashtbl.find_opt t.data path

let find t path =
  Hashtbl.find t.data path

let mem t path =
  Hashtbl.mem t.data path

let length t =
  Hashtbl.length t.data

let is_empty t =
  length t = 0

let iter f t =
  Hashtbl.iter f t.data

let fold f t init =
  Hashtbl.fold f t.data init

let to_list t =
  fold (fun k v acc -> (k, v) :: acc) t []

let paths t =
  fold (fun k _ acc -> k :: acc) t []

let values t =
  fold (fun _ v acc -> v :: acc) t []




