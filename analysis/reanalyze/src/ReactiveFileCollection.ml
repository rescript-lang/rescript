(** Reactive File Collection - Implementation
    
    Uses CmtCache for efficient file change detection via Unix.stat. *)

type event = Added of string | Removed of string | Modified of string

type 'v t = {data: (string, 'v) Hashtbl.t; process: Cmt_format.cmt_infos -> 'v}

let create ~process = {data = Hashtbl.create 256; process}

let add t path =
  let cmt_infos = CmtCache.read_cmt path in
  let value = t.process cmt_infos in
  Hashtbl.replace t.data path value

let remove t path =
  Hashtbl.remove t.data path;
  CmtCache.invalidate path

let update t path =
  (* Re-read the file and update the cache *)
  add t path

let set t path value = Hashtbl.replace t.data path value

let apply t events =
  List.iter
    (function
      | Added path -> add t path
      | Removed path -> remove t path
      | Modified path -> update t path)
    events

let get t path = Hashtbl.find_opt t.data path

let find t path = Hashtbl.find t.data path

let mem t path = Hashtbl.mem t.data path

let length t = Hashtbl.length t.data

let is_empty t = length t = 0

let iter f t = Hashtbl.iter f t.data

let fold f t init = Hashtbl.fold f t.data init

let to_list t = fold (fun k v acc -> (k, v) :: acc) t []

let paths t = fold (fun k _ acc -> k :: acc) t []

let values t = fold (fun _ v acc -> v :: acc) t []
