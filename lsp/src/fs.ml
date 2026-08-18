type fs = Eio.Fs.dir_ty Eio.Path.t

let append = Eio.Path.( / )

let to_eio_path ~(fs : fs) (path : string) = append fs path

let protect f =
  match f () with
  | value -> Some value
  | exception _ -> None

let load ~fs path = protect (fun () -> Eio.Path.load (to_eio_path ~fs path))

let save ?append ~create ~fs path content =
  Eio.Path.save ?append ~create (to_eio_path ~fs path) content

let stat ?(follow = true) ~fs path =
  protect (fun () -> Eio.Path.stat ~follow (to_eio_path ~fs path))

let kind ?(follow = true) ~fs path =
  match protect (fun () -> Eio.Path.kind ~follow (to_eio_path ~fs path)) with
  | Some `Not_found | None -> None
  | Some kind -> Some kind

let exists ?(follow = true) ~fs path =
  match kind ~follow ~fs path with
  | Some _ -> true
  | None -> false

let is_file ~fs path =
  protect (fun () -> Eio.Path.is_file (to_eio_path ~fs path))

let is_directory ~fs path =
  protect (fun () -> Eio.Path.is_directory (to_eio_path ~fs path))

let read_dir ~fs path =
  protect (fun () -> Eio.Path.read_dir (to_eio_path ~fs path))

let mkdir ~perm ~fs path =
  protect (fun () -> Eio.Path.mkdir ~perm (to_eio_path ~fs path))

let mkdirs ?exists_ok ~perm ~fs path =
  protect (fun () -> Eio.Path.mkdirs ?exists_ok ~perm (to_eio_path ~fs path))

let read_link ~fs path =
  protect (fun () -> Eio.Path.read_link (to_eio_path ~fs path))

let unlink ~fs path = protect (fun () -> Eio.Path.unlink (to_eio_path ~fs path))

let rmdir ~fs path = protect (fun () -> Eio.Path.rmdir (to_eio_path ~fs path))

let rmtree ?missing_ok ~fs path =
  protect (fun () -> Eio.Path.rmtree ?missing_ok (to_eio_path ~fs path))
