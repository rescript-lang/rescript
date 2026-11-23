type fs = Eio.Fs.dir_ty Eio.Path.t

let append = Eio.Path.( / )

let to_eio_path ~(fs : fs) (path : string) = append fs path

let protect f =
  match f () with
  | value -> Some value
  | exception _ -> None

let load ~fs path = protect (fun () -> Eio.Path.load (to_eio_path ~fs path))

let save ~append ~create ~fs path content =
  Eio.Path.save ~append ~create (to_eio_path ~fs path) content

let kind ~follow ~fs path =
  match protect (fun () -> Eio.Path.kind ~follow (to_eio_path ~fs path)) with
  | Some `Not_found | None -> None
  | Some kind -> Some kind

let exists ~follow ~fs path =
  match kind ~follow ~fs path with
  | Some _ -> true
  | None -> false
