type t = {
  require_emitter_early: string list;
  export_emitter_early: string list;
  require_emitter: string list;
  import_emitter: string list;
  export_emitter: string list;
}

let initial =
  {
    require_emitter_early = [];
    export_emitter_early = [];
    require_emitter = [];
    import_emitter = [];
    export_emitter = [];
  }

let string ~emitter s = s :: emitter

let require_early ~emitters s =
  {
    emitters with
    require_emitter_early = s |> string ~emitter:emitters.require_emitter_early;
  }

let export_early ~emitters s =
  {
    emitters with
    export_emitter_early = s |> string ~emitter:emitters.export_emitter_early;
  }

let require ~emitters s =
  {
    emitters with
    require_emitter = s |> string ~emitter:emitters.require_emitter;
  }

let import ~emitters s =
  {emitters with import_emitter = s |> string ~emitter:emitters.import_emitter}

let export ~emitters s =
  {emitters with export_emitter = s |> string ~emitter:emitters.export_emitter}

let to_string ~separator emitters =
  [
    emitters.require_emitter_early |> List.rev;
    emitters.export_emitter_early |> List.rev;
    emitters.require_emitter |> List.rev;
    emitters.import_emitter |> List.rev;
    emitters.export_emitter |> List.rev;
  ]
  |> List.concat |> String.concat separator
