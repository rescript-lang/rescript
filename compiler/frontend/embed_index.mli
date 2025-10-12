val write_structure_index :
  outprefix:string -> sourcefile:string -> Parsetree.structure -> unit
(** When Js_config.collect_embeds is enabled, scan [structure] for supported
    embed extensions and write an index JSON next to [outprefix]^".ast".
    No-op when flag is disabled. *)

