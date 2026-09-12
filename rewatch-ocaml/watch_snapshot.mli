(** Native filesystem events are treated as wakeups rather than authoritative
    edits. Snapshots provide the content baseline needed to coalesce duplicate
    events and to detect changes that arrive while a build is running. *)

type change_kind = Added | Removed | Modified
type change = {path: string; kind: change_kind}

type file = {modified: float; size: int; digest: string}
type dependency = {modified: float; size: int}

(** Dependency candidates need states distinct from ordinary files: a missing
    candidate is meaningful and must remain observable, whereas [Unreadable]
    represents a filesystem failure that must not be treated as deletion. *)
type state =
  | File of file
  | Dependency_candidate of dependency
  | Missing_dependency_candidate
  | Unreadable

type entry = {path: string; state: state}

type registration =
  | Ready of {
      snapshot: entry list;
      paths: Native_watcher.watch_path list;
      targets: string list;
    }
  | Registration_failed of {snapshot: entry list; message: string}

val equal : entry list -> entry list -> bool

val create :
  ?on_source_symlink:(string -> unit) ->
  (string, float * float * int * string) Hashtbl.t ->
  Watch_scope.t ->
  entry list

val create_with_symlink_paths :
  (string, float * float * int * string) Hashtbl.t ->
  Watch_scope.t ->
  registration

val changes_between : entry list -> entry list -> change list

val update_entries :
  (string, float * float * int * string) Hashtbl.t ->
  entry list ->
  change list ->
  entry list option

val polling_build_changes :
  previous:entry list ->
  trigger:entry list ->
  before_build:entry list ->
  change list

val changes_are_incremental : change list -> bool

val reconciliation_baseline :
  old_scope:Watch_scope.t ->
  new_scope:Watch_scope.t ->
  entry list ->
  entry list ->
  entry list
