(** Configuration for dead code elimination analysis.
    
    This module encapsulates all configuration needed for DCE,
    gathered from RunConfig and CLI flags. *)

type cli_config = {
  debug: bool;
  ci: bool;
  json: bool;
  write: bool;
  live_names: string list;
  live_paths: string list;
  exclude_paths: string list;
}

type t = {run: RunConfig.t; cli: cli_config}

(** Capture the current DCE configuration from global state.
    
    This reads from [RunConfig.runConfig] and [Common.Cli] refs
    to produce a single immutable configuration value. *)
let current () =
  let cli =
    {
      debug = !Common.Cli.debug;
      ci = !Common.Cli.ci;
      json = !Common.Cli.json;
      write = !Common.Cli.write;
      live_names = !Common.Cli.liveNames;
      live_paths = !Common.Cli.livePaths;
      exclude_paths = !Common.Cli.excludePaths;
    }
  in
  {run = Common.runConfig; cli}
