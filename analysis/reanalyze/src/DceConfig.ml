(** Configuration for dead code elimination analysis.
    
    This module encapsulates all configuration needed for DCE,
    gathered from RunConfig and CLI flags. *)

type cli_config = {
  debug: bool;
  ci: bool;
  json: bool;
  live_names: string list;
  live_paths: string list;
  exclude_paths: string list;
}

type t = {run: RunConfig.t; cli: cli_config}

(** Capture the current DCE configuration from global state.
    
    This reads from [RunConfig.runConfig] and [Cli] refs
    to produce a single immutable configuration value. *)
let current () =
  let cli =
    {
      debug = !Cli.debug;
      ci = !Cli.ci;
      json = !Cli.json;
      live_names = !Cli.liveNames;
      live_paths = !Cli.livePaths;
      exclude_paths = !Cli.excludePaths;
    }
  in
  {run = RunConfig.runConfig; cli}
