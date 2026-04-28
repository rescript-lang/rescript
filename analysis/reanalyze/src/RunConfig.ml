type t = {
  mutable bsbProjectRoot: string;
  mutable dce: bool;
  mutable exception_: bool;
  mutable projectRoot: string;
  mutable suppress: string list;
  mutable termination: bool;
  mutable transitive: bool;
  mutable unsuppress: string list;
}

let runConfig =
  {
    bsbProjectRoot = "";
    dce = false;
    exception_ = false;
    projectRoot = "";
    suppress = [];
    termination = false;
    transitive = false;
    unsuppress = [];
  }

let reset () =
  runConfig.dce <- false;
  runConfig.exception_ <- false;
  runConfig.suppress <- [];
  runConfig.termination <- false;
  runConfig.transitive <- false;
  runConfig.unsuppress <- []

let all () =
  runConfig.dce <- true;
  runConfig.exception_ <- true;
  runConfig.termination <- true

let dce () = runConfig.dce <- true
let exception_ () = runConfig.exception_ <- true
let termination () = runConfig.termination <- true

let transitive b = runConfig.transitive <- b

type snapshot = {
  dce: bool;
  exception_: bool;
  suppress: string list;
  termination: bool;
  transitive: bool;
  unsuppress: string list;
}

let snapshot () =
  {
    dce = runConfig.dce;
    exception_ = runConfig.exception_;
    suppress = runConfig.suppress;
    termination = runConfig.termination;
    transitive = runConfig.transitive;
    unsuppress = runConfig.unsuppress;
  }

let equal_snapshot (a : snapshot) (b : snapshot) = a = b
