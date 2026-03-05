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

let all () =
  runConfig.dce <- true;
  runConfig.exception_ <- true;
  runConfig.termination <- true

let dce () = runConfig.dce <- true
let exception_ () = runConfig.exception_ <- true
let termination () = runConfig.termination <- true

let transitive b = runConfig.transitive <- b
