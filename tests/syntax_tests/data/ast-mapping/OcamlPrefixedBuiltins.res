@@ocaml.warning("-32")
@@ocaml.ppwarning("lint warning")

%%ocaml.error("ppx error")

let x = @ocaml.warning("-3") %ocaml.error("nested")
