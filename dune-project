(lang dune 3.17)

(name rescript)

(generate_opam_files true)

(license LGPL-3.0-or-later)

(authors "Hongbo Zhang <bobzhang1988@gmail.com>")

(maintainers "Hongbo Zhang <bobzhang1988@gmail.com>" "Cristiano Calcagno")

(homepage "https://github.com/rescript-lang/rescript-compiler")

(bug_reports "https://github.com/rescript-lang/rescript-compiler/issues")

(package
 (name rescript)
 (synopsis "ReScript compiler")
 (depends
  (ocaml
   (>= 5.0.0))
  dune
  (ppxlib
   (>= 0.38.0))
  (ppx_deriving
   (and :build (= 6.2.0)))
  (ppx_gen_rec :build)
  (wtf8
   (= 1.0.2))
  (ocamlformat
   (and :with-test (= 0.29.0)))
  (yojson
   (= 3.0.0))
  (spawn
   (= v0.17.0))
  (cmdliner
   (= 2.1.1))
  (luv
   (= 0.5.14+rescript.1))
  ; Linked into the build system through luv.
  (ctypes
   (= 0.24.0))
  (integers
   (= 0.8.0))
  (stdlib-shims
   (= 0.3.0))
  (re
   (= 1.14.0))
  (ounit2
   (and :with-test (= 2.2.7)))
  (odoc :with-doc)
  (ocaml-lsp-server
   (and :with-dev-setup (>= 1.23.0)))
  (js_of_ocaml
   (and
    (<> :os "win32")
    :with-test
    (= 6.5.0)))
  (wasm_of_ocaml-compiler
   (and
    (<> :os "win32")
    :with-test
    (= 6.5.0)))))

(package
 (name analysis)
 (synopsis "ReScript Analysis")
 (depends
  (ocaml
   (>= 5.0.0))
  (odoc :with-doc)
  (lsp
   (= 1.27.0))
  ; Linked into the binaries through lsp.
  (jsonrpc
   (= 1.27.0))
  (uutf
   (= 1.0.4))
  ; v0.17.0 needs OCaml 5.1; the OCaml 5.0 CI job gets v0.16.0.
  (ppx_yojson_conv_lib
   (and
    (>= v0.16.0)
    (<= v0.17.0)))
  (yojson
    (= 3.0.0))))

(package
 (name tools)
 (synopsis "ReScript Tools")
 (depends
  (ocaml
   (>= 5.0.0))
  (cmarkit
   (= 0.4.0))
  analysis
  (yojson
    (= 3.0.0))
  (odoc :with-doc)))
