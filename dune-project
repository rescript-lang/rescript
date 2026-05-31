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
  (cppo
   (= 1.8.0))
  (flow_parser
   (= 0.267.0))
  (ocamlformat
   (and :with-test (= 0.27.0)))
  (yojson
   (and :with-test (= 3.0.0)))
  (ounit2
   (and :with-test (= 2.2.7)))
  (odoc :with-doc)
  (ocaml-lsp-server
   (and :with-dev-setup (>= 1.23.0)))
  (bisect_ppx
   (and :with-dev-setup (>= 2.8.0)))
  (js_of_ocaml
   (and
    (<> :os "win32")
    :with-test
    (= 6.0.1)))
  (wasm_of_ocaml-compiler
   (and
    (<> :os "win32")
    :with-test
    (= 6.0.1)))))

(package
 (name analysis)
 (synopsis "ReScript Analysis")
 (depends
  (ocaml
   (>= 5.0.0))
  (cppo
   (= 1.8.0))
  (odoc :with-doc)
  (lsp
   (>= 1.23.0))
  (yojson
    (= 3.0.0))))

(package
 (name tools)
 (synopsis "ReScript Tools")
 (depends
  (ocaml
   (>= 5.0.0))
  (cmarkit
   (>= 0.3.0))
  (cppo
   (= 1.8.0))
  analysis
  (yojson
    (= 3.0.0))
  (odoc :with-doc)))

(package
 (name rescript-language-server)
 (synopsis "ReScript LSP")
 (depends
 (ocaml
  (>= 4.10))
 (lsp
  (>= 1.22.0))
 (eio
  (>= 1.3))
 (eio_main
  (>= 1.3))
 analysis
 dune))
