(lang dune 2.3)

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
   (>= 4.10))
  (ocamlformat
   (and
    :with-test
    (= 0.26.2)))
  (yojson
   (and
    :with-test
    (= 2.2.2)))
  (ocaml-lsp-server
   (and
    :with-dev-setup
    (= 1.19.0)))
  (cppo
   (= 1.6.9))
  (js_of_ocaml
   (= 5.9.1))
  (ounit2
   (= 2.2.7))
  (reanalyze
   (= 2.25.1))
  dune))

(package
 (name analysis)
 (synopsis "ReScript Analysis")
 (depends
  (ocaml
   (>= 4.10))
  (cppo
   (= 1.6.9))
  dune))

(package
 (name tools)
 (synopsis "ReScript Tools")
 (depends
  (ocaml
   (>= 4.10))
  (cppo
   (= 1.6.9))
  analysis
  dune))
