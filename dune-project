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
   (>= 5.3))))

(package
 (name analysis)
 (synopsis "ReScript Analysis")
 (depends
  (ocaml
   (>= 5.3))
  (cppo
   (= 1.8.0))
  eio
  eio_main
  dune))

(package
 (name tools)
 (synopsis "ReScript Tools")
 (depends
  (ocaml
   (>= 5.3))
  (cmarkit
   (>= 0.3.0))
  (cppo
   (= 1.8.0))
  analysis
  dune))
