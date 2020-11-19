(lang dune 2.5)
(generate_opam_files true)

(name custom-caqti-type)
(version 0.0.1)

(license "MIT")
(authors "Sébastien Dailly")
(maintainers "Sébastien Dailly <sebastien-ocaml@chimrod.com>")

(package
  (name custom-caqti-type)
  (synopsis "PPX deriving for custom caqti types")
  (depends
    (ocaml (>= 4.05.0))
    (ppx_optcomp (>= 0.14.0))
    (ocaml-migrate-parsetree (>= 1.8.0))
    (ppxlib (>= 0.15.0))
  )
)
