(lang dune 2.5)
(generate_opam_files true)

(name custom-caqti-type)
(version 0.0.2)

(license "MIT")
(authors "Sébastien Dailly")
(maintainers "Sébastien Dailly <sebastien-ocaml@chimrod.com>")

(package
  (name custom-caqti-type)
  (synopsis "PPX deriving for custom caqti types")
  (depends
    (ocaml (>= 4.14.0))
    (ppx_optcomp (>= 0.15.0))
    (ppxlib (>= 0.25.1))
  )
)
