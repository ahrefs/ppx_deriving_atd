(lang dune 3.10)

(name ppx_deriving_atd)

(generate_opam_files true)

(source
 (github ahrefs/ppx_deriving_atd))

(license ISC)

(authors "Sewen Thy")

(maintainers "sewen.thy@ahrefs.com")

(package
 (name ppx_deriving_atd)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune atd ppxlib re2)
 (tags
  (topics "to describe" your project)))
