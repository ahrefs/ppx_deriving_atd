(lang dune 3.10)

(name ppx_deriving_atd)

(generate_opam_files true)

(source
 (github ahrefs/ppx_deriving_atd))

(license MIT)

(authors "Sewen Thy")

(maintainers "sewen.thy@ahrefs.com")

(package
 (name ppx_deriving_atd)
 (synopsis "A ppx_deriving plugin for ATD")
 (description "A ppx_deriving plugin for ATD")
 (depends ocaml dune atd ppxlib)
 (tags
  (topics "ppx" "deriving" "atd")))
