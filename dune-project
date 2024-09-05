(lang dune 3.10)

(name ppx_deriving_atd)

(generate_opam_files true)

(source
 (github ahrefs/ppx_deriving_atd))

(authors "Author Name")

(maintainers "Maintainer Name")

(license "MIT")

(documentation https://git.ahrefs.com/ahrefs/ppx_deriving_atd/README.md)

(package
 (name ppx_deriving_atd)
 (synopsis "PPX deriver for JSON and ATD files")
 (description "Using OCaml as source of truth for JSON (de)serializing and generate ATD file for other services to use..")
 (depends
  ocaml
  dune
  ppxlib
  re2)
 (tags
  (topics ATD ppx_deriving)))
