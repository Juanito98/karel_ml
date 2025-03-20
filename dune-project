(lang dune 3.18)

(name karel_compiler)

(using menhir 3.0)

(source
 (github username/reponame))

(authors "Juan Carlos Sigler Priego <juanito@omegaup.com>")

(maintainers "Juan Carlos Sigler Priego <juanito@omegaup.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name karel_compiler)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  (ocaml
   (< 5.3))
  menhir
  core
  core_unix
  notty
  ppx_deriving
  ppx_jane
  xml-light)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
