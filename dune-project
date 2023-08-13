(lang dune 3.10)

(name advent_of_code)

(generate_opam_files true)

(source
 (github DanikVitek/advent-of-code-2022))

(authors "Danik Vitek")

(maintainers "Danik Vitek")

(license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name advent_of_code)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune (alcotest :with-test))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
