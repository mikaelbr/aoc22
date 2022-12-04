(lang dune 3.6)

(name aoc)

(generate_opam_files true)

(source
 (github mikaelbr/aoc22))

(authors "Mikael Brevik")

(maintainers "Mikael Brevik")

(license MIT)

(documentation https://github.com/mikaelbr/aoc22)

(package
 (name aoc)
 (synopsis "Solutions to AOC 22 in OCaml")
 (description "Solutions to AOC 22 in OCaml")
 (depends ocaml dune)
 (tags ("aoc")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
