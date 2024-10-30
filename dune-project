(lang dune 3.12)

(name dbcaml)

(generate_opam_files true)

(source
 (github emilpriver/dbcaml))

(authors "Emil Priver <emil@priver.dev>")

(maintainers "Emil Priver <emil@priver.dev>")

(license MIT)

(documentation https://github.com/emilpriver/dbcaml)

(pin
 (url "git+https://github.com/serde-ml/serde.git")
 (package (name serde)))

(pin
 (url "git+https://github.com/serde-ml/serde.git")
 (package (name serde_json)))

(pin
 (url "git+https://github.com/serde-ml/serde.git")
 (package (name serde_derive)))

(pin
 (url "git+https://github.com/riot-ml/riot.git")
 (package (name riot)))


(package
 (name dbcaml)
 (synopsis "Database toolkit for OCaml")
 (description "Database toolkit for OCaml")
 (depends
  ocaml
  dune
  (alcotest :with-test)
  (riot
   (>= 0.0.9))
  serde
  uri)
 (tags
  (topics "database" "dbcaml")))

(package
 (name dbcaml-driver-postgres)
 (synopsis "Postgres driver for dbcaml")
 (description "Postgres driver for dbcaml")
 (depends
  ocaml
  (alcotest :with-test)
  dune
  (riot
   (>= 0.0.9))
  bytestring
  cryptokit
  castore
  (tls (>= 1.0.0))
  uri
  ;; Internal
  dbcaml
  serde-postgres)
 (tags
  (topics "database" "dbcaml" "dbcaml")))

(package
 (name silo)
 (synopsis "Silo")
 (description "Silo")
 (depends
  ocaml
  (alcotest :with-test)
  dune
  dbcaml
  serde
  serde_derive
  serde-postgres)
 (tags
  (topics "postgres" "ocaml")))

(package
 (name serde-postgres)
 (synopsis "serde-postgres")
 (description "serde-postgres")
 (depends
  ocaml
  (alcotest :with-test)
  dune
  serde
  serde_derive)
 (tags
  (topics "postgres" "serde" "ocaml")))

(package
 (name SerdeSqlite)
 (synopsis "Sqlite Serde Stuff")
 (description "Sqlite Serde Stuff")
 (depends
  ocaml
  (alcotest :with-test)
  dune
  sqlite3
  serde
  silo
  serde_json
  serde_derive
  yojson)
 (tags
  (topics "sqlite" "serde" "ocaml")))

(package
  (name dbcaml-examples)
  (depends dbcaml dbcaml-driver-postgres serde riot silo))
