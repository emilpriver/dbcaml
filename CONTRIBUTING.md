# Contributing to DBCaml

*This file is under development and will lack information. More to come *

Thanks for taking the time to contribute to DBCaml ✨ All contributions are
welcomed! This includes:

* PRs with bug fixes or features
* Examples
* Doc fixes
* Bug reports
* Links blogs featuring DBCaml
* Links to projects using DBCaml that can serve as large examples
* Links to libraries that can be used with DBCaml

More information on how DBCaml work exist at [the documentation](https://dbca.ml)

## Installing from Sources

Run `opam install . --deps-only` to install the necessary packages.

To install DBCaml, DBCaml postgres driver, Serde postgres from sources do you need to pin each package:

```sh
; opam pin dbcaml.0.0.3 git+https://github.com/dbcaml/dbcaml
; opam pin dbcaml-driver-postgres.0.0.3 git+https://github.com/dbcaml/dbcaml
; opam pin serde-postgres.0.0.3 git+https://github.com/dbcaml/dbcaml
; opam pin silo.0.0.3 git+https://github.com/dbcaml/dbcaml
```

You can run builds with:

```sh
; dune build
```

You can run all tests with

```sh
; dune runtest
```

# Local development

To help with testing locally do it exist a `docker-compose.yaml` file in examples, This file contains all the database currently in use or planned to be used. 
Running  `docker compose up` should setup the database for you, migrate and add data, and expose it. The database urls for each database are:

- Postgres:postgresql://postgres:postgres@localhost:6432/postgres

# Building a driver

A driver within DBCaml is the thing who actually talking to the database, the main DBCaml is just a interface between the user and the database. The main package do contact the driver to ask it for executing jobs.
The way for the main package to interact with the driver is that the driver should export a module that hold the type `Driver.t`. 

[Example driver](./drivers/postgres/)

## Driver requirements

1. Each driver should only start 1 connection. The pool manager starts multiple connections using the driver.
2. The driver needs to escape params send to the database. It's also important that this is tested
3. The driver needs to be able to make a TLS upgrade(use ssl). However it should determine if it needs to make a TLS upgrade on the sslmode query parameter in the connection(`?sslmod=disabled`).
4. The driver should return the full necessary bytes from the database as this is used by other libraries

## Purpose of a Driver

The purpose of a driver is to bridge DBCaml with a database and with that also implementing security such as TLS and escaping queries.
When a driver is being developed is it important to have a extra look at the security.

