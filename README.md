<h1 align="center">
  <img alt="dbcaml logo" src="https://raw.githubusercontent.com/dbcaml/dbcaml/main/images/logo.png" width="300"/>
</h1>

<p align="center">
  A database toolkit built on <a href="https://github.com/riot-ml/riot">Riot</a>
</p>

<p align="center">
  <a href="#quick-start">Quick Start</a> |
  <a href="https://dbca.ml">Documentation</a> |
  <a href="https://github.com/dbcaml/dbcaml/tree/main/examples">Examples</a> |
  &nbsp;&nbsp;
</p>


DBCaml is an async database toolkit built on <a href="https://github.com/riot-ml/riot">Riot</a>, an actor-model multi-core scheduler for OCaml 5. DBCaml is inspired by [Elixirs](https://github.com/elixir-ecto/ecto) where the developer can spin up a connection manager and connections the manager takes cares of. 

** Note: DBCaml is currently in development and is not ready for production. Only for testing purposes **

DBCaml aims to offer:

* **Database pooling**. Built using Riots lightweight process to spin up a connection pool.

* **Database Agnostic**. Support for Postgres, and more to come (MySQL, MariaDB, SQLite).

* **Built in security**. With built-in security it allows users to focus on writing queries, without being afraid of security breaches.

* **Cross Platform**. DBCaml compiles anywhere!

* **Not an ORM**. DBCaml is not an orm, it simply handles the boring stuff you don't want to deal with and allows you to have full insight on what's going on.

## Quick Start

```
opam pin dbcaml.0.0.2 git+https://github.com/dbcaml/dbcaml
```

After that, you can use any of the [examples](./examples) as a base for your app, and run them:
```
dune exec X
```
# Important
DBCaml is heavily in development, the content in this repo will change. It's not production ready and will probably have bugs
