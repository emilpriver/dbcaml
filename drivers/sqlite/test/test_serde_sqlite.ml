open Alcotest

let ( let* ) = Result.bind

type user = {
  name: string;
  age: int;
  balance: float;
}
[@@deriving serialize, deserialize]

type users = user list [@@deriving serialize, deserialize]

let test_sqlite_open () =
  Result.get_ok
  @@
  let config =
    Dbcaml.config
      ~connections:1
      ~driver:(module DBCamlSqlite.DBCaml.Driver)
      ~connection_string:"test.db"
  in
  let* db = Dbcaml.connect ~config in
  let drop = "DROP TABLE IF EXISTS users" in
  let* _ = Dbcaml.execute db ~params:[] ~query:drop in
  let sql =
    "CREATE TABLE users (
      name TEXT,
      age INTEGER,
      balance FLOAT
    )"
  in
  let* _ = Dbcaml.execute db ~params:[] ~query:sql in
  let* _ =
    Dbcaml.execute
      db
      ~query:"INSERT INTO users (name, age, balance) VALUES (?, ?, ?)"
      ~params:[String "teej_dv"; Number 30; Float 3.14]
  in
  let* _ =
    Dbcaml.execute
      db
      ~query:"INSERT INTO users (name, age, balance) VALUES (?, ?, ?)"
      ~params:[String "theprimeagen"; Number 45; Float 4.20]
  in

  let* rows =
    Dbcaml.query
      db
      ~params:[]
      ~query:"SELECT * FROM users"
      ~deserializer:deserialize_users
  in
  let rows = Option.get rows in
  (* Teej User *)
  let teej = List.find (fun user -> user.name = "teej_dv") rows in
  Alcotest.(check string) "name" "teej_dv" teej.name;
  Alcotest.(check int) "age" 30 teej.age;
  Alcotest.(check (float 0.001)) "balance" 3.14 teej.balance;

  (* Prime User *)
  let check_prime user =
    Alcotest.(check string) "name" "theprimeagen" user.name;
    Alcotest.(check int) "age" 45 user.age;
    Alcotest.(check (float 0.001)) "balance" 4.20 user.balance
  in
  let theprimeagen = List.find (fun user -> user.name = "theprimeagen") rows in
  check_prime theprimeagen;

  (* Now select with WHERE query *)
  let* rows =
    Dbcaml.query
      db
      ~params:[String "theprimeagen"]
      ~query:"SELECT * FROM users WHERE name = ?"
      ~deserializer:deserialize_users
  in
  let rows = Option.get rows in
  Alcotest.(check int) "Should only be one row" 1 (List.length rows);
  let theprimeagen = List.find (fun user -> user.name = "theprimeagen") rows in
  check_prime theprimeagen;
  Ok ()

let () =
  Riot.run @@ fun () ->
  let _ = Riot.Logger.start () in
  Riot.Logger.set_log_level (Some Debug);

  run
    "Serde + Sqlite"
    [("Basic Tests", [test_case "Sqlite Test Cases" `Quick test_sqlite_open])]
