open Alcotest

let test_queries () =
  let test_cases =
    [
      ("Query without params", [], "select * from users limit 2");
      ( "Query with string param",
        [DBCaml.Params.String "Alice"],
        "select * from users where name = $1 limit 2" );
      ( "Query with int param",
        [DBCaml.Params.Number 12],
        "select * from users where id > $1 limit 2" );
      ( "Query with float param",
        [DBCaml.Params.Float 1.1],
        "select * from users where some_float = $1 limit 2" );
      ( "Query with bool param",
        [DBCaml.Params.Bool true],
        "select * from users where some_bool = $1 limit 2" );
      ( "Query with 1 string array param",
        [DBCaml.Params.StringArray ["Alice"]],
        "select * from users where name = any($1) limit 2" );
      ( "Query with 2 string array param",
        [DBCaml.Params.StringArray ["Alice"; "Bob"]],
        "select * from users where name = any($1) limit 2" );
      ( "Query with number array param",
        [DBCaml.Params.NumberArray [1; 2]],
        "select * from users where id = any($1) limit 2" );
      ( "Query with 1 number array param",
        [DBCaml.Params.NumberArray [1]],
        "select * from users where  id = any($1) limit 2" );
      ( "Query with different type param",
        [
          DBCaml.Params.NumberArray [1];
          DBCaml.Params.String "Alice";
          DBCaml.Params.Bool true;
        ],
        "select * from users where id = any($1) and name = $2 and some_bool = $3  limit 2"
      );
    ]
  in

  let connection =
    Dbcaml_driver_postgres.connect
      "postgresql://postgres:postgres@localhost:6432/postgres?sslmode=disable"
  in

  let conn =
    match connection with
    | Driver { driver = (module DriverModule); config } ->
      (match DriverModule.connect config with
      | Ok e -> Ok e
      | Error (`Msg e) ->
        Alcotest.fail
          (Printf.sprintf "should be able to start a connection: %S" e)
      | Error _ -> Alcotest.fail "Should be able to start a connection")
  in
  let conn = Result.get_ok conn in

  List.iter
    (fun (name, params, query) ->
      let result =
        match DBCaml.Connection.query ~conn ~params ~query ~row_limit:0 with
        | Ok s -> Ok (Bytes.to_string s)
        | Error e ->
          Alcotest.fail
            (Printf.sprintf
               "%S : %S"
               name
               (DBCaml.Res.execution_error_to_string e))
      in
      let result = Result.get_ok result in
      let first_char = result.[0] in

      Alcotest.(check char)
        (Printf.sprintf "%S : Does return a successful response" name)
        'T'
        first_char)
    test_cases

let test_unsuccessful_query () =
  let connection =
    Dbcaml_driver_postgres.connect
      "postgresql://postgres:postgres@localhost:6432/postgres?sslmode=disable"
  in

  let conn =
    match connection with
    | Driver { driver = (module DriverModule); config } ->
      (match DriverModule.connect config with
      | Ok e -> Ok e
      | Error (`Msg e) ->
        Alcotest.fail
          (Printf.sprintf "should be able to start a connection: %S" e)
      | Error _ -> Alcotest.fail "Should be able to start a connection")
  in
  let conn = Result.get_ok conn in

  match
    DBCaml.Connection.query
      ~conn
      ~params:[DBCaml.Params.Number 10]
      ~query:"select * from users where i_dont_exist > $1 limit 2"
      ~row_limit:0
  with
  | Ok r ->
    Printf.printf "%S" (Bytes.to_string r);
    Alcotest.fail "We should error here"
  | Error _ -> ()

let suite =
  [
    test_case "Does handle unsuccessful query" `Quick test_unsuccessful_query;
    test_case "Does handle queries with different params" `Quick test_queries;
  ]
