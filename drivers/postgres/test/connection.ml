open Alcotest

let test_connection_should_fail () =
  let connection =
    DBCamlPostgres.connect
      "postgresql://postgres-i-dont-exist:postgres@localhost:1337/postgres?sslmode=disable"
  in

  match connection with
  | Driver { driver = (module DriverModule); config } ->
    (match DriverModule.connect config with
    | Ok _ ->
      Alcotest.fail
        "should not be able to start connection as the port is wrong"
    | Error _ -> ())

let suite =
  [
    test_case
      "Does handle connection problems"
      `Quick
      test_connection_should_fail;
  ]
