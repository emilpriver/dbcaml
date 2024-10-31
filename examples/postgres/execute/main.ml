open Riot
module Params = Dbcaml.Params

open Logger.Make (struct
  let namespace = ["examples"; "basic_postgres"]
end)

let ( let* ) = Result.bind

let () =
  Riot.run_with_status ~on_error:(fun x -> failwith x) @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  set_log_level (Some Logger.Debug);
  info (fun f -> f "Starting application");

  (* Start the database connection pool *)
  let* db =
    let config =
      Dbcaml.config
        ~connections:5
        ~driver:(module Dbcaml_driver_postgres)
        ~connection_string:
          "postgresql://postgres:postgres@localhost:6432/postgres?sslmode=disable"
    in

    Dbcaml.connect ~config
  in

  (* Fetch the user and return the user to a variable *)
  let* rows_affected =
    Dbcaml.execute
      db
      ~params:
        [
          Dbcaml.Params.string "Emil";
          Dbcaml.Params.bool true;
          Dbcaml.Params.string "Danza";
          Dbcaml.Params.number 1;
          Dbcaml.Params.number 1;
          Dbcaml.Params.float 1.1;
          Dbcaml.Params.string_list ["Danza"];
        ]
      ~query:
        "insert into users (name, some_bool, pet_name, some_int64, some_int32, some_float, pets) values ($1, $2, $3, $4, $5, $6, $7)"
  in

  let _ = rows_affected in

  (* Fetch the user and return the user to a variable *)
  let* rows_affected =
    Dbcaml.execute
      db
      ~params:[Dbcaml.Params.string "Emil"; Dbcaml.Params.string "Lowa"]
      ~query:"update users set pet_name = $2 where name = $1"
  in

  let _ = rows_affected in

  (* Fetch the user and return the user to a variable *)
  let* rows_affected =
    Dbcaml.execute
      db
      ~params:[Params.string "Emil"]
      ~query:"delete from users where name = $1"
  in

  let _ = rows_affected in

  Ok 1
