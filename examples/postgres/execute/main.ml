open Riot
module Params = DBCaml.Params
module Values = DBCaml.Params.Values

open Logger.Make (struct
  let namespace = ["examples"; "basic_postgres"]
end)

let ( let* ) = Result.bind

let () =
  Riot.run_with_status ~on_error:(fun s -> failwith (DBCaml.Error.show s))
  @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  set_log_level (Some Logger.Debug);
  info (fun f -> f "Starting application");

  (* Start the database connection pool *)
  let* db =
    let config =
      DBCaml.config
        ~connector:(module DBCamlPostgres)
        ~connections:5
        ~connection_string:
          "postgresql://postgres:postgres@localhost:6432/postgres?sslmode=disable"
    in

    DBCaml.connect ~config
  in

  (* (* Fetch the user and return the user to a variable *) *)
  let* _ =
    DBCaml.execute
      db
      ~params:
        Values.
          [
            text "Emil";
            (* bool true; *)
            assert false;
            text "Danza";
            integer 1;
            integer 1;
            float 1.1;
            (* string_list ["Danza"]; *)
            assert false;
          ]
      ~query:
        "insert into users (name, some_bool, pet_name, some_int64, some_int32, some_float, pets) values ($1, $2, $3, $4, $5, $6, $7)"
  in

  (* Fetch the user and return the user to a variable *)
  let* _ =
    DBCaml.execute
      db
      ~params:Values.[text "Emil"; text "Lowa"]
      ~query:"update users set pet_name = $2 where name = $1"
  in

  (* Fetch the user and return the user to a variable *)
  let* _ =
    DBCaml.execute
      db
      ~params:Values.[text "Emil"]
      ~query:"delete from users where name = $1"
  in

  Ok 1
