open Riot
module Params = DBCaml.Params

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
        ~connector:(module Dbcaml_driver_postgres)
        ~connections:5
        ~connection_string:
          "postgresql://postgres:postgres@localhost:6432/postgres?sslmode=disable"
    in

    DBCaml.connect ~config
  in

  (* (* Fetch the user and return the user to a variable *) *)
  (* let* rows_affected = *)
  (*   DBCaml.execute *)
  (*     db *)
  (*     ~params: *)
  (*       [ *)
  (*         DBCaml.Params.string "Emil"; *)
  (*         DBCaml.Params.bool true; *)
  (*         DBCaml.Params.string "Danza"; *)
  (*         DBCaml.Params.number 1; *)
  (*         DBCaml.Params.number 1; *)
  (*         DBCaml.Params.float 1.1; *)
  (*         DBCaml.Params.string_list ["Danza"]; *)
  (*       ] *)
  (*     ~query: *)
  (*       "insert into users (name, some_bool, pet_name, some_int64, some_int32, some_float, pets) values ($1, $2, $3, $4, $5, $6, $7)" *)
  (* in *)

  (* (* Fetch the user and return the user to a variable *) *)
  (* let* rows_affected = *)
  (*   DBCaml.execute *)
  (*     db *)
  (*     ~params:[DBCaml.Params.string "Emil"; DBCaml.Params.string "Lowa"] *)
  (*     ~query:"update users set pet_name = $2 where name = $1" *)
  (* in *)

  (* (* Fetch the user and return the user to a variable *) *)
  (* let* rows_affected = *)
  (*   DBCaml.execute *)
  (*     db *)
  (*     ~params:[Params.string "Emil"] *)
  (*     ~query:"delete from users where name = $1" *)
  (* in *)
  Ok 1
