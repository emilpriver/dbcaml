open Dbcaml_driver_postgres
open Riot

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in

  Logger.set_log_level (Some Logger.Debug);

  Logger.info (fun f -> f "Starting application");

  let _ =
    Pg_pool.PgPool.connect
      "psql://postgres:mysecretpassword@localhost:6437/app?sslmode=disable"
  in

  ()