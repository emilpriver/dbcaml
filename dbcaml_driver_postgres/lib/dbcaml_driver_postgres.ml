module Bs = Bytestring

let ( let* ) = Result.bind

module Postgres = struct
  type config = { conninfo: string }

  let connect config =
    let* (conn, conninfo) = Pg.connect config.conninfo in

    Pg_logger.debug "Sending startup message";
    let* _ =
      Pg.send
        conn
        ~buffer:
          (Messages.Startup.start
             ~username:conninfo.user
             ~database:conninfo.database)
    in

    let* (_, message_format, _size, _) = Pg.receive conn in

    let* _ =
      match message_format with
      | Message_format.Authentication ->
        let* _ =
          Scram_auth.authenticate
            ~conn
            ~is_plus:false
            ~username:conninfo.user
            ~password:conninfo.password
        in

        Ok conn
      | mf ->
        Error
          (`Msg
            (Printf.sprintf
               "Unexpected message format: %s"
               (Message_format.to_string ~format:mf)))
    in

    let query ~connection ~params ~query ~row_limit =
      match Executer.query ~conn:connection ~query ~row_limit ~params with
      | Ok response -> Ok response
      | Error (`Msg e) -> Error (Dbcaml.Res.ExecutionError e)
      | Error _ -> Error (Dbcaml.Res.ExecutionError "Unknown error")
    in

    (* Create a new connection which we also want to use to create a PID *)
    let* conn = Dbcaml.Connection.make ~conn ~query () in

    Ok conn

  (** Deserialize the response bytes from postgres into a type *)
  let deserialize = Serde_postgres.of_bytes

  (* let get_rows_affected message = *)
  (*   try *)
  (*     let length = String.length message in *)
  (*     (* Find the position of the last space, before the number of rows *) *)
  (*     let space_pos = String.rindex message ' ' in *)
  (*     (* Extract the number from space position to the end minus the null character *) *)
  (*     let number_str = *)
  (*       String.sub message (space_pos + 1) (length - space_pos - 2) *)
  (*     in *)
  (*     (* Convert the extracted string to an integer *) *)
  (*     let value = int_of_string number_str in *)
  (*     Ok value *)
  (*   with *)
  (*   | _ -> Error "failed to parse command complete message" *)
end

(** Create a interface which returns back a Dbcaml.Driver.t type. This type is used to create a connection and make queries *)
let connection conninfo =
  Dbcaml.Driver.Driver { driver = (module Postgres); config = { conninfo } }
