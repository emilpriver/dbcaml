open Dbcaml.Params

let ( let* ) = Result.bind

(** String param *)
let string v = String v

(** Number param *)
let number v = Number v

(** Float param *)
let float v = Float v

(** Bool param *)
let bool v = Bool v

(** List of strings param  *)
let string_list v = StringArray v

(** List of numbers param *)
let number_list v = NumberArray v

module type Intf = sig
  val connection : string -> Dbcaml.Driver.t
end

type t =
  | Ready_to_connect of {
      driver: (module Intf);
      connections: int;
      connection_string: string;
    }
  | Connected of {
      driver: Dbcaml.Driver.t;
      connections: int;
      connection_string: string;
      conn_mgr_pid: Riot.Pid.t;
    }

(** Create a new config based on the provided params.  *)
let config ~connections ~driver ~connection_string =
  Ready_to_connect { driver; connections; connection_string }

(** 
  Start a connection to the database.
  This spins up a pool and creates the amount of connections provided in the config
*)
let connect ~config =
  match config with
  | Ready_to_connect
      { driver = (module DriverModule); connections; connection_string; _ } ->
    let connection = DriverModule.connection connection_string in
    (match Dbcaml.start_link ~connections connection with
    | Ok c ->
      Ok
        (Connected
           {
             driver = connection;
             connections;
             connection_string;
             conn_mgr_pid = c;
           })
    | Error (`Msg error_message) -> Error error_message)
  | Connected _ -> Error "You can't connect with a connected config"

(* Check if we have rows back. If we don't have rows shouldn't we try to start a deserializer as there is no data *)
let have_rows message =
  let data_row_description_length =
    Bytes.get_int32_be message 1 |> Int32.to_int
  in
  match Bytes.get message (data_row_description_length + 1) with
  | 'D' -> Some ()
  | _ -> None

(** Query send a fetch request to the database and use the bytes to deserialize the output to a type using serde. Ideal to use for select queries *)
let query ?params config ~query ~deserializer =
  match config with
  | Connected { conn_mgr_pid; driver; _ } ->
    let* result = Dbcaml.raw_query conn_mgr_pid ~params ~query in
    let result_bytes = Bytes.of_string result in
    (match have_rows result_bytes with
    | Some _ ->
      (match Dbcaml.deserialize driver deserializer result_bytes with
      | Ok t -> Ok (Some t)
      | Error e ->
        Error (Format.asprintf "Deserialize error: %a" Serde.pp_err e))
    | None -> Ok None)
  | Ready_to_connect _ -> Error "Should be a connected config"

(** Used internally. parse_command_complete reads the "Command Complete" message which starts with a C and reads how many rows that is effected and return a int
    If it's unable to do so do it return a error *)
let parse_command_complete message =
  try
    let length = String.length message in
    (* Find the position of the last space, before the number of rows *)
    let space_pos = String.rindex message ' ' in
    (* Extract the number from space position to the end minus the null character *)
    let number_str =
      String.sub message (space_pos + 1) (length - space_pos - 2)
    in
    (* Convert the extracted string to an integer *)
    let value = int_of_string number_str in
    Ok value
  with
  | _ -> Error "failed to parse command complete message"

(** Execute sends a execute command to the database and returns the amount of rows affected. Ideal to use for insert,update and delete queries  *)
let execute ?(params = []) config ~query =
  match config with
  | Connected { conn_mgr_pid;  _ } ->
    let params =
      if List.length params > 0 then
        Some params
      else
        None
    in

    (* (* Dbcaml.get_rows_affected  *) *)
    (* let* rows_affected = parse_command_complete result in *)
    (**)
    (* Ok rows_affected *)
    let* _ = Dbcaml.raw_query conn_mgr_pid ~params ~query in
    (* Dbcaml.Driver.get_rows_affected driver result  *)
    Ok 0
  | Ready_to_connect _ -> Error "Should be a connected config"
