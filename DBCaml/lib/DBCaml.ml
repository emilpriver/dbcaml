open Riot
module Connection = Connection
module Driver = Driver
module Res = Res
module Params = Params
module Error = Error

open Logger.Make (struct
  let namespace = ["dbcaml"]
end)

let ( let* ) = Error.bind

let deserialize = Driver.deserialize

let rec wait_for_connections max_connections connections =
  if max_connections == connections then
    Ok ()
  else
    let selector msg =
      match msg with
      | Messages.ConnectionResult r -> `select (`connection_result r)
      | _ -> `skip
    in
    match receive ~selector () with
    | `connection_result (Ok ()) ->
      wait_for_connections max_connections (connections + 1)
    | `connection_result (Error e) -> Error (`Msg e)

(**
 * start_link is the main function for Dbcaml, starts the Supervisor which 
 * controls the Pool manager.
 *)
let start_link ?(connections = 10) (driver : Driver.t) =
  let global_storage : (Pid.t, Storage.status) Hashtbl.t =
    Hashtbl.create connections
  in

  let pool_id =
    Pool.start_link ~pool_size:connections ~storage:global_storage
  in

  let child_specs =
    List.init connections (fun _ -> Driver.child_spec (self ()) pool_id driver)
  in

  let* _ = Supervisor.start_link ~restart_limit:10 ~child_specs () in
  let* _ = wait_for_connections connections 0 in
  debug (fun f -> f "Started %d connections" connections);

  Ok pool_id

(** raw_query send a query to the database and return raw bytes.
* It handles asking for a lock a item in the pool and releasing after query is done.
*)
let raw_query ?(row_limit = 0) connection_manager_id ~params ~query :
    (string, 'a) Error.or_error =
  let p = Option.value ~default:[] params in
  let (holder_pid, connection) =
    match Pool.get_connection connection_manager_id with
    | Ok h -> h
    | Error e -> failwith e
  in

  let result =
    Connection.query ~conn:connection ~params:p ~query ~row_limit
    |> Result.map Bytes.to_string
  in

  Pool.release_connection connection_manager_id ~holder_pid;

  result

let ( let* ) = Result.bind

module type CONNECTOR = sig
  val connect : string -> Driver.t
end

type config = {
  connector: (module CONNECTOR);
  connections: int;
  connection_string: string;
}

(** Create a new config based on the provided params.  *)
let config ~connections ~connector ~connection_string =
  { connector; connections; connection_string }

type t = {
  pid: Riot.Pid.t;
  driver: Driver.t;
  connections: int;
  connection_string: string;
}

(** 
  Start a connection to the database.
  This spins up a pool and creates the amount of connections provided in the config
*)
let connect ~(config : config) =
  let { connector = (module C); connections; connection_string } = config in
  let driver = C.connect connection_string in
  start_link ~connections driver
  |> Result.map (fun pid -> { driver; connections; connection_string; pid })

(** Query send a fetch request to the database and use the bytes to deserialize the output to a type using serde. Ideal to use for select queries *)
let query ?params connection ~query ~deserializer =
  let* result = raw_query connection.pid ~params ~query in
  let result_bytes = Bytes.of_string result in
  match Driver.deserialize connection.driver deserializer result_bytes with
  | Ok r -> Ok r
  | Error _ -> failwith "OH NO"

(** Execute sends a execute command to the database and returns the amount of rows affected. Ideal to use for insert,update and delete queries  *)
let execute ?(params = []) connection ~query =
  let params =
    if List.length params > 0 then
      Some params
    else
      None
  in

  let* _ = raw_query connection.pid ~params ~query in
  Ok ()
