open Riot
open Prelude

open Logger.Make (struct
  let namespace = ["dbcaml"]
end)

(* Export Modules *)
module Connection = Connection
module Driver = Driver
module Params = Params
module Error = Error

(**
 * start_link is the main function for Dbcaml, starts the Supervisor which 
 * controls the Pool manager.
 *)
let start_link ?(connections = 10) (driver : Driver.t) =
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
      | `connection_result (Error e) -> Error (`msg e)
  in

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

(** raw_query send a query to the database and return raw bytes.
 * It handles asking for a lock a item in the pool and releasing after query is done.
 *)
let raw_query ?(row_limit = 0) ?params ~query pid =
  Pool.with_connection pid (fun conn ->
      let params = Option.value ~default:Params.empty params in
      let* result = Connection.query ~conn ~params ~query ~row_limit in
      Ok (Bytes.to_string result))

(** Query send a fetch request to the database and use the bytes to deserialize the output to a type using serde. Ideal to use for select queries *)
let query
    (type a)
    ?(params : a Params.Values.t option)
    connection
    ~query
    ~deserializer =
  let params = Option.map (fun params -> Params.VALUES params) params in
  let params = Option.value ~default:Params.empty params in
  let* result = raw_query connection.pid ~params ~query in
  let result_bytes = Bytes.of_string result in
  Driver.deserialize connection.driver deserializer result_bytes

(** Execute sends a execute command to the database and returns the amount of rows affected. Ideal to use for insert,update and delete queries  *)
let execute (type a) ?(params : a Params.Values.t option) connection ~query =
  let params = Option.map (fun params -> Params.VALUES params) params in
  let params = Option.value ~default:Params.empty params in
  let* _ = raw_query connection.pid ~params ~query in
  Ok ()
