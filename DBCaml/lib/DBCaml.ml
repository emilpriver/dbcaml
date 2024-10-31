open Riot
module Connection = Connection
module Driver = Driver
module Res = Res
module Params = Params

open Logger.Make (struct
  let namespace = ["dbcaml"]
end)

let ( let* ) = Result.bind

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

  let* _supervisor_pid =
    match Supervisor.start_link ~restart_limit:10 ~child_specs () with
    | Ok pid -> Ok pid
    | Error _ -> Error (`Msg "Failed to start supervisor")
  in

  let* _ = wait_for_connections connections 0 in

  debug (fun f -> f "Started %d connections" connections);

  Ok pool_id

(** raw_query send a query to the database and return raw bytes.
* It handles asking for a lock a item in the pool and releasing after query is done.
*)
let raw_query ?(row_limit = 0) connection_manager_id ~params ~query =
  let p =
    match params with
    | Some opts -> opts
    | None -> []
  in

  let (holder_pid, connection) =
    match Pool.get_connection connection_manager_id with
    | Ok h -> h
    | Error e -> failwith e
  in

  let result =
    match Connection.query ~conn:connection ~params:p ~query ~row_limit with
    | Ok s -> Ok (Bytes.to_string s)
    | Error e -> Error (Res.execution_error_to_string e)
  in

  Pool.release_connection connection_manager_id ~holder_pid;

  result

let ( let* ) = Result.bind

module type Intf = sig
  val connection : string -> Driver.t
end

type t =
  | Ready of {
      driver: (module Intf);
      connections: int;
      connection_string: string;
    }
  | Connected of {
      driver: Driver.t;
      connections: int;
      connection_string: string;
      conn_mgr_pid: Riot.Pid.t;
    }

(** Create a new config based on the provided params.  *)
let config ~connections ~driver ~connection_string =
  Ready { driver; connections; connection_string }

(** 
  Start a connection to the database.
  This spins up a pool and creates the amount of connections provided in the config
*)
let connect ~config =
  match config with
  | Ready { driver = (module Driver); connections; connection_string; _ } ->
    let connection = Driver.connection connection_string in
    (match start_link ~connections connection with
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

(** Query send a fetch request to the database and use the bytes to deserialize the output to a type using serde. Ideal to use for select queries *)
let query ?params config ~query ~deserializer =
  match config with
  | Ready _ ->
    (* let* self = connect ~config in *)
    (* query self ~params ~query:(Serde.to_string query) *)
    failwith "NOT CONNECTED YET BUT WE SHOULD DO THIS AUTOMATICALLY"
  | Connected { conn_mgr_pid; driver; _ } ->
    let* result = raw_query conn_mgr_pid ~params ~query in
    let result_bytes = Bytes.of_string result in
    begin
      match Driver.deserialize driver deserializer result_bytes with
      | Ok t -> Ok (Some t)
      | Error e ->
        Error (Format.asprintf "Deserialize error: %a" Serde.pp_err e)
      (* | None -> Ok None) *)
    end

(** Execute sends a execute command to the database and returns the amount of rows affected. Ideal to use for insert,update and delete queries  *)
let execute ?(params = []) config ~query =
  match config with
  | Connected { conn_mgr_pid; _ } ->
    let params =
      if List.length params > 0 then
        Some params
      else
        None
    in

    let* _ = raw_query conn_mgr_pid ~params ~query in
    (* DBCaml.Driver.get_rows_affected driver result  *)
    (* Ok 0 *)
    failwith "IMPLEMENT ROWS AFFECTED"
  | Ready _ -> Error "Should be a connected config"
