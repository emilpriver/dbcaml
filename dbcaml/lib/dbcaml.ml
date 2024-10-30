open Riot
module Connection = Connection
module Driver = Driver
module Res = Res
module Params = Params

let ( let* ) = Result.bind

open Logger.Make (struct
  let namespace = ["dbcaml"]
end)

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
    | Ok s -> Ok s
    | Error e -> Error (Res.execution_error_to_string e)
  in

  Pool.release_connection connection_manager_id ~holder_pid;

  result

let deserialize = Driver.deserialize
