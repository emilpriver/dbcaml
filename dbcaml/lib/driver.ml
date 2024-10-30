open Riot

open Logger.Make (struct
  let namespace = ["dbcaml"; "connection"]
end)

let ( let* ) = Result.bind

module type Intf = sig
  type config

  val connect :
    config ->
    ( Connection.t,
      [> `Closed
      | `Connection_closed
      | `Eof
      | `Exn of exn
      | `Msg of string
      | `No_info
      | `Noop
      | `Process_down
      | `Timeout
      | `Unix_error of Unix.error
      | `Would_block
      ] )
    result

  val deserialize : 'a Serde.De.t -> bytes -> ('a, Serde.error) result

  (* TODO: More of this should be extracted into the driver. Less requirements of DBCaml *)
  (* val get_rows_affected : string -> (int, string) result *)
end

type t =
  | Driver : {
      driver: (module Intf with type config = 'config);
      config: 'config;
    }
      -> t

(* let get_rows_affected = function *)
(*   | Driver { driver = (module DriverModule); _ } -> *)
(*     DriverModule.get_rows_affected *)

type 'ctx state = {
  connection_manager_pid: Pid.t;
  driver: t;
  requester_pid: Pid.t;
}

let rec wait_for_job connection_manager_pid item =
  let holder_pid = self () in
  debug (fun f -> f "%a is waiting for job" Pid.pp holder_pid);

  (match receive_any () with
  | Messages.CheckOut requester_pid ->
    debug (fun f ->
        f "Sending what i'm holding to requester %a" Pid.pp requester_pid);
    send requester_pid (Messages.HolderMessage (holder_pid, item))
  | _ -> error (fun f -> f "Unknown message"));

  wait_for_job connection_manager_pid item

let start_link { connection_manager_pid; driver; requester_pid } =
  let* conn =
    match driver with
    | Driver { driver = (module DriverModule); config } ->
      (match DriverModule.connect config with
      | Ok e -> Ok e
      | Error (`Msg e) ->
        send requester_pid (Messages.ConnectionResult (Error e));
        Error (`Msg e)
      | Error `Connection_closed
      | Error `Closed ->
        send
          requester_pid
          (Messages.ConnectionResult
             (Error "Connection closed, is the database up?"));
        Error (`Msg "Connection closed, is the database up?")
      | Error e ->
        send
          requester_pid
          (Messages.ConnectionResult
             (Error "Failed to start connection, is the database up?"));
        Error e)
  in

  let child_pid =
    spawn_link (fun () -> wait_for_job connection_manager_pid conn)
  in

  send connection_manager_pid (Messages.CheckIn child_pid);

  (* Tell the main process we have a connection ready*)
  send requester_pid (Messages.ConnectionResult (Ok ()));

  debug (fun f -> f "Created a new holder with pid: %a" Pid.pp child_pid);

  Ok child_pid

let child_spec requester_pid connection_manager_pid (driver : t) =
  let state = { connection_manager_pid; requester_pid; driver } in

  Supervisor.child_spec start_link state

let deserialize driver (deserializer : 'a Serde.De.t) buf =
  match driver with
  | Driver { driver = (module D : Intf with type config = _); _ } ->
    D.deserialize deserializer buf
