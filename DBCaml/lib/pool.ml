open Riot
open Prelude

open Logger.Make (struct
  let namespace = ["dbcaml"; "pool"]
end)

(* Creates the pool and return a Supervisor that make sure the pool is up and running *)
let start_link ~pool_size ~storage =
  let child_specs = [Manager.child_spec ~pool_size ~storage ()] in

  match Supervisor.start_link ~restart_limit:10 ~child_specs () with
  | Ok s -> s
  | Error _ -> failwith "Failed to start link"

(* Function to ask for a connection within the pool and then return the actual connection to the requester *)
let get_connection connection_manager_pid =
  send connection_manager_pid (Messages.LockHolder (self ()));

  match receive_any () with
  | Messages.HolderMessage (holder_pid, conn) -> Ok (holder_pid, conn)
  | _ -> Error (`msg "didn't get the message I expected")

(* used to release a connection after it's been used so other processes can use it *)
let release_connection connection_manager_pid ~holder_pid =
  send connection_manager_pid (Messages.CheckIn holder_pid)

let with_connection pid f =
  let* (holder_pid, conn) = get_connection pid in
  Fun.protect
    ~finally:(fun () -> release_connection pid ~holder_pid)
    (fun () -> f conn)
