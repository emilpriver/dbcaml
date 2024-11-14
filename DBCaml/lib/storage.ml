open Riot

open Logger.Make (struct
  let namespace = ["dbcaml"; "storage"]
end)

type status =
  | Ready
  | Busy

(*
   * This function try to find the specific key in the storage,
   * and if it don't exist do it add it, otherwise it do replace it
*)
let add_or_replace global_storage storage_mutex key value =
  Mutex.lock storage_mutex;
  (match Hashtbl.find_opt global_storage key with
  | Some _ -> Hashtbl.replace global_storage key value
  | None -> Hashtbl.add global_storage key value);
  Mutex.unlock storage_mutex

let get global_storage storage_mutex key =
  Mutex.lock storage_mutex;
  let value = Hashtbl.find_opt global_storage key in
  Mutex.unlock storage_mutex;
  value

let remove global_storage storage_mutex key =
  Mutex.lock storage_mutex;
  Hashtbl.remove global_storage key;
  Mutex.unlock storage_mutex

(*
* This function fold over all available items in the global_storage
* and try to find someone which are ready, when it finds a holder which is available do it return it
* otherwise do it return a error to let the request know it needs to wait
*)
let available_holder global_storage storage_mutex =
  Mutex.lock storage_mutex;
  let result =
    Hashtbl.fold
      (fun key x accu ->
        match x with
        | Ready -> (key, x) :: accu
        | _ -> accu)
      global_storage
      []
  in
  Mutex.unlock storage_mutex;

  match List.length result with
  | 0 -> Error "No available items"
  | _ -> Ok (List.hd result)
