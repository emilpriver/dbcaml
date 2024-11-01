open Riot

module Log = Logger.Make (struct
  let namespace = ["dbcaml"; "dbcaml_postgres_driver"]
end)

let debug log_string =
  Log.debug (fun f -> f "PID %a: %s" Pid.pp (self ()) log_string)

let info log_string =
  Log.info (fun f -> f "PID %a: %s" Pid.pp (self ()) log_string)

let warn log_string =
  Log.warn (fun f -> f "PID %a: %s" Pid.pp (self ()) log_string)

let trace log_string =
  Log.trace (fun f -> f "PID %a: %s" Pid.pp (self ()) log_string)

let error log_string =
  Log.error (fun f -> f "PID %a: %s" Pid.pp (self ()) log_string)
