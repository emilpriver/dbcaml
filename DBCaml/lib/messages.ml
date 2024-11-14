open Riot

(* The messages here is for the connection manager to know what to do *)
type Message.t +=
  | (** Let the connection manager know that a Process ID is ready to be locked by a new process *)
      CheckIn of
      Pid.t
  | (* Locking the PID and sending it to the requester. The requester will get a connection and is later on expected to release the connection *)
      CheckOut of
      Pid.t
  | (* Lock a holder before CheckingOut *)
      LockHolder of Pid.t
  | (* Register a new holder in the connection manager *)
      NewHolder of
      Connection.t
  | (* The content fo the holder. such as the connection *)
      HolderMessage of
      Pid.t * Connection.t
  | ConnectionResult of (unit, string) result
