(*
* Create a new type t that holds a connection and a function to execute a query which all the drivers will implement and use.
* This makes us able to  
*)

type t =
  | C : {
      conn: 'conn;
      query:
        connection:'conn ->
        params:Params.values ->
        query:string ->
        row_limit:int ->
        (* TODO: This would probably be good to switch to ExecutionError.t,
            but it seems like postgres doesn't actually do that yet *)
        (bytes, Error.t) result;
    }
      -> t

(* Create a new connection based of type t.
   This function is used by the drivers to make it possible to have different drivers without doing a lot of matching what type the driver is *)
let make ~conn ~query () = Ok (C { conn; query })

let query ~conn ~params ~query ~row_limit =
  match conn with
  | C c -> c.query ~connection:c.conn ~params ~query ~row_limit
