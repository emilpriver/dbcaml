(*
* Create a new type t that holds a connection and a function to execute a query which all the drivers will implement and use.
* This makes us able to  
*)

type t =
  | C : {
      (* 'conn is a generic *)
      conn: 'conn;
      (* This function takes a 'generic conn and a query. And return a Row.T list which is our type of a row *)
      query:
        connection:'conn ->
        params:Params.t list ->
        query:string ->
        row_limit:int ->
        (string Riot.IO.Reader.t, Res.execution_error) result;
    }
      -> t

(* Create a new connection based of type t.
   This function is used by the drivers to make it possible to have different drivers without doing a lot of matching what type the driver is *)
let make ~conn ~query () = Ok (C { conn; query })

let query ~conn ~params ~query ~row_limit =
  match conn with
  | C c -> c.query ~connection:c.conn ~params ~query ~row_limit
