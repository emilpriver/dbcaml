open Riot
module Bs = Bytestring

let ( let* ) = Result.bind

(** 
  Startup message is used so postgres knows that it's time to create a new connection.
  This message normally returns the available auth messages
*)
let start ~username ~database =
  let buffer = Buffer.create 20 in

  Buffer_tools.put_length_prefixed buffer (fun b ->
      Buffer.add_string b "\000\003\000\000";
      Buffer_tools.put_str_null b "user";
      Buffer_tools.put_str_null b username;
      Buffer_tools.put_str_null b "database";
      Buffer_tools.put_str_null b database;
      Buffer.add_char b '\000');

  buffer
