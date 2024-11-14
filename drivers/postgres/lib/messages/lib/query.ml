let ( let* ) = Result.bind

(** Create a query. This is for example: select * from users *)
let query ~query =
  let buf = Buffer.create 0 in

  (* Calculates the total length of the message to be sent. PostgreSQL protocol requires
     the length to include the size of the length field itself (4 bytes), the message type field (1 byte),
     and the length of the query string, plus a null terminator (1 byte) for the string. *)
  let message_length = 4 + 1 + String.length query in

  Buffer.add_char buf 'Q';
  Buffer.add_int32_be buf (Int32.of_int message_length);
  Buffer_tools.put_str_null buf query;

  buf
