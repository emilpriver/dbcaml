(** Ask postgres to describe the statement. Used to get DataRowDescripion within the response *)
let statement ~statement_id =
  let buf = Buffer.create (String.length statement_id + 5) in

  Buffer.add_char buf 'D';
  (* Creates a new buffer with statement_id, the query and what the expected object ids are for the params provided*)
  Buffer_tools.put_length_prefixed buf (fun b ->
      Buffer.add_char b 'S';
      Buffer.add_string b statement_id;
      Buffer.add_int8 b 0);

  buf
