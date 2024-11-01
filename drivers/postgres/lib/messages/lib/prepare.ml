(** Prepare a new query with generated statement*)
let prepare ~statement_id ~params_oid ~query =
  let params_oid_length = List.length params_oid in
  (* Create a buffer with the size of the statement_id, query's length.
     But also add +2 to count the int16 bytes.
     And finaly add the amount of params * 4 to add the size of all the params which will be added to the buffer
  *)
  let buf =
    Buffer.create
      (String.length statement_id
      + String.length query
      + 2
      + (params_oid_length * 4))
  in

  Buffer.add_char buf 'P';
  (* Creates a new buffer with statement_id, the query and what the expected object ids are for the params provided*)
  Buffer_tools.put_length_prefixed buf (fun b ->
      Buffer_tools.put_str_null b statement_id;
      Buffer_tools.put_str_null b query;
      Buffer.add_int16_be b (List.length params_oid);
      List.iter (fun oid -> Buffer.add_int32_be b (Int32.of_int oid)) params_oid);

  buf
