(** Tells postgres to sync so we get the response  *)
let database () =
  let sync_buffer = Buffer.create 20 in
  Buffer.add_char sync_buffer 'S';
  Buffer.add_int32_be sync_buffer (Int32.of_int 4);
  sync_buffer
