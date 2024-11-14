(** Close a portal *)
let close ~portal_name =
  let sync_buffer = Buffer.create 20 in
  Buffer.add_char sync_buffer 'C';
  Buffer_tools.put_length_prefixed sync_buffer (fun buf ->
      Buffer.add_char buf 'P';
      Buffer.add_string buf portal_name;
      Buffer.add_int8 buf 0);
  sync_buffer
