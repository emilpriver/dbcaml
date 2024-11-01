(** Execute function is used to tell postgres is time to execute the function as all the data required should be sent *)
let execute ~portal_name ~row_limit =
  let execute_buffer = Buffer.create 20 in
  Buffer.add_char execute_buffer 'E';
  Buffer_tools.put_length_prefixed execute_buffer (fun buf ->
      Buffer_tools.put_str_null buf portal_name;
      Buffer.add_int32_be buf (Int32.of_int row_limit));
  execute_buffer
