type field = {
  name: string;
  relation_id: int option;
  relation_attribute_no: int option;
  data_type_id: int;
  data_type_size: int;
  type_modifier: int;
  format: int;
}

module Decode = struct
  type state = {
    buf: bytes;
    mutable pos: int;
    amount_of_columns: int;
  }

  let make buf =
    let value = Bytes.get_uint16_be buf 5 in
    { buf; pos = 7; amount_of_columns = value }

  let get_u16 state =
    let value = Bytes.get_uint16_be state.buf state.pos in
    state.pos <- state.pos + 2;
    value

  let get_i16 state =
    let value = Bytes.get_int16_be state.buf state.pos in
    state.pos <- state.pos + 2;
    value

  let get_i32 state =
    let value = Bytes.get_int32_be state.buf state.pos in
    state.pos <- state.pos + 4;
    value

  let index_of_null state =
    (* get the amount of bytes left *)
    let rec aux i =
      if state.pos + i >= Bytes.length state.buf then
        None
      else if Bytes.get state.buf (state.pos + i) = '\000' then
        Some i
      else
        aux (i + 1)
    in
    aux 0

  let get_str_nul state =
    let ending = index_of_null state |> Option.get in
    let value = Bytes.sub state.buf state.pos ending |> Bytes.to_string in
    state.pos <- state.pos + ending;
    value

  let rec decode_row state index (acc : field list) =
    if index < state.amount_of_columns then (
      let name = get_str_nul state in
      let relation_id =
        let value = get_i32 state in
        if Int32.compare value Int32.zero = 0 then
          None
        else
          Some (Int32.to_int value)
      in
      let relation_attribute_no =
        let value = get_i16 state in
        if value == 0 then
          None
        else
          Some value
      in
      let data_type_id = get_i32 state in
      let data_type_id = Int32.to_int data_type_id in
      let data_type_size = get_i16 state in
      let type_modifier = get_i32 state in
      let type_modifier = Int32.to_int type_modifier in
      let format = get_i16 state in

      state.pos <- state.pos + 1;

      let f =
        {
          name;
          relation_id;
          relation_attribute_no;
          data_type_id;
          data_type_size;
          type_modifier;
          format;
        }
      in
      decode_row state (index + 1) (acc @ [f])
    ) else
      (*Return the field list and the buf without row description *)
      (acc, Bytes.sub state.buf state.pos (Bytes.length state.buf - state.pos))
end

(* Decode the row description so we can get which columns we have in the message. And then return the buffer without the RowDescription *)
let decode_row_description buf =
  let state = Decode.make buf in

  let (fields, buf) = Decode.decode_row state 0 [] in

  Ok (buf, fields)
