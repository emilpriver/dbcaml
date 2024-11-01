(* The bind message is used to tell postgres what params we are sending to the database *)

(* Escape a string before we send it to the database *)
let escape_sql_value value =
  let buffer = Buffer.create (String.length value) in
  String.iter
    (fun c ->
      match c with
      | '\'' ->
        Buffer.add_string
          buffer
          "''" (* Replace single quote with two single quotes *)
      | _ -> Buffer.add_char buffer c)
    value;
  Buffer.contents buffer

(* Encode int32 to bytes*)
let encode_int32 n =
  let result = Bytes.create 4 in
  Bytes.set_int32_be result 0 n;
  result

(* Encode int8 to bytes*)
let encode_int8 n = Bytes.make 1 (Char.chr n)

(* Function used to encode a param into bytes that postgres can read *)
let rec encode_param : 'a. 'a DBCaml.Params.value -> bytes =
 fun (type a) (param : a DBCaml.Params.value) ->
  let open DBCaml.Params in
  match param with
  | CONST (s, TEXT) -> escape_sql_value s |> Bytes.of_string
  | CONST (s, NULLABLE TEXT) -> begin
    match s with
    | Some s -> encode_param (CONST (s, TEXT))
    | None -> failwith "PRIIIIIIIIIIIIIIIIIIIIIIV"
  end
  | CONST (i, INTEGER) -> encode_int32 (Int32.of_int i)
  | _ -> failwith "OH NO NO NO"
(* match param with *)
(* | DBCaml.Params.Optional _ -> failwith "POSTGRES TODO" *)
(* | DBCaml.Params.String str -> escape_sql_value str |> Bytes.of_string *)
(* | DBCaml.Params.Number i -> encode_int32 (Int32.of_int i) *)
(* | DBCaml.Params.Float f -> encode_int32 (Int32.bits_of_float f) *)
(* | DBCaml.Params.Bool b -> *)
(*   if b then *)
(*     encode_int8 1 *)
(*   else *)
(*     encode_int8 0 *)
(* | DBCaml.Params.StringArray s -> *)
(*   let oid = Bytes.of_string "1015" in *)
(*   let length = List.length s in *)
(*   let header = *)
(*     Bytes.concat *)
(*       Bytes.empty *)
(*       [ *)
(*         encode_int32 1l; *)
(*         encode_int32 0l; *)
(*         oid; *)
(*         encode_int32 (Int32.of_int length); *)
(*         encode_int32 1l; *)
(*       ] *)
(*   in *)
(*   let content = *)
(*     List.fold_left *)
(*       (fun acc x -> *)
(*         let escaped = escape_sql_value x in *)
(*         Bytes.cat *)
(*           acc *)
(*           (Bytes.concat *)
(*              Bytes.empty *)
(*              [ *)
(*                encode_int32 (String.length escaped |> Int32.of_int); *)
(*                Bytes.of_string escaped; *)
(*              ])) *)
(*       Bytes.empty *)
(*       s *)
(*   in *)
(*   Bytes.concat Bytes.empty [header; content] *)
(* | DBCaml.Params.NumberArray s -> *)
(*   let oid = Bytes.of_string "1007" in *)
(*   let length = List.length s in *)
(*   let header = *)
(*     Bytes.concat *)
(*       Bytes.empty *)
(*       [ *)
(*         encode_int32 (Int32.of_int 1); *)
(*         encode_int32 (Int32.of_int 0); *)
(*         oid; *)
(*         encode_int32 (Int32.of_int length); *)
(*         encode_int32 (Int32.of_int 1); *)
(*       ] *)
(*   in *)
(*   let content = *)
(*     List.fold_left *)
(*       (fun acc x -> *)
(*         Bytes.cat *)
(*           acc *)
(*           (Bytes.concat *)
(*              Bytes.empty *)
(*              [encode_int32 4l; encode_int32 (Int32.of_int x)])) *)
(*       Bytes.empty *)
(*       s *)
(* in *)
(* Bytes.concat Bytes.empty [header; content] *)

let encode_value (value : 'a DBCaml.Params.value) =
  let value_param = encode_param value in
  let buffer = Buffer.create (Bytes.length value_param + 4) in

  let len =
    match Bytes.length value_param with
    | 0 -> -1
    | len -> len
  in

  Buffer.add_int32_be buffer (Int32.of_int len);
  Buffer.add_bytes buffer value_param;

  buffer

(** Create a bind message with the statement_id, portal_name and params *)
let bind ~statement_id ~(params : DBCaml.Params.values) ~portal_name =
  let bind_buffer = Buffer.create 0 in

  (* We're both sending and recieving data as binary *)
  let formats = [1] in

  Buffer.add_char bind_buffer 'B';
  Buffer_tools.put_length_prefixed bind_buffer (fun buf ->
      (* Add the portal_name and a null terminator *)
      Buffer.add_string buf portal_name;
      Buffer.add_int8 buf 0;

      (* Add the statement id and a null terminator *)
      Buffer.add_string buf statement_id;
      Buffer.add_int8 buf 0;

      (* Add the amount formats and each format. A format is either text(0) or binary(1) *)
      Buffer.add_int16_be buf (List.length formats);
      List.iter (fun format -> Buffer.add_int16_be buf format) formats;

      (* Add the params length and all the values *)
      Buffer.add_int16_be buf (DBCaml.Params.length params);
      DBCaml.Params.iter
        { iter = (fun param -> encode_value param |> Buffer.add_buffer buf) }
        params;

      (* Add result formats *)
      Buffer.add_int16_be buf (List.length formats);
      List.iter (fun format -> Buffer.add_int16_be buf format) formats);

  bind_buffer
