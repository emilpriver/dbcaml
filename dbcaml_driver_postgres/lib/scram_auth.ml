let ( let* ) = Result.bind

open Messages

let scram_hi ~data ~salt ~iterations =
  Pbkdf.pbkdf2
    ~prf:`SHA256
    ~salt
    ~password:data
    ~count:iterations
    ~dk_len:(Int32.of_int 32)

let scram_hmac ~key ~text =
  Digestif.SHA256.hmac_string ~key text |> Digestif.SHA256.to_raw_string

let scram_h ~text =
  Digestif.SHA256.digest_string text |> Digestif.SHA256.to_raw_string

let xor ~a ~b = Mirage_crypto.Uncommon.xor a b |> Base64.encode_exn

let base64_encode input =
  Cryptokit.transform_string (Cryptokit.Base64.encode_compact ()) input

let parse_payload ~payload_str =
  let parts = String.split_on_char ',' payload_str in
  List.fold_left
    (fun acc part ->
      match String.split_on_char '=' part with
      | k :: v :: xs -> (k, v ^ String.make (List.length xs) '=') :: acc
      | _ ->
        failwith (Printf.sprintf "split should have at least 2 parts: %s" part))
    []
    parts

let verify_server_proof ~server_key ~auth_message ~verifier =
  let server_signature = scram_hmac ~key:server_key ~text:auth_message in
  let decoded_verifier = Base64.decode_exn verifier in
  server_signature = decoded_verifier

(** 
  Authenticate using SASL.
  Ref: https://www.postgresql.org/docs/current/sasl-authentication.html
*)
let authenticate ~conn ~is_plus ~username ~password =
  (* Create initial message *)
  let nonce = Nonce.generate () |> base64_encode in
  let channel_binding = "n,," in
  let first_bare = Printf.sprintf "n=%s,r=%s" username nonce in
  let buf = Buffer.create 128 in

  Buffer.add_char buf 'p';
  let response =
    Bytes.of_string (Printf.sprintf "%s%s" channel_binding first_bare)
  in

  let mechanism =
    if is_plus then
      "SCRAM-SHA-256-PLUS"
    else
      "SCRAM-SHA-256"
  in

  Buffer_tools.put_length_prefixed buf (fun buf ->
      Buffer_tools.put_str_null buf mechanism;
      Buffer.add_int32_be buf (Int32.of_int (Bytes.length response));
      Buffer.add_bytes buf response);

  Pg_logger.debug
    (Printf.sprintf "Sending initial %s message to server" mechanism);

  let* _ = Pg.send conn ~buffer:buf in
  let* (_, _, _size, server_first_message) = Pg.receive conn in
  (* The server_first_message comes with type and length of the total message which is not information we really need so we offset the bytes with 9 *)
  let server_first_message =
    Bytes.sub server_first_message 9 (Bytes.length server_first_message - 9)
    |> Bytes.to_string
  in

  let parsed_payload = parse_payload ~payload_str:server_first_message in
  let iterations = int_of_string (List.assoc "i" parsed_payload) in
  let salt = List.assoc "s" parsed_payload in
  let server_nonce = List.assoc "r" parsed_payload in
  let without_proof =
    Printf.sprintf "c=%s,r=%s" (base64_encode channel_binding) server_nonce
  in

  let salt = Base64.decode_exn salt in
  let salted_password = scram_hi ~data:password ~salt ~iterations in
  let client_key = scram_hmac ~key:salted_password ~text:"Client Key" in
  let stored_key = scram_h ~text:client_key in
  let auth_message =
    String.concat "," [first_bare; server_first_message; without_proof]
  in

  let client_signature = scram_hmac ~key:stored_key ~text:auth_message in
  let client_proof =
    Printf.sprintf "p=%s" (xor ~a:client_key ~b:client_signature)
  in
  let client_final = String.concat "," [without_proof; client_proof] in

  let buf = Buffer.create 128 in
  Buffer.add_char buf 'p';
  Buffer.add_int32_be buf (Int32.of_int (String.length client_final + 4));
  Buffer.add_string buf client_final;

  Pg_logger.debug "Sending final SCRAM-SHA-256 message to server";

  let* _ = Pg.send conn ~buffer:buf in
  let* (_, _, _size, message) = Pg.receive conn in

  let message = Bytes.to_string message in
  (* The server_first_message comes with type and length of the total message which is not information we really need so we offset the bytes with 9 *)
  let message =
    String.sub message 9 (String.length message - 9)
    |> String.split_on_char ','
    |> List.hd
  in

  let parsed_payload = parse_payload ~payload_str:message in
  let verifier = List.assoc "v" parsed_payload in
  let salted_password = scram_hi ~data:password ~salt ~iterations in
  let server_key = scram_hmac ~key:salted_password ~text:"Server Key" in
  let auth_message =
    String.concat "," [first_bare; server_first_message; without_proof]
  in

  match verify_server_proof ~server_key ~auth_message ~verifier with
  | true ->
    Pg_logger.debug "SCRAM authentication successful";
    Ok ()
  | false -> Error (`msg "Server proof verification failed")
