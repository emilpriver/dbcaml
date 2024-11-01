open Message_format
module StringMap = Map.Make (String)

let ( let* ) = Result.bind

let get_message_parts ~message =
  let parts = String.split_on_char '\000' message in
  List.fold_left
    (fun acc part ->
      if String.length part > 1 then
        let head = String.get part 0 in
        let tail = String.sub part 1 (String.length part - 1) in
        (head, tail) :: acc
      else
        acc)
    []
    parts

(* Read the response from Postgres and return the message and the buffer after the message *)
let read_message buf =
  let size = Bytes.get_int32_be buf 1 in
  let total_message_length = (Int32.unsigned_to_int size |> Option.get) + 1 in
  let message = Bytes.sub buf 0 total_message_length in

  let new_buffer =
    Bytes.sub buf total_message_length (Bytes.length buf - total_message_length)
  in

  Ok (message, new_buffer)

(* parse_response reads the message and filter which messages to keep. Some of the messages such as CloseComplete are we not
   not interested in but we want the data in RowDescription for instance *)
let rec parse_response acc message =
  (* FIXME: we need to parse messages until we have a C. Seems as we don't read all the data we need from the socket *)
  let message_type =
    match from_u8 (Bytes.get message 0) with
    | Ok v -> v
    | Error e -> raise (Error e)
  in
  match message_type with
  | DataRow
  | CommandComplete
  | RowDescription ->
    let* (message, new_buffer) = read_message message in
    if Bytes.length new_buffer > 0 then
      parse_response (acc @ [message]) new_buffer
    else
      Ok acc
  | ReadyForQuery
  | ParseComplete
  | BindComplete
  | NoData
  | EmptyQueryResponse
  | PortalSuspended
  | CloseComplete
  | ParameterDescription ->
    let* (_message, new_buffer) = read_message message in
    if Bytes.length new_buffer > 0 then
      parse_response acc new_buffer
    else
      Ok acc
  | ErrorResponse ->
    let parse_error_message =
      get_message_parts ~message:(String.of_bytes message)
    in
    let error_message = List.assoc 'M' parse_error_message in
    Error (`msg error_message)
  | mt ->
    Error
      (`msg
        (Printf.sprintf
           "unexpected message_type: %S"
           (Message_format.to_string ~format:mt)))

(* Wait for the response from database and then parse it *)
let wait_for_response conn =
  let* (_, _message_type, _size, message) = Pg.receive conn in
  Pg_logger.warn
    (Format.sprintf
       "Received message: %s with length: %d"
       (Bytes.to_string message)
       (Int32.to_int _size));

  let* messages = parse_response [] message in

  Ok (Bytes.concat Bytes.empty messages)
