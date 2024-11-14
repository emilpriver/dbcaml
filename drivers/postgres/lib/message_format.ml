let ( let* ) = Result.bind

(* Message format is the message types that Postgres send backs on queries *)
type message_format =
  | Authentication
  | BackendKeyData
  | BindComplete
  | CloseComplete
  | CommandComplete
  | CopyData
  | CopyDone
  | CopyInResponse
  | CopyOutResponse
  | DataRow
  | EmptyQueryResponse
  | ErrorResponse
  | NoData
  | NoticeResponse
  | NotificationResponse
  | ParameterDescription
  | ParameterStatus
  | ParseComplete
  | PortalSuspended
  | ReadyForQuery
  | RowDescription

type message = {
  format: message_format;
  contents: string;
}

exception Error of string

(** Return which message format it is based on char. This char is normally the first char in the message. Ex D for DataRow *)
let from_u8 (v : char) : (message_format, string) result =
  match v with
  | '1' -> Ok ParseComplete
  | '2' -> Ok BindComplete
  | '3' -> Ok CloseComplete
  | 'C' -> Ok CommandComplete
  | 'd' -> Ok CopyData
  | 'c' -> Ok CopyDone
  | 'G' -> Ok CopyInResponse
  | 'H' -> Ok CopyOutResponse
  | 'D' -> Ok DataRow
  | 'E' -> Ok ErrorResponse
  | 'I' -> Ok EmptyQueryResponse
  | 'A' -> Ok NotificationResponse
  | 'K' -> Ok BackendKeyData
  | 'N' -> Ok NoticeResponse
  | 'R' -> Ok Authentication
  | 'S' -> Ok ParameterStatus
  | 'T' -> Ok RowDescription
  | 'Z' -> Ok ReadyForQuery
  | 'n' -> Ok NoData
  | 's' -> Ok PortalSuspended
  | 't' -> Ok ParameterDescription
  | _ -> Error (Printf.sprintf "unknown message type: %c" v)

let to_string ~(format : message_format) : string =
  match format with
  | Authentication -> "Authentication"
  | BackendKeyData -> "BackendKeyData"
  | BindComplete -> "BindComplete"
  | CloseComplete -> "CloseComplete"
  | CommandComplete -> "CommandComplete"
  | CopyData -> "CopyData"
  | CopyDone -> "CopyDone"
  | CopyInResponse -> "CopyInResponse"
  | CopyOutResponse -> "CopyOutResponse"
  | DataRow -> "DataRow"
  | EmptyQueryResponse -> "EmptyQueryResponse"
  | ErrorResponse -> "ErrorResponse"
  | NoData -> "NoData"
  | NoticeResponse -> "NoticeResponse"
  | NotificationResponse -> "NotificationResponse"
  | ParameterDescription -> "ParameterDescription"
  | ParameterStatus -> "ParameterStatus"
  | ParseComplete -> "ParseComplete"
  | PortalSuspended -> "PortalSuspended"
  | ReadyForQuery -> "ReadyForQuery"
  | RowDescription -> "RowDescription"

let message buf =
  let message_type =
    if Bytes.length buf = 0 then raise (Error "empty message");

    match from_u8 (Bytes.get buf 0) with
    | Ok v -> v
    | Error e -> raise (Error e)
  in

  let size = Bytes.get_int32_be buf 1 in

  Ok (message_type, size, buf)
