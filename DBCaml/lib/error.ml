module RioError = struct
  type t = (Rio.io_error[@printer Rio.pp_err]) [@@deriving show]
end

module Connection = struct
  let pp_exn _ _ = ()

  type t =
    [ `closed
    | `connection_closed
    | `eof
    | `msg of string
    | `no_info
    | `noop
    | `process_down
    | `timeout
    | `would_block
    | RioError.t
    | `Exit of exn
    ]
  [@@deriving show]
end

module Execution = struct
  type t =
    [ `execution_error of string
    | `no_rows
    | `general_error of string
    | `fatal_error of string
    | `bad_response of string
    ]
  [@@deriving show]
end

(* KEKW leandro names it "error" but only makes pp_err.... salt my hammies *)
module SerdeError = struct
  type t = (Serde.error[@printer Serde.pp_err]) [@@deriving show]
end

type t =
  [ SerdeError.t
  | Execution.t
  | Connection.t
  | `Supervisor_error
  ]
[@@deriving show]
