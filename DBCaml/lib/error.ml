module ConnectionError = struct
  type t =
    [ `connection_error of string
    | `authenication_error of string
    | `general_error of string
    ]
  [@@deriving show]
end

module ExecutionError = struct
  type t =
    [ `execution_error of string
    | `no_rows
    | `general_error of string
    | `fatal_error of string
    | `bad_response of string
    ]
  [@@deriving show]
end

module SerdeError = struct
  type t = (Serde.error[@printer Serde.pp_err]) [@@deriving show]
end

type t =
  [ SerdeError.t
  | ExecutionError.t
  | ConnectionError.t
  | `Supervisor_error
  ]
[@@deriving show]

type ('ok, 'a) or_error = ('ok, 'a) Stdlib.result constraint 'a = [< t ]

let bind :
      'a. ('a, 'b) or_error -> ('a -> ('c, 'b) or_error) -> ('c, 'b) or_error =
 fun r f ->
  match r with
  | Ok v -> f v
  | Error _ as e -> e
