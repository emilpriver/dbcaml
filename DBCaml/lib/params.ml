(** The available params a driver a driver need to support *)
type t =
  | String of string
  | Number of int
  | Float of float
  | Bool of bool
  | StringArray of string list
  | NumberArray of int list

(** String param *)
let string v = String v

(** Number param *)
let number v = Number v

(** Float param *)
let float v = Float v

(** Bool param *)
let bool v = Bool v

(** List of strings param  *)
let string_list v = StringArray v

(** List of numbers param *)
let number_list v = NumberArray v
