open DBCaml.Params

(** Get the oid for each type we implement in DBCaml.Params *)
let oid_of_type : 'a. 'a value -> int =
 fun (type a) (param : a value) ->
  match param with
  | CONST (_, TEXT) -> 1043
  | CONST (_, INTEGER) -> 23
  | CONST (_, FLOAT) -> 700
  | CONST (_, NULLABLE _) -> failwith "PIIIRRRRRIIIVVVVVV 2"
  | _ -> failwith "PIIIRRRRRIIIVVVVVV 3"
(* | Bool _ -> 16 *)
(* | StringArray _ -> 1015 *)
(* | NumberArray _ -> 1007 *)
