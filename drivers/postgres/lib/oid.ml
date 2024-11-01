open DBCaml.Params

(** Get the oid for each type we implement in DBCaml.Params *)
let oid_of_type = function
  | String _ -> 1043
  | Number _ -> 23
  | Float _ -> 700
  | Bool _ -> 16
  | StringArray _ -> 1015
  | NumberArray _ -> 1007
