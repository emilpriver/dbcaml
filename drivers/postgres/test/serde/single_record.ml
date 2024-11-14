open Alcotest

type user = {
  id: int;
  some_int64: int64;
  some_int32: int32;
  some_float: float;
  some_bool: bool;
  pets: string list;
  pets_array: string array;
  pet_name: string option;
  name: string;
}
[@@deriving deserialize]

(*
TODO: add tests for:
1. a field within the record does not exist in the raw message. Should error
*)

let message =
  "T\000\000\000\244\000\tname\000\000\000@\005\000\002\000\000\004\019\255\255\000\000\001\003\000\000id\000\000\000@\005\000\001\000\000\000\023\000\004\255\255\255\255\000\000some_bool\000\000\000@\005\000\003\000\000\000\016\000\001\255\255\255\255\000\000pet_name\000\000\000@\005\000\b\000\000\004\019\255\255\000\000\001\003\000\000some_int64\000\000\000@\005\000\004\000\000\000\020\000\b\255\255\255\255\000\000some_int32\000\000\000@\005\000\005\000\000\000\023\000\004\255\255\255\255\000\000some_float\000\000\000@\005\000\006\000\000\002\189\000\b\255\255\255\255\000\000pets\000\000\000@\005\000\007\000\000\003\241\255\255\255\255\255\255\000\000pets_array\000\000\000@\005\000\007\000\000\003\241\255\255\255\255\255\255\000\000D\000\000\000\177\000\t\000\000\000\006Alice,\000\000\000\004\000\000\000\001\000\000\000\001\001\255\255\255\255\000\000\000\b\000\000\000\000\000\000\000\001\000\000\000\004\000\000\000\002\000\000\000\b?\241\153\153\153\153\153\154\000\000\0004\000\000\000\001\000\000\000\000\000\000\000\025\000\000\000\003\000\000\000\001\000\000\000\004Lucy\000\000\000\004Adam\000\000\000\012helllo,world\000\000\0004\000\000\000\001\000\000\000\000\000\000\000\025\000\000\000\003\000\000\000\001\000\000\000\004Lucy\000\000\000\004Adam\000\000\000\012helllo,worldD\000\000\000\147\000\t\000\000\000\003Bob\000\000\000\004\000\000\000\002\000\000\000\001\001\000\000\000\005Danza\000\000\000\b\000\000\000\000\000\000\000\001\000\000\000\004\000\000\000\002\000\000\000\b?\241\153\153\153\153\153\154\000\000\000$\000\000\000\001\000\000\000\000\000\000\000\025\000\000\000\002\000\000\000\001\000\000\000\004Lucy\000\000\000\004Adam\000\000\000$\000\000\000\001\000\000\000\000\000\000\000\025\000\000\000\002\000\000\000\001\000\000\000\004Lucy\000\000\000\004AdamC\000\000\000\rSELECT 2\000"
  |> Bytes.of_string

let test_record () =
  let u =
    match DBCamlPostgres.deserialize deserialize_user message with
    | Ok u -> u
    | Error e -> fail (Format.asprintf "Deserialize error: %a" Serde.pp_err e)
  in

  Alcotest.(check string)
    "Does deserialize and have the expected value"
    "Alice,"
    u.name;

  Alcotest.(check bool)
    "Does deserialize and have the expected value"
    true
    u.some_bool;

  Alcotest.(check (option string)) "Does deserialize options" None u.pet_name;

  Alcotest.(check (float 1e-2))
    "Does deserialize and have the expected value"
    1.1
    u.some_float;

  Alcotest.(check int32)
    "Does deserialize and have the expected value"
    (Int32.of_int 2)
    u.some_int32;

  Alcotest.(check int64)
    "Does deserialize and have the expected value"
    (Int64.of_int 1)
    u.some_int64;

  Alcotest.(check int) "Does deserialize and have the expected value" 1 u.id;

  Alcotest.(check int)
    "Does deserialize and have the expected value"
    3
    (Array.length u.pets_array);

  Alcotest.(check int)
    "Does deserialize and have the expected value"
    3
    (List.length u.pets)

let suite = [test_case "single_record" `Quick test_record]
