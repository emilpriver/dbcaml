(** Generate a random nonce used for generating random statements and portals *)
let generate () =
  let count = Riot.Crypto.Random.int ~max:(64 - 28 + 28) () in

  let gen_char () =
    let rec loop () =
      let c = Random.int (0x7E - 0x21 + 1) in
      if c = 0x2C then
        loop ()
      else
        Char.chr c
    in
    loop ()
  in

  String.init count (fun _ -> gen_char ())

let random_string length =
  let chars = "abcdefghijklmnopqrstuvwxyz" in
  let chars_len = String.length chars in
  let result = Bytes.create length in

  for i = 0 to length - 1 do
    Bytes.set result i chars.[Random.int chars_len]
  done;

  Bytes.to_string result
