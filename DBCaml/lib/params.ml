(* | LIST : 'a t -> 'a list t *)
(* | REAL : float t *)
(* | BOOLEAN : bool t *)
type 'a t =
  | NULLABLE : 'a t -> 'a option t
  | TEXT : string t
  | INTEGER : int t
  | FLOAT : float t
  | BOOLEAN : bool t

(* | CONST_STATIC : 'a * 'a t -> 'a value *)
(* | FIELD : 'a field -> 'a value *)
(* | COERCETO : 'a value * 'b t -> 'b value *)
(* | REF : string * 'a t -> 'a value *)
(* | AS : 'a value * string -> 'a value *)
type 'a value =
  | NULL : 'a option t -> 'a option value
  | CONST : 'a * 'a t -> 'a value

module Values = struct
  type 'a t =
    | [] : unit t
    | ( :: ) : ('a value * 'b t) -> ('a * 'b) t

  type iter = { iter: 'a. 'a value -> unit }

  let iter (iterator : iter) (l : 'b t) : unit =
    let rec aux : type a. a t -> unit = function
      | [] -> ()
      | x :: xs ->
        iterator.iter x;
        aux xs
    in
    aux l

  type 'b map = { map: 'a. 'a value -> 'b }

  let map (mapper : 'b map) (l : 'a t) : 'b list =
    let rec aux : type a. a t -> 'b list =
     fun l ->
      match l with
      | [] -> List.[]
      | x :: xs ->
        let rest = aux xs in
        List.cons (mapper.map x) rest
    in
    aux l

  (* let map (type a b) (i : a iter) (l : b t) : a list = *)
  (*   let rec aux type (l : a t) = *)
  (*     match l with *)
  (*     | [] -> Stdlib.[] *)
  (*     | [x] -> Stdlib.[i.iter x] *)
  (*     | x :: xs -> *)
  (*       let value = i.iter x in *)
  (*       let rest = aux xs in *)
  (*       Stdlib.([value] @ rest) *)
  (*   in *)
  (*   aux l *)

  type iteri = { iter: 'a. int -> 'a value -> unit }

  let iteri (iterator : iteri) (l : 'b t) : unit =
    let rec aux : type a. int -> a t -> unit =
     fun idx l ->
      match l with
      | [] -> ()
      | x :: xs ->
        iterator.iter idx x;
        aux (idx + 1) xs
    in
    aux 0 l

  let length l =
    let rec aux : type a. a t -> int -> int =
     fun l i ->
      match l with
      | [] -> i
      | _ :: xs -> aux xs (i + 1)
    in

    aux l 0

  let value ~ty i = CONST (i, ty)

  let option ~ty v = CONST (v, NULLABLE ty)

  let integer i = CONST (i, INTEGER)

  let integer_opt i = option ~ty:INTEGER i

  let text s = CONST (s, TEXT)

  let text_opt s = option ~ty:TEXT s

  let float f = CONST (f, FLOAT)

  let boolean b = CONST (b, BOOLEAN)
end

type values = VALUES : 'a Values.t -> values

let empty = VALUES []

let make v = VALUES v

let length (VALUES l) = Values.length l

let iter f (VALUES l) = Values.iter f l

let iteri f (VALUES l) = Values.iteri f l

let map f (VALUES l) = Values.map f l
