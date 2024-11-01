let ( let* ) = Result.bind

type authentication_information = {
  user: string;
  password: string;
  database: string;
  host: string;
  port: int;
  sslmode: bool;
}

(* Used to get the databse name from the url string. Without this would Uri.path add / to the beginning of the database  *)
let remove_first_letter_if_slash str =
  if String.starts_with ~prefix:"/" str then
    String.sub str 1 (String.length str - 1)
  else
    str

(* Create the connection_information object based on the connection string *)
let make ~conninfo =
  let uri = Uri.of_string conninfo in
  let user = Uri.user uri |> Option.value ~default:"" in
  let password = Uri.password uri |> Option.value ~default:"" in
  let database = Uri.path uri |> remove_first_letter_if_slash in
  let host = Uri.host uri |> Option.value ~default:"localhost" in
  let port = Uri.port uri |> Option.value ~default:5432 in
  let sslmode =
    Uri.get_query_param uri "sslmode"
    |> Option.value ~default:"disable"
    |> String.equal "disable"
    (* We only want sslmode to be false if sslmode is enabled and have value "disabled" *)
    |> not
  in

  { user; password; database; host; port; sslmode }
