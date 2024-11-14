module Bs = Bytestring
open Riot

let ( let* ) = Result.bind

type t =
  | Conn : {
      writer: 'socket IO.Writer.t;
      reader: 'socket IO.Reader.t;
      uri: Uri.t;
      addr: Net.Addr.stream_addr;
    }
      -> t

let send (Conn { writer; _ } as conn) ~buffer =
  let buf = Buffer.contents buffer in
  let* () = IO.write_all writer ~buf in
  Ok conn

let receive (Conn { reader; _ } as conn) =
  let* data = Bs.with_bytes (fun buf -> IO.read reader buf) in
  Pg_logger.debug (Format.sprintf "Received %d bytes" (Bs.length data));

  let* (message_type, size, message) =
    Message_format.message (Bs.to_string data |> Bytes.of_string)
  in

  Ok (conn, message_type, size, message)

let make_connection ~reader ~writer ~uri ~addr =
  Conn { writer; reader; uri; addr }

(* Create a TLS connection using certificates *)
module Auth = struct
  let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)

  let make_default authenticator =
    Tls.Config.client ~authenticator () |> Result.get_ok

  let default () =
    let time () = Some (Ptime_clock.now ()) in
    let decode_pem ca =
      let cert = X509.Certificate.decode_pem ca in
      Result.get_ok cert
    in
    let cas = List.map decode_pem Ca_store.certificates in
    let authenticator = X509.Authenticator.chain_of_trust ~time cas in
    make_default authenticator
end

(** 
    Create a new connection using the provided connection string. 
    Also tries to make a SSL upgrade if the user don't provide sslmode=disable as a query *)
let connect conninfo =
  let authentication_information = Authentication_information.make ~conninfo in

  let uri = Uri.of_string conninfo in
  let* addr = Riot.Net.Addr.of_uri uri in
  let* sock = Net.Tcp_stream.connect addr in

  let* host =
    let* domain_name = Domain_name.of_string authentication_information.host in
    Domain_name.host domain_name
  in

  let conn =
    match authentication_information.sslmode with
    | false ->
      let (reader, writer) = Net.Tcp_stream.(to_reader sock, to_writer sock) in
      make_connection ~reader ~writer ~addr ~uri
    | true ->
      let config = Auth.default () in
      let tls_sock = SSL.of_client_socket ~host ~config sock in
      let (reader, writer) = SSL.(to_reader tls_sock, to_writer tls_sock) in
      make_connection ~reader ~writer ~addr ~uri
  in

  Ok (conn, authentication_information)
