let ( let* ) = Result.bind

open Messages
module Params = DBCaml.Params

let params_to_oid (params : Params.values) =
  DBCaml.Params.map Params.Values.{ map = Oid.oid_of_type } params

let _base64_encode input =
  Cryptokit.transform_string (Cryptokit.Base64.encode_compact ()) input

(** Execute the query, params on the provided connection and returns a message which includes DataRowDescripion, DataRows and CommandComplete*)
let query ~conn ~query ~row_limit ~params =
  let sync_buffer = Sync.database () in
  let statement_id = "dbcaml_s_" ^ Nonce.random_string 20 in
  let portal_name = "dbcaml_p_" ^ Nonce.random_string 20 in
  let params_oid = params_to_oid params in
  let _ = List.map in
  let prepare_buffer =
    Messages.Prepare.prepare ~statement_id ~params_oid ~query
  in
  let describe_buffer = Describe.statement ~statement_id in
  (* We need the RowDescription message as this message gives us information about the columns for the statement *)
  Buffer.add_buffer prepare_buffer describe_buffer;
  Buffer.add_buffer prepare_buffer sync_buffer;
  (*Send the prepare query and expect a ParseComplete back*)
  let* _ = Pg.send conn ~buffer:prepare_buffer in
  let* describe_response = Parse_message.wait_for_response conn in

  let bind_buffer = Bind.bind ~statement_id ~params ~portal_name in
  let execute_buffer = Execute.execute ~portal_name ~row_limit in
  let close_portal_buffer = Portal.close ~portal_name in

  let buffer = Buffer.create 1024 in
  Buffer.add_buffer buffer bind_buffer;
  Buffer.add_buffer buffer execute_buffer;
  Buffer.add_buffer buffer close_portal_buffer;
  Buffer.add_buffer buffer sync_buffer;

  let* _ = Pg.send conn ~buffer in
  let* execute_buffer = Parse_message.wait_for_response conn in

  let response = Bytes.concat Bytes.empty [describe_response; execute_buffer] in

  Ok response
