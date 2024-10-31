let ( let* ) = Result.bind

let assert_ok = function
  | Sqlite3.Rc.OK -> ()
  | Sqlite3.Rc.DONE -> ()
  | rc ->
    failwith @@ Format.sprintf "SQLITE: Not OK! %s" (Sqlite3.Rc.to_string rc)

let param_to_sqlite (param : DBCaml.Params.t) : Sqlite3.Data.t =
  match param with
  | DBCaml.Params.String v -> Sqlite3.Data.TEXT v
  | DBCaml.Params.Number v -> Sqlite3.Data.INT (Int64.of_int v)
  | DBCaml.Params.Float v -> Sqlite3.Data.FLOAT v
  | DBCaml.Params.Bool v -> Sqlite3.Data.opt_bool (Some v)
  | DBCaml.Params.StringArray _ -> failwith "SQLITE: Not Supported!"
  | DBCaml.Params.NumberArray _ -> failwith "SQLITE: Not Supported!"

let data_to_string (data : Sqlite3.Data.t) : string option =
  Some (Sqlite3.Data.to_string_coerce data)

module Driver = struct
  module Store = struct
    type t = Yojson.Basic.t list ref

    let empty () : t = ref []

    let of_row (store : t) row headers =
      let record = ref [] in
      Array.iteri
        (fun idx -> function
          | Some s -> record := (headers.(idx), `String s) :: !record
          | None -> record := (headers.(idx), `Null) :: !record)
        row;
      store := `Assoc !record :: !store

    let to_string (store : t) = Yojson.Basic.to_string (`List !store)

    let to_response (store : t) =
      to_string store |> Bytes.of_string |> Result.ok
  end

  module Connection = struct
    type config = { conninfo: string }

    let collect_rows connection query =
      let store = Store.empty () in
      assert_ok @@ Sqlite3.exec connection query ~cb:(Store.of_row store);
      Store.to_response store

    let connect config =
      SqliteLogger.info "Connecting to database";
      let conn = Sqlite3.db_open config.conninfo in
      SqliteLogger.info "Conection complete";

      let query ~connection ~params ~query ~row_limit :
          (Bytes.t, DBCaml.Res.execution_error) result =
        SqliteLogger.info (Format.sprintf "Querying database: %s" query);
        let _ = row_limit in
        match params with
        | [] -> collect_rows connection query
        | _ ->
          let open Sqlite3 in
          let stmt = prepare connection query in
          List.iteri
            (fun idx param ->
              assert_ok @@ bind stmt (idx + 1) (param_to_sqlite param))
            params;
          (* Useful functions for later! *)
          (* (column_decltype stmt i |> Option.get) *)
          (* (column_name stmt i) *)
          (* (Data.to_string_coerce c)) *)
          let store = Store.empty () in
          assert_ok
          @@ iter stmt ~f:(fun row ->
                 let headers = row_names stmt in
                 let data = Array.map data_to_string row in
                 Store.of_row store data headers);

          Store.to_response store
      in

      (* Create a new connection which we also want to use to create a PID *)
      let* conn = DBCaml.Connection.make ~conn ~query () in

      Ok conn

    (* "Temporary" Hack: Just encode to JSON, and then decode from it later. It's fine *)
    let deserialize de buf = Serde_json.of_string de (Bytes.to_string buf)

    (* let get_rows_affected result = Ok (Sqlite3.changes result) *)
  end

  let connection conninfo =
    DBCaml.Driver.Driver { driver = (module Connection); config = { conninfo } }
end

let assert_ok rc = assert (rc = Sqlite3.Rc.OK)
