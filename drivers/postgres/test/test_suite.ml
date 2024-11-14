(* This tests  equires*a running postgres database running
   If you run this tests locally is the easiest way to start the docker compose
*)

let () =
  Riot.run @@ fun () ->
  Alcotest.run
    "Postgres Driver tests"
    [("Queries", Queries.suite); ("Connection", Connection.suite)]
