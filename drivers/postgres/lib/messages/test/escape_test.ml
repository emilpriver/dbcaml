open Messages.Bind

let test_escape_no_quotes () =
  Alcotest.(check string)
    "no quotes"
    "hello world"
    (escape_sql_value "hello world")

let test_escape_with_quotes () =
  Alcotest.(check string)
    "with quotes"
    "It''s a test"
    (escape_sql_value "It's a test")

let test_escape_empty_string () =
  Alcotest.(check string) "empty string" "" (escape_sql_value "")

let test_escape_special_chars () =
  Alcotest.(check string)
    "special chars"
    "Hello, ''world''!"
    (escape_sql_value "Hello, 'world'!")

(* Group the tests into a list *)
let test_suite =
  [
    ("escape no quotes", `Quick, test_escape_no_quotes);
    ("escape with quotes", `Quick, test_escape_with_quotes);
    ("escape empty string", `Quick, test_escape_empty_string);
    ("escape special chars", `Quick, test_escape_special_chars);
  ]

(* Entry point to run the tests *)
let () = Alcotest.run "SQL Escape Tests" [("Escape Tests", test_suite)]
