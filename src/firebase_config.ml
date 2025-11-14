(* src/firebase_config.ml *)

open Core

let project_id = "rummy-ocaml"

let collection = "rummy"

let base_url =
  sprintf
    "https://firestore.googleapis.com/v1/projects/%s/databases/(default)/documents"
    project_id

let document_id = "n9HZyYuqk9cUnEvEDcz5"

(* Full REST URL for that one document *)
let document_url () : string =
  sprintf "%s/%s/%s" base_url collection document_id