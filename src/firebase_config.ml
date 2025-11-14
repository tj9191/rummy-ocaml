(* src/firebase_config.ml *)

open Core

let project_id = "rummy-ocaml"

(* Name of the Firestore collection that holds all lobby documents. *)
let collection = "Rummy"

(* Base REST URL for Firestore documents in this project *)
let base_url =
  sprintf
    "https://firestore.googleapis.com/v1/projects/%s/databases/(default)/documents"
    project_id

(* Given a lobby_id (a string the players type, or that you generate),
   build the full document URL for that lobby. *)
let document_url ~(lobby_id : string) : string =
  (* This corresponds to: projects/.../documents/Rummy/{lobby_id} *)
  sprintf "%s/%s/%s" base_url collection lobby_id