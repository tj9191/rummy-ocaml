open! Core
open Js_of_ocaml

module RE = Rummy_engine
module T = RE.Types

type game_state = T.state

let firestore_doc_url =
  "https://firestore.googleapis.com/v1/projects/rummy-ocaml/databases/(default)/documents/rummy/n9HZyYuqk9cUnEvEDcz5?updateMask.fieldPaths=state"

(* ------------------------------------------------------------ *)
(* SAVE STATE TO FIRESTORE (stub)                               *)
(* ------------------------------------------------------------ *)

let save_state (_st : game_state) : unit =
  Firebug.console##log
    (Js.string "[Firestore] save_state stub called (no-op).")

(* ------------------------------------------------------------ *)
(* LOAD STATE FROM FIRESTORE                                    *)
(* ------------------------------------------------------------ *)

let load_state (on_result : game_state option -> unit) : unit =
  let xhr = XmlHttpRequest.create () in
  xhr##_open
    (Js.string "GET")
    (Js.string firestore_doc_url)
    Js._true;

  xhr##.onreadystatechange :=
    Js.wrap_callback (fun () ->
        match xhr##.readyState with
        | XmlHttpRequest.DONE ->
            let code = xhr##.status in
            let response_text =
              Js.Opt.case xhr##.responseText
                (fun () -> "")
                Js.to_string
            in
            if code >= 200 && code < 300 then (
              (* Parse Firestore doc JSON and decode T.state *)
              (try
                 let doc_json = Yojson.Safe.from_string response_text in
                 let open Yojson.Safe.Util in
                 let state_string =
                   doc_json
                   |> member "fields"
                   |> member "state"
                   |> member "stringValue"
                   |> to_string
                 in
                 let state_json = Yojson.Safe.from_string state_string in
                 let st = T.state_of_yojson state_json in
                 Firebug.console##log
                   (Js.string "[Firestore] load_state OK (decoded).");
                 on_result (Some st)
               with exn ->
                 Firebug.console##log
                   (Js.string
                      ("[Firestore] exception while decoding: "
                       ^ Exn.to_string exn));
                 on_result None)
            ) else (
              Firebug.console##log
                (Js.string
                   ("[Firestore] load_state ERROR "
                    ^ string_of_int code
                    ^ ": "
                    ^ response_text));
              on_result None
            )
        | _ -> ());
  xhr##send Js.null