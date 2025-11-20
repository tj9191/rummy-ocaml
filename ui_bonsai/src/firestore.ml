open! Core
open Js_of_ocaml
open Async_kernel

module RE = Rummy_engine
module T = RE.Types

(* Our game state type *)
type game_state = T.state

(* Base document URL, no query params *)
let firestore_doc_url_base =
  "https://firestore.googleapis.com/v1/projects/rummy-ocaml/databases/(default)/documents/rummy/n9HZyYuqk9cUnEvEDcz5"

(* For GET: use this (no updateMask) *)
let firestore_get_url = firestore_doc_url_base

(* For PATCH: we can specify updateMask here if we want *)
let firestore_patch_url =
  firestore_doc_url_base ^ "?updateMask.fieldPaths=state"

(* ------------------------------------------------------------ *)
(* SAVE STATE TO FIRESTORE (real PATCH)                         *)
(* ------------------------------------------------------------ *)

let save_state (st : game_state) : unit =
  let state_json_string = T.serialize_state st in

  let firestore_body =
    `Assoc [
      ("fields",
       `Assoc [
         ("state",
          `Assoc [
            ("stringValue", `String state_json_string)
          ])
       ])
    ]
    |> Yojson.Safe.to_string
  in

  let xhr = XmlHttpRequest.create () in
  xhr##_open
    (Js.string "PATCH")
    (Js.string firestore_patch_url)
    Js._true;

  (* Required header for JSON PATCH *)
  xhr##setRequestHeader
    (Js.string "Content-Type")
    (Js.string "application/json");

  xhr##.onreadystatechange :=
    Js.wrap_callback (fun () ->
        match xhr##.readyState with
        | XmlHttpRequest.DONE ->
            let code = xhr##.status in
            let response_text =
              Js.Opt.case xhr##.responseText (fun () -> "") Js.to_string
            in
            if code >= 200 && code < 300 then
              Firebug.console##log (Js.string "[Firestore] save_state OK (patched).")
            else
              Firebug.console##log
                (Js.string
                   ("[Firestore] save_state ERROR "
                    ^ string_of_int code
                    ^ ": "
                    ^ response_text))
        | _ -> ()
      );

  xhr##send (Js.some (Js.string firestore_body))

(* ------------------------------------------------------------ *)
(* LOAD STATE FROM FIRESTORE (already fine)                     *)
(* ------------------------------------------------------------ *)

let load_state (on_result : game_state option -> unit) : unit =
  let xhr = XmlHttpRequest.create () in
    xhr##_open
    (Js.string "GET")
    (Js.string firestore_get_url)
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
              try
                let doc_json = Yojson.Safe.from_string response_text in
                let open Yojson.Safe.Util in
                let state_string =
                  doc_json
                  |> member "fields"
                  |> member "state"
                  |> member "stringValue"
                  |> to_string
                in
                match T.deserialize_state state_string with
                | Some st ->
                    Firebug.console##log
                      (Js.string "[Firestore] load_state OK (decoded).");
                    on_result (Some st)
                | None ->
                    Firebug.console##log
                      (Js.string "[Firestore] load_state: failed to deserialize state.");
                    on_result None
              with exn ->
                Firebug.console##log
                  (Js.string
                     ("[Firestore] exception while decoding: "
                      ^ Exn.to_string exn));
                on_result None
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

  let pull_state () : game_state option Deferred.t =
  let ivar = Ivar.create () in
  load_state (fun st_opt ->
      if not (Ivar.is_full ivar) then
        Ivar.fill ivar st_opt
    );
  Ivar.read ivar