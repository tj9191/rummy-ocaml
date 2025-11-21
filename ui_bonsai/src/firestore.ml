open! Core
open Js_of_ocaml
open Async_kernel

module RE = Rummy_engine
module T  = RE.Types

type game_state = T.state

(* ------------------------------------------------------------ *)
(* FIRESTORE DOC URL                                            *)
(* ------------------------------------------------------------ *)

let firestore_doc_url_base =
  "https://firestore.googleapis.com/v1/projects/rummy-ocaml/databases/(default)/documents/rummy/n9HZyYuqk9cUnEvEDcz5"

let firestore_get_url   = firestore_doc_url_base
let firestore_patch_url = firestore_doc_url_base

(* ------------------------------------------------------------ *)
(* SAVE STATE TO FIRESTORE (PATCH)                              *)
(* ------------------------------------------------------------ *)

let save_state (st : game_state) : unit =
  let state_json_string = T.serialize_state st in

  let firestore_body =
    `Assoc [
      ("fields",
       `Assoc [
         ("state",
          `Assoc [ ("stringValue", `String state_json_string) ])
       ])
    ]
    |> Yojson.Safe.to_string
  in

  let xhr = XmlHttpRequest.create () in
  xhr##_open (Js.string "PATCH")
             (Js.string firestore_patch_url)
             Js._true;

  xhr##setRequestHeader
    (Js.string "Content-Type")
    (Js.string "application/json");

  xhr##.onreadystatechange :=
    Js.wrap_callback (fun () ->
      match xhr##.readyState with
      | XmlHttpRequest.DONE ->
        let code = xhr##.status in
        let body =
          Js.Opt.case xhr##.responseText (fun () -> "") Js.to_string
        in
        if code >= 200 && code < 300 then
          Firebug.console##log (Js.string "[Firestore] save_state OK.")
        else
          Firebug.console##log
            (Js.string
               ("[Firestore] save_state ERROR "
                ^ string_of_int code ^ ": " ^ body))
      | _ -> ()
    );

  xhr##send (Js.some (Js.string firestore_body))

(* ------------------------------------------------------------ *)
(* LOAD STATE (REST GET)                                        *)
(* ------------------------------------------------------------ *)

let load_state (on_result : game_state option -> unit) : unit =
  let xhr = XmlHttpRequest.create () in
  xhr##_open (Js.string "GET")
             (Js.string firestore_get_url)
             Js._true;

  xhr##.onreadystatechange :=
    Js.wrap_callback (fun () ->
      match xhr##.readyState with
      | XmlHttpRequest.DONE ->
        let code = xhr##.status in
        let body =
          Js.Opt.case xhr##.responseText (fun () -> "") Js.to_string
        in
        if code >= 200 && code < 300 then (
          try
            let json = Yojson.Safe.from_string body in
            let open Yojson.Safe.Util in
            let state_string =
              json |> member "fields"
                   |> member "state"
                   |> member "stringValue"
                   |> to_string
            in
            match T.deserialize_state state_string with
            | Some st -> on_result (Some st)
            | None -> on_result None
          with _ ->
            on_result None
        ) else
          on_result None
      | _ -> ()
    );

  xhr##send Js.null

let pull_state () : game_state option Deferred.t =
  let iv = Ivar.create () in
  load_state (fun st_opt ->
      if not (Ivar.is_full iv) then Ivar.fill_exn iv st_opt);
  Ivar.read iv

(* ------------------------------------------------------------ *)
(* PLAYER-2 JOINED FLAG                                         *)
(* ------------------------------------------------------------ *)

let mark_player2_joined () : unit =
  let body =
    `Assoc [
      ("fields",
       `Assoc [
         ("player2_joined",
          `Assoc [ ("booleanValue", `Bool true) ])
       ])
    ]
    |> Yojson.Safe.to_string
  in

  let xhr = XmlHttpRequest.create () in
  xhr##_open (Js.string "PATCH")
             (Js.string firestore_patch_url)
             Js._true;

  xhr##setRequestHeader
    (Js.string "Content-Type")
    (Js.string "application/json");

  xhr##.onreadystatechange :=
    Js.wrap_callback (fun () ->
      match xhr##.readyState with
      | XmlHttpRequest.DONE -> ()
      | _ -> ()
    );

  xhr##send (Js.some (Js.string body))

let has_player2_joined () : bool Deferred.t =
  let iv = Ivar.create () in

  let xhr = XmlHttpRequest.create () in
  xhr##_open (Js.string "GET")
             (Js.string firestore_get_url)
             Js._true;

  xhr##.onreadystatechange :=
    Js.wrap_callback (fun () ->
      match xhr##.readyState with
      | XmlHttpRequest.DONE ->
        let body =
          Js.Opt.case xhr##.responseText (fun () -> "") Js.to_string
        in
        let joined =
          try
            let json = Yojson.Safe.from_string body in
            match Yojson.Safe.Util.(json |> member "fields"
                                         |> member "player2_joined"
                                         |> member "booleanValue")
            with
            | `Bool b -> b
            | _ -> false
          with _ -> false
        in
        if not (Ivar.is_full iv) then Ivar.fill_exn iv joined
      | _ -> ()
    );

  xhr##send Js.null;
  Ivar.read iv

(* ------------------------------------------------------------ *)
(* REALTIME LISTENER (onSnapshot delivered by JS)               *)
(* ------------------------------------------------------------ *)

(** Call the JavaScript global function:
    startRummyListener(function(stateString){ ... })
*)
let js_start_rummy_listener (cb : Js.js_string Js.t -> unit) : unit =
  let cb_js = Js.wrap_callback cb in
  Js.Unsafe.(fun_call (pure_js_expr "startRummyListener") [| inject cb_js |])

(** Install real-time listener.  
    JS calls cb(state_json_string) whenever the Firestore
    document changes. *)
let start_realtime_listener (on_remote_state : game_state -> unit) : unit =
  js_start_rummy_listener (fun raw_js ->
      let raw = Js.to_string raw_js in
      match T.deserialize_state raw with
      | None ->
        Firebug.console##log
          (Js.string "[Firestore] listener: failed to decode remote state.")
      | Some st ->
        Firebug.console##log
          (Js.string "[Firestore] listener: received state.");
        on_remote_state st
    )