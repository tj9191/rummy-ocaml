(* src/firebase_client.ml *)

open Async_kernel

module Cfg  = Firebase_config
module T    = Types
module Http = Http_client

(* Extract our serialized T.state from the Firestore document JSON. *)
let parse_state_from_firestore (body : string) : T.state option =
  let open Yojson.Safe.Util in
  match Yojson.Safe.from_string body with
  | exception _ ->
      (* Body was not valid JSON (e.g. HTML error page); just ignore. *)
      None
  | json ->
      (* If this is an error response, we also safely bail out. *)
      (match member "error" json with
       | `Null ->
           (* Try to dig out fields.state.stringValue *)
           (match json
                  |> member "fields"
                  |> member "state"
                  |> member "stringValue"
            with
            | `String s -> T.deserialize_state s
            | _ -> None)
       | _ ->
           (* JSON had an "error" field – treat as no state. *)
           None)

(* Pull the current game state for the single fixed lobby. *)
let pull_state () : T.state option Deferred.t =
  let url = Cfg.document_url () in
  let%map body_str = Http.get url in
  parse_state_from_firestore body_str

(* Push (overwrite) the game state for the fixed lobby. *)
let push_state (st : T.state) : unit Deferred.t =
  let state_str = T.serialize_state st in

  let body_json =
    `Assoc [
      ("fields",
        `Assoc [
          ("state",
            `Assoc [ ("stringValue", `String state_str) ]
          )
        ])
    ]
    |> Yojson.Safe.to_string
  in

  let url = Cfg.document_url () in
  Http.patch url body_json