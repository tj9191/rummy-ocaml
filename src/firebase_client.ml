(* src/firebase_client.ml *)

open Async_kernel

module Cfg  = Firebase_config
module T    = Types
module Http = Http_client

(* Extract our serialized T.state from the Firestore document JSON. *)
let parse_state_from_firestore (body : string) : T.state option =
  let open Yojson.Safe.Util in
  match Yojson.Safe.from_string body with
  | exception _ -> None
  | json ->
    (match json |> member "fields" |> member "state" |> member "stringValue" with
     | `String s -> T.deserialize_state s
     | _ -> None)

(* Pull the current game state for a given lobby. *)
let pull_state ~(lobby_id : string) () : T.state option Deferred.t =
  let url = Cfg.document_url ~lobby_id in
  let%map body_str = Http.get url in
  parse_state_from_firestore body_str

(* Push (overwrite) the game state for a given lobby. *)
let push_state ~(lobby_id : string) (st : T.state) : unit Deferred.t =
  let state_str = T.serialize_state st in
  let body_json =
    `Assoc
      [ ( "fields"
        , `Assoc
            [ ( "state"
              , `Assoc [ ("stringValue", `String state_str) ]
              )
            ]
        )
      ]
    |> Yojson.Safe.to_string
  in
  let url = Cfg.document_url ~lobby_id in
  Http.patch url body_json