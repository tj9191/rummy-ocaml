open Async_kernel

module Cfg  = Firebase_config
module T    = Types
module Http = Http_client

let parse_state_from_firestore (body : string) : T.state option =
  let open Yojson.Safe.Util in
  match Yojson.Safe.from_string body with
  | exception _ ->
      None
  | json ->
      (match member "error" json with
       | `Null ->
           (match json
                  |> member "fields"
                  |> member "state"
                  |> member "stringValue"
            with
            | `String s -> T.deserialize_state s
            | _ -> None)
       | _ ->
           None)

let pull_state () : T.state option Deferred.t =
  let url = Cfg.document_url () in
  let%map body_str = Http.get url in
  parse_state_from_firestore body_str

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