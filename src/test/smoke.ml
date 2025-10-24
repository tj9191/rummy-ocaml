open! Core
module T = Rummy_engine.Types

let%test _ =
  let _c : T.card = { suit = T.Clubs; rank = T.Ten } in
  true