open! Core
module S = Rummy_engine.Setup

let%expect_test "deck size is 52" =
  Printf.printf "%d\n" (List.length S.all_cards);
  [%expect {| 52 |}]