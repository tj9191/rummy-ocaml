open! Core
open! Bonsai
open! Bonsai_web

module Vdom  = Virtual_dom.Vdom
module RE    = Rummy_engine
module T     = RE.Types
module E     = RE.Engine
module Setup = Rummy_engine.Setup

(* ---- helpers to write example hands ---- *)
let h r = { T.suit = Hearts;   rank = r }
let s r = { T.suit = Spades;   rank = r }
let d r = { T.suit = Diamonds; rank = r }
let c r = { T.suit = Clubs;    rank = r }

(* card equality for filtering from full deck *)
let card_equal (a : T.card) (b : T.card) =
  Base.Poly.(a.suit = b.suit && a.rank = b.rank)

let string_of_card (c : T.card) = RE.Types.string_of_card c

let string_of_phase = function
  | T.Draw     -> "Draw"
  | T.Play     -> "Play"
  | T.Discard  -> "Discard"
  | T.EndCheck -> "EndCheck"

let view_cards (cards : T.card list) =
  Vdom.Node.ul
    (List.map cards ~f:(fun c ->
       Vdom.Node.li [ Vdom.Node.text (string_of_card c) ]))

let view_discard (discard : T.card list) =
  (* Show the whole pile, top -> bottom, with indices *)
  let items =
    List.mapi discard ~f:(fun i c ->
      let label =
        if i = 0
        then Printf.sprintf "top: %s" (string_of_card c)
        else Printf.sprintf "%d) %s" i (string_of_card c)
      in
      Vdom.Node.li [ Vdom.Node.text label ])
  in
  Vdom.Node.div
    [ Vdom.Node.h3 [ Vdom.Node.text "Discard pile (top → bottom)" ]
    ; Vdom.Node.ul items
    ]

(* initial two-player state; set vs_computer to name player 2 accordingly *)
let initial_state ~vs_computer () : T.state =
  let p0 =
    { T.id = 0
    ; name = "Player 1"
    ; hand = [ h T.Seven; h T.Eight; c T.Nine; d T.King; s T.Ace; d T.Three; c T.Three ]
    }
  in
  let p1_name = if vs_computer then "Computer" else "Player 2" in
  let p1 =
    { T.id = 1
    ; name = p1_name
    ; hand = [ c T.Five; c T.Six; h T.Queen; d T.Queen; s T.Two; d T.Nine; s T.Jack ]
    }
  in
  let used_cards = p0.hand @ p1.hand in
  let deck_remaining =
    Setup.all_cards
    |> List.filter ~f:(fun c -> not (List.exists used_cards ~f:(card_equal c)))
    |> Setup.shuffle
  in
  { T.deck            = deck_remaining
  ; T.discard         = []
  ; T.melds           = []
  ; T.players         = [| p0; p1 |]
  ; T.current         = 0
  ; T.phase           = T.Draw
  ; T.ace_policy      = T.Low
  ; T.scoring         = { T.deadwood_threshold = None }
  ; T.required_to_use = None
  ; T.scores          = Array.create ~len:2 0
  }

(* small helpers for parsing UI text inputs (space-separated 1-based indices) *)
let parse_indices (s : string) : int list =
  s
  |> String.split ~on:' '
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.filter_map ~f:(fun t -> Option.try_with (fun () -> Int.of_string t))
  |> List.map ~f:(fun k -> k - 1)

let nth i xs =
  let rec go k = function
    | [] -> None
    | x :: tl -> if k = 0 then Some x else go (k - 1) tl
  in
  if i < 0 then None else go i xs

let cards_by_indices (hand : T.card list) (idxs0 : int list) : T.card list option =
  let rec gather acc = function
    | [] -> Some (List.rev acc)
    | i :: is ->
        (match nth i hand with
         | None -> None
         | Some c -> gather (c :: acc) is)
  in
  gather [] idxs0

(* ========================================================= *)
(* Component *)
(* ========================================================= *)

let component graph =
  (* screen + vs_comp flag + game state + UI text inputs + history *)
  let state =
    Bonsai.state
      (`Intro,
       false (* vs_comp *),
       initial_state ~vs_computer:false (),
       "" (* draw N *),
       "" (* set indices *),
       "" (* run indices *),
       "" (* lay card idx *),
       "" (* lay meld idx *),
       "" (* discard idx *),
       ([] : T.state list)  (* history: top = head *)
      )
      graph
  in

  Value.map state ~f:(fun ((screen, vs_comp, st, drawN, set_s, run_s, lay_c, lay_m, disc_i, hist), set_all) ->
    (* Small helper to update just state and history without touching other fields *)
    let set_all' ~st':st1 ~hist':hist1 =
      set_all (screen, vs_comp, st1, drawN, set_s, run_s, lay_c, lay_m, disc_i, hist1)
    in

    let set_screen scr = set_all (scr, vs_comp, st, drawN, set_s, run_s, lay_c, lay_m, disc_i, hist) in
    let set_vs b       = set_all (screen, b,     st, drawN, set_s, run_s, lay_c, lay_m, disc_i, hist) in
    let set_state st'  = set_all' ~st':st' ~hist':hist in
    let set_drawN s    = set_all (screen, vs_comp, st, s, set_s, run_s, lay_c, lay_m, disc_i, hist) in
    let set_set_s s    = set_all (screen, vs_comp, st, drawN, s, run_s, lay_c, lay_m, disc_i, hist) in
    let set_run_s s    = set_all (screen, vs_comp, st, drawN, set_s, s, lay_c, lay_m, disc_i, hist) in
    let set_lay_c s    = set_all (screen, vs_comp, st, drawN, set_s, run_s, s, lay_m, disc_i, hist) in
    let set_lay_m s    = set_all (screen, vs_comp, st, drawN, set_s, run_s, lay_c, s, disc_i, hist) in
    let set_disc_i s   = set_all (screen, vs_comp, st, drawN, set_s, run_s, lay_c, lay_m, s, hist) in

    (* Undo/back: restore previous state from history and clear inputs *)
    let on_back _ev =
      match hist with
      | prev :: rest ->
          set_all (screen, vs_comp, prev, "", "", "", "", "", "", rest)
      | [] -> Ui_effect.Ignore
    in

    match screen with
    | `Intro ->
      let start_pass_and_play _ev =
        Ui_effect.Many
          [ set_vs false
          ; set_all' ~st':(initial_state ~vs_computer:false ()) ~hist':[]
          ; set_screen `Playing
          ]
      in
      let start_vs_computer _ev =
        Ui_effect.Many
          [ set_vs true
          ; set_all' ~st':(initial_state ~vs_computer:true ()) ~hist':[]
          ; set_screen `Playing
          ]
      in
      Vdom.Node.div
        [ Vdom.Node.h1 [ Vdom.Node.text "Welcome to Rummy!" ]
        ; Vdom.Node.p  [ Vdom.Node.text "Choose a mode to begin:" ]
        ; Vdom.Node.div
            [ Vdom.Node.button
                ~attrs:[ Vdom.Attr.on_click start_pass_and_play ]
                [ Vdom.Node.text "Pass & Play" ]
            ; Vdom.Node.button
                ~attrs:[ Vdom.Attr.on_click start_vs_computer
                       ; Vdom.Attr.style (Css_gen.margin_left (`Px 10)) ]
                [ Vdom.Node.text "Play vs Computer" ]
            ]
        ]

    | `Playing ->
      (* shared handler: IMPORTANT — push to history *)
      let on_end_check _ev =
        match E.endcheck st with
        | Ok st' | End_round st' -> set_all' ~st':st' ~hist':(st :: hist)
        | Error _ -> Ui_effect.Ignore
      in

      (* ------- controls conditional on phase ------- *)
      let controls : Vdom.Node.t list =
        match st.phase with
        | T.Draw ->
          let on_draw_deck _ =
            match E.draw ~source:T.FromDeck st with
            | Ok st' | End_round st' -> set_all' ~st':st' ~hist':(st :: hist)
            | Error _ -> Ui_effect.Ignore
          in
          let on_draw_discard_top _ =
            match E.draw ~source:T.FromDiscard st with
            | Ok st' | End_round st' -> set_all' ~st':st' ~hist':(st :: hist)
            | Error _ -> Ui_effect.Ignore
          in
          let on_draw_discard_n _ =
            let n =
              match Int.of_string_opt (String.strip drawN) with
              | Some k when k > 0 -> k
              | _ -> 1
            in
            match E.draw ~source:(T.FromDiscardN n) st with
            | Ok st' | End_round st' -> set_all' ~st':st' ~hist':(st :: hist)
            | Error _ -> Ui_effect.Ignore
          in
          [ Vdom.Node.h3 [ Vdom.Node.text "Draw phase" ]
          ; Vdom.Node.div
              [ Vdom.Node.button ~attrs:[ Vdom.Attr.on_click on_draw_deck ]
                  [ Vdom.Node.text "Draw from deck" ]
              ; Vdom.Node.button
                  ~attrs:[ Vdom.Attr.on_click on_draw_discard_top
                         ; Vdom.Attr.style (Css_gen.margin_left (`Px 8)) ]
                  [ Vdom.Node.text "Draw top of discard" ]
              ]
          ; Vdom.Node.div
              [ Vdom.Node.input
                  ~attrs:[ Vdom.Attr.type_ "number"
                         ; Vdom.Attr.placeholder "N"
                         ; Vdom.Attr.value drawN
                         ; Vdom.Attr.min 1.
                         ; Vdom.Attr.on_input (fun _ev s -> set_drawN s)
                         ; Vdom.Attr.style (Css_gen.width (`Px 80)) ]
                  ()
              ; Vdom.Node.button
                  ~attrs:[ Vdom.Attr.on_click on_draw_discard_n
                         ; Vdom.Attr.style (Css_gen.margin_left (`Px 8)) ]
                  [ Vdom.Node.text "Draw N from discard" ]
              ]
          ]

        | T.Play ->
          let p = st.players.(st.current) in
          let on_make_set _ =
            match cards_by_indices p.hand (parse_indices set_s) with
            | None -> Ui_effect.Ignore
            | Some cs ->
              (match E.play ~action:(T.Make_set cs) st with
               | Ok st' | End_round st' -> set_all' ~st':st' ~hist':(st :: hist)
               | Error _ -> Ui_effect.Ignore)
          in
          let on_make_run _ =
            match cards_by_indices p.hand (parse_indices run_s) with
            | None -> Ui_effect.Ignore
            | Some cs ->
              (match E.play ~action:(T.Make_run cs) st with
               | Ok st' | End_round st' -> set_all' ~st':st' ~hist':(st :: hist)
               | Error _ -> Ui_effect.Ignore)
          in
          let on_layoff _ =
            let ci_opt = Int.of_string_opt (String.strip lay_c) in
            let mi_opt = Int.of_string_opt (String.strip lay_m) in
            match ci_opt, mi_opt with
            | Some ci1, Some mi1 ->
              let ci0 = ci1 - 1 and mi0 = mi1 - 1 in
              (match nth ci0 p.hand with
               | None -> Ui_effect.Ignore
               | Some card ->
                 (match E.play ~action:(T.Lay_off (card, mi0)) st with
                  | Ok st' | End_round st' -> set_all' ~st':st' ~hist':(st :: hist)
                  | Error _ -> Ui_effect.Ignore))
            | _ -> Ui_effect.Ignore
          in
          let on_skip _ =
            match E.play ~action:T.Skip_to_discard st with
            | Ok st' | End_round st' -> set_all' ~st':st' ~hist':(st :: hist)
            | Error _ -> Ui_effect.Ignore
          in
          [ Vdom.Node.h3 [ Vdom.Node.text "Play phase" ]
          ; Vdom.Node.div
              [ Vdom.Node.input
                  ~attrs:[ Vdom.Attr.placeholder "set: indices e.g. 1 3 5"
                         ; Vdom.Attr.value set_s
                         ; Vdom.Attr.on_input (fun _ev s -> set_set_s s)
                         ; Vdom.Attr.style (Css_gen.width (`Px 240)) ]
                  ()
              ; Vdom.Node.button
                  ~attrs:[ Vdom.Attr.on_click on_make_set
                         ; Vdom.Attr.style (Css_gen.margin_left (`Px 8)) ]
                  [ Vdom.Node.text "Make set" ]
              ]
          ; Vdom.Node.div
              [ Vdom.Node.input
                  ~attrs:[ Vdom.Attr.placeholder "run: indices e.g. 2 3 4"
                         ; Vdom.Attr.value run_s
                         ; Vdom.Attr.on_input (fun _ev s -> set_run_s s)
                         ; Vdom.Attr.style (Css_gen.width (`Px 240)) ]
                  ()
              ; Vdom.Node.button
                  ~attrs:[ Vdom.Attr.on_click on_make_run
                         ; Vdom.Attr.style (Css_gen.margin_left (`Px 8)) ]
                  [ Vdom.Node.text "Make run" ]
              ]
          ; Vdom.Node.div
              [ Vdom.Node.input
                  ~attrs:[ Vdom.Attr.placeholder "layoff: card index"
                         ; Vdom.Attr.value lay_c
                         ; Vdom.Attr.on_input (fun _ev s -> set_lay_c s)
                         ; Vdom.Attr.style (Css_gen.width (`Px 140)) ]
                  ()
              ; Vdom.Node.input
                  ~attrs:[ Vdom.Attr.placeholder "on meld index"
                         ; Vdom.Attr.value lay_m
                         ; Vdom.Attr.on_input (fun _ev s -> set_lay_m s)
                         ; Vdom.Attr.style (Css_gen.concat [ Css_gen.width (`Px 140); Css_gen.margin_left (`Px 6) ]) ]
                  ()
              ; Vdom.Node.button
                  ~attrs:[ Vdom.Attr.on_click on_layoff
                         ; Vdom.Attr.style (Css_gen.margin_left (`Px 8)) ]
                  [ Vdom.Node.text "Layoff card on meld" ]
              ]
          ; Vdom.Node.button
              ~attrs:[ Vdom.Attr.on_click on_skip
                     ; Vdom.Attr.style (Css_gen.margin_top (`Px 8)) ]
              [ Vdom.Node.text "Skip to Discard" ]
          ]

        | T.Discard ->
          let p = st.players.(st.current) in
          let on_discard _ =
            let idx0 =
              match Int.of_string_opt (String.strip disc_i) with
              | Some k when k >= 1 -> k - 1
              | _ -> 0
            in
            match nth idx0 p.hand with
            | None -> Ui_effect.Ignore
            | Some card ->
              (match E.discard ~action:(T.Discard_card card) st with
               | Ok st' | End_round st' -> set_all' ~st':st' ~hist':(st :: hist)
               | Error _ -> Ui_effect.Ignore)
          in
          [ Vdom.Node.h3 [ Vdom.Node.text "Discard phase" ]
          ; Vdom.Node.div
              [ Vdom.Node.input
                  ~attrs:[ Vdom.Attr.placeholder "discard: card index"
                         ; Vdom.Attr.value disc_i
                         ; Vdom.Attr.on_input (fun _ev s -> set_disc_i s)
                         ; Vdom.Attr.style (Css_gen.width (`Px 160)) ]
                  ()
              ; Vdom.Node.button
                  ~attrs:[ Vdom.Attr.on_click on_discard
                         ; Vdom.Attr.style (Css_gen.margin_left (`Px 8)) ]
                  [ Vdom.Node.text "Discard" ]
              ]
          ]

        | T.EndCheck ->
          [ Vdom.Node.h3 [ Vdom.Node.text "End-of-turn checks" ]
          ; Vdom.Node.button
              ~attrs:[ Vdom.Attr.on_click on_end_check ]
              [ Vdom.Node.text "Continue / End check" ]
          ]
      in

      (* ------- main panel ------- *)
      let p = st.players.(st.current) in
      Vdom.Node.div
        [ Vdom.Node.h1 [ Vdom.Node.text "Rummy" ]
        ; Vdom.Node.p
            [ Vdom.Node.text
                (Printf.sprintf "Current: %s (phase: %s)"
                   p.name (string_of_phase st.phase)) ]
        ; Vdom.Node.p
            [ Vdom.Node.text
                (Printf.sprintf "Deck: %d   Discard: %d"
                   (List.length st.deck) (List.length st.discard)) ]
        ; Vdom.Node.h3 [ Vdom.Node.text (p.name ^ "'s hand") ]
        ; view_cards p.hand
        ; view_discard st.discard
        ; Vdom.Node.hr ()
        ; Vdom.Node.div controls
        ; (match hist with
           | _ :: _ ->
               Vdom.Node.button
                 ~attrs:[ Vdom.Attr.on_click on_back
                        ; Vdom.Attr.style (Css_gen.margin_top (`Px 10)) ]
                 [ Vdom.Node.text "⬅ Back" ]
           | [] -> Vdom.Node.none)
        ]
  )

let () =
  Bonsai_web.Start.start
    ~bind_to_element_with_id:"app"
    component