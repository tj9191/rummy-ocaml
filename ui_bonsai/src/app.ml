open! Core
open! Bonsai
open! Bonsai_web

module Vdom  = Virtual_dom.Vdom
module RE    = Rummy_engine
module T     = RE.Types
module E     = RE.Engine
module Setup = Rummy_engine.Setup
module AI    = Rummy_engine.Ai
module Html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js

(* ---- helpers to write example hands ---- *)
let h r = { T.suit = Hearts;   rank = r }
let s r = { T.suit = Spades;   rank = r }
let d r = { T.suit = Diamonds; rank = r }
let c r = { T.suit = Clubs;    rank = r }

let string_of_card (c : T.card) = RE.Types.string_of_card c

let string_of_phase = function
  | T.Draw     -> "Draw"
  | T.Play     -> "Play"
  | T.Discard  -> "Discard"
  | T.EndCheck -> "EndCheck"

let card_point_value (r : T.rank) : int =
  match r with
  | T.Ace   -> 15
  | T.Two   -> 5
  | T.Three -> 5
  | T.Four  -> 5
  | T.Five  -> 5
  | T.Six   -> 5
  | T.Seven -> 5
  | T.Eight -> 5
  | T.Nine  -> 5
  | T.Ten   -> 10
  | T.Jack  -> 10
  | T.Queen -> 10
  | T.King  -> 10

(* compute total value of all cards currently on the table in melds *)
let current_table_points (melds : T.meld list) : int =
  List.sum (module Int) melds ~f:(fun m ->
    List.sum (module Int) m.cards ~f:(fun card ->
      card_point_value card.T.rank
    )
  )

(* view total meld points *)
let view_table_points (melds : T.meld list) =
  let pts = current_table_points melds in
  Vdom.Node.div
    [ Vdom.Node.h3 [ Vdom.Node.text "Meld value on table" ]
    ; Vdom.Node.p
        [ Vdom.Node.text
            (Printf.sprintf "%d points currently down" pts)
        ]
    ]

let view_cards (cards : T.card list) =
  Vdom.Node.ul
    (List.mapi cards ~f:(fun i c ->
       (* display 1-based index to match how you type moves *)
       let label =
         Printf.sprintf "%d) %s" (i + 1) (string_of_card c)
       in
       Vdom.Node.li [ Vdom.Node.text label ]))

let view_discard (discard : T.card list) =
  (* top of discard is head of list *)
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

(* show melds currently on the table *)
let view_melds (melds : T.meld list) =
  if List.is_empty melds then
    Vdom.Node.div
      [ Vdom.Node.h3 [ Vdom.Node.text "Melds on table" ]
      ; Vdom.Node.p  [ Vdom.Node.text "(none yet)" ]
      ]
  else
    let meld_nodes =
      List.mapi melds ~f:(fun i m ->
        let kind_str =
          match m.kind with
          | T.Set -> "Set"
          | T.Run -> "Run"
        in
        let cards_str =
          String.concat ~sep:", "
            (List.map m.cards ~f:string_of_card)
        in
        Vdom.Node.li
          [ Vdom.Node.text
              (Printf.sprintf "%d) %s: %s" (i + 1) kind_str cards_str)
          ])
    in
    Vdom.Node.div
      [ Vdom.Node.h3 [ Vdom.Node.text "Melds on table" ]
      ; Vdom.Node.ul meld_nodes
      ]

(* show current scores for each player *)
let view_scores (scores : int array) (players : T.player array) =
  let rows =
    Array.mapi players ~f:(fun i pl ->
      let label =
        Printf.sprintf "%s: %d points" pl.T.name scores.(i)
      in
      Vdom.Node.li [ Vdom.Node.text label ])
    |> Array.to_list
  in
  Vdom.Node.div
    [ Vdom.Node.h3 [ Vdom.Node.text "Scores" ]
    ; Vdom.Node.ul rows
    ]
  


(* build a starting 2p state; player 2 may be computer or human name *)
let initial_state ~vs_computer () : T.state =
  let p0 =
    { T.id = 0
    ; name = "Player 1"
    ; hand =
        [ h T.Seven
        ; h T.Eight
        ; c T.Nine
        ; d T.King
        ; s T.Ace
        ; d T.Three
        ; c T.Three
        ]
    }
  in
  let p1_name = if vs_computer then "Computer" else "Player 2" in
  let p1 =
    { T.id = 1
    ; name = p1_name
    ; hand =
        [ c T.Five
        ; c T.Six
        ; h T.Queen
        ; d T.Queen
        ; s T.Two
        ; d T.Nine
        ; s T.Jack
        ]
    }
  in
  (* remove those cards from the deck and shuffle the rest *)
  let used_cards = p0.hand @ p1.hand in
let card_equal (a : T.card) (b : T.card) =
  Base.Poly.(a.suit = b.suit && a.rank = b.rank)
  in
  let deck_remaining =
    Setup.all_cards
    |> List.filter ~f:(fun c ->
         not (List.exists used_cards ~f:(fun u -> card_equal u c)))
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

(* ----- helpers for parsing text input into indices ----- *)

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
(* Component with AI turn logic                              *)
(* ========================================================= *)

let component graph =
  (* persistent RNG for the computer policy *)
  let rng = Random.State.make [| 0xC0FFEE |] in

  (* Our full app model:
     - screen (`Intro or `Playing)
     - vs_comp (bool)
     - st (game state)
     - input boxes for the current human turn
     - hist: undo stack of past states *)
  let state =
    Bonsai.state
      (`Intro,
       false,
       initial_state ~vs_computer:false (),
       "" (* drawN *),
       "" (* set indices *),
       "" (* run indices *),
       "" (* lay card idx *),
       "" (* lay meld idx *),
       "" (* discard idx *),
       ([] : T.state list))
      graph
  in

  Bonsai.Value.map state
    ~f:(fun ((screen
             , vs_comp
             , st
             , drawN
             , set_s
             , run_s
             , lay_c
             , lay_m
             , disc_i
             , hist),
            set_all) ->
      (* ---------------- basic setters / input updaters ---------------- *)

      let set_all_full
            ~screen'
            ~vs_comp'
            ~st'
            ~drawN'
            ~set_s'
            ~run_s'
            ~lay_c'
            ~lay_m'
            ~disc_i'
            ~hist'
        =
        set_all
          ( screen'
          , vs_comp'
          , st'
          , drawN'
          , set_s'
          , run_s'
          , lay_c'
          , lay_m'
          , disc_i'
          , hist'
          )
      in

      let set_drawN s =
        set_all
          ( screen, vs_comp, st, s, set_s, run_s, lay_c, lay_m, disc_i, hist )
      in
      let set_set_s s =
        set_all
          ( screen, vs_comp, st, drawN, s, run_s, lay_c, lay_m, disc_i, hist )
      in
      let set_run_s s =
        set_all
          ( screen, vs_comp, st, drawN, set_s, s, lay_c, lay_m, disc_i, hist )
      in
      let set_lay_c s =
        set_all
          ( screen, vs_comp, st, drawN, set_s, run_s, s, lay_m, disc_i, hist )
      in
      let set_lay_m s =
        set_all
          ( screen, vs_comp, st, drawN, set_s, run_s, lay_c, s, disc_i, hist )
      in
      let set_disc_i s =
        set_all
          ( screen, vs_comp, st, drawN, set_s, run_s, lay_c, lay_m, s, hist )
      in

      let set_screen new_screen =
        set_all
          ( new_screen
          , vs_comp
          , st
          , drawN
          , set_s
          , run_s
          , lay_c
          , lay_m
          , disc_i
          , hist
          )
      in

      let set_vs b =
        set_all
          ( screen
          , b
          , st
          , drawN
          , set_s
          , run_s
          , lay_c
          , lay_m
          , disc_i
          , hist
          )
      in

      (* ---------------- computer turn loop ---------------- *)
      (* Let the AI (player 1) automatically play until done. *)

(* ---------------- computer turn loop ---------------- *)
      (* Let the AI (player 1) automatically play until done. *)
      (* Let the AI act up to N steps. After that, if it's STILL the computer's turn,
   just hand it back to player 0 so the UI never freezes. *)
let advance_ai_turn (st_start : T.state) : T.state =
  let max_steps = 20 in
  let rec loop steps st_ai =
    (* if it's already human's turn, we're done *)
    if st_ai.T.current = 0 then
      st_ai
    else if steps <= 0 then
      (* safety cutoff: force turn back to human so UI continues *)
      { st_ai with current = 0; phase = T.Draw }
    else
      match st_ai.T.phase with
      | T.EndCheck ->
        (match E.endcheck st_ai with
         | Ok st' | End_round st' ->
           if st'.T.current = 0
           then st'
           else loop (steps - 1) st'
         | Error _ ->
           (* couldn't endcheck; force turn to human *)
           { st_ai with current = 0; phase = T.Draw })

      | T.Draw | T.Play | T.Discard ->
        (match AI.random_ai rng st_ai with
         | Some st' ->
           if st'.T.current = 0
           then st'
           else loop (steps - 1) st'
         | None ->
           (* AI had no legal move; try endcheck once *)
           (match E.endcheck st_ai with
            | Ok st' | End_round st' ->
              if st'.T.current = 0
              then st'
              else loop (steps - 1) st'
            | Error _ ->
              (* truly stuck; force turn to human *)
              { st_ai with current = 0; phase = T.Draw }))
  in
  loop max_steps st_start
in

      (* This runs after *any* successful human action.
         - Push the previous state to hist.
         - If vs_comp and it's now player 1's turn, let AI finish.
         - Clear the text boxes.
      *)

      let apply_and_maybe_ai (st_after : T.state) : (unit Ui_effect.t) =
        let hist' = st :: hist in
        let final_state =
          if vs_comp && st_after.T.current = 1
          then advance_ai_turn st_after
          else st_after
        in

        set_all_full
          ~screen':screen
          ~vs_comp':vs_comp
          ~st':final_state
          ~drawN':""
          ~set_s':""
          ~run_s':""
          ~lay_c':""
          ~lay_m':""
          ~disc_i':""
          ~hist':hist'
      in

      (* Undo/back button: pop from hist *)
      let on_back _ev =
        match hist with
        | prev :: rest ->
          set_all_full
            ~screen':screen
            ~vs_comp':vs_comp
            ~st':prev
            ~drawN':""
            ~set_s':""
            ~run_s':""
            ~lay_c':""
            ~lay_m':""
            ~disc_i':""
            ~hist':rest
        | [] -> Ui_effect.Ignore
      in

      (* ===================================================== *)
      (* Screen branching                                      *)
      (* ===================================================== *)

      match screen with
      | `Intro ->
        let start_pass_and_play _ev =
          Ui_effect.Many
            [ set_vs false
            ; set_all_full
                ~screen':`Playing
                ~vs_comp':false
                ~st':(initial_state ~vs_computer:false ())
                ~drawN':""
                ~set_s':""
                ~run_s':""
                ~lay_c':""
                ~lay_m':""
                ~disc_i':""
                ~hist':[]
            ]
        in
        let start_vs_computer _ev =
          Ui_effect.Many
            [ set_vs true
            ; set_all_full
                ~screen':`Playing
                ~vs_comp':true
                ~st':(initial_state ~vs_computer:true ())
                ~drawN':""
                ~set_s':""
                ~run_s':""
                ~lay_c':""
                ~lay_m':""
                ~disc_i':""
                ~hist':[]
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
                  ~attrs:
                    [ Vdom.Attr.on_click start_vs_computer
                    ; Vdom.Attr.style (Css_gen.margin_left (`Px 10))
                    ]
                  [ Vdom.Node.text "Play vs Computer" ]
              ]
          ]

      | `Playing ->
        (* shared handler for EndCheck button *)
        let on_end_check _ev =
          match E.endcheck st with
          | Ok st' | End_round st' -> apply_and_maybe_ai st'
          | Error _ -> Ui_effect.Ignore
        in

        (* controls vary with phase *)
        let controls : Vdom.Node.t list =
          match st.phase with
          | T.Draw ->
            let on_draw_deck _ =
              match E.draw ~source:T.FromDeck st with
              | Ok st' | End_round st' -> apply_and_maybe_ai st'
              | Error _ -> Ui_effect.Ignore
            in
            let on_draw_discard_top _ =
              match E.draw ~source:T.FromDiscard st with
              | Ok st' | End_round st' -> apply_and_maybe_ai st'
              | Error _ -> Ui_effect.Ignore
            in
            let on_draw_discard_n _ =
              let n =
                match Int.of_string_opt (String.strip drawN) with
                | Some k when k > 0 -> k
                | _ -> 1
              in
              match E.draw ~source:(T.FromDiscardN n) st with
              | Ok st' | End_round st' -> apply_and_maybe_ai st'
              | Error _ -> Ui_effect.Ignore
            in
            [ Vdom.Node.h3 [ Vdom.Node.text "Draw phase" ]
            ; Vdom.Node.div
                [ Vdom.Node.button
                    ~attrs:[ Vdom.Attr.on_click on_draw_deck ]
                    [ Vdom.Node.text "Draw from deck" ]
                ; Vdom.Node.button
                    ~attrs:
                      [ Vdom.Attr.on_click on_draw_discard_top
                      ; Vdom.Attr.style (Css_gen.margin_left (`Px 8))
                      ]
                    [ Vdom.Node.text "Draw top of discard" ]
                ]
            ; Vdom.Node.div
                [ Vdom.Node.input
                    ~attrs:
                      [ Vdom.Attr.type_ "number"
                      ; Vdom.Attr.placeholder "N"
                      ; Vdom.Attr.value drawN
                      ; Vdom.Attr.min 1.
                      ; Vdom.Attr.on_input (fun _ev s -> set_drawN s)
                      ; Vdom.Attr.style (Css_gen.width (`Px 80))
                      ]
                    ()
                ; Vdom.Node.button
                    ~attrs:
                      [ Vdom.Attr.on_click on_draw_discard_n
                      ; Vdom.Attr.style (Css_gen.margin_left (`Px 8))
                      ]
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
                 | Ok st' | End_round st' -> apply_and_maybe_ai st'
                 | Error _ -> Ui_effect.Ignore)
            in
            let on_make_run _ =
              match cards_by_indices p.hand (parse_indices run_s) with
              | None -> Ui_effect.Ignore
              | Some cs ->
                (match E.play ~action:(T.Make_run cs) st with
                 | Ok st' | End_round st' -> apply_and_maybe_ai st'
                 | Error _ -> Ui_effect.Ignore)
            in
            let on_layoff _ =
              let ci_opt = Int.of_string_opt (String.strip lay_c) in
              let mi_opt = Int.of_string_opt (String.strip lay_m) in
              match ci_opt, mi_opt with
              | Some ci1, Some mi1 ->
                let ci0 = ci1 - 1
                and mi0 = mi1 - 1 in
                (match nth ci0 p.hand with
                 | None -> Ui_effect.Ignore
                 | Some card ->
                   (match E.play ~action:(T.Lay_off (card, mi0)) st with
                    | Ok st' | End_round st' -> apply_and_maybe_ai st'
                    | Error _ -> Ui_effect.Ignore))
              | _ -> Ui_effect.Ignore
            in
            let on_skip _ =
              match E.play ~action:T.Skip_to_discard st with
              | Ok st' | End_round st' -> apply_and_maybe_ai st'
              | Error _ -> Ui_effect.Ignore
            in
            [ Vdom.Node.h3 [ Vdom.Node.text "Play phase" ]
            ; Vdom.Node.div
                [ Vdom.Node.input
                    ~attrs:
                      [ Vdom.Attr.placeholder "set: indices e.g. 1 3 5"
                      ; Vdom.Attr.value set_s
                      ; Vdom.Attr.on_input (fun _ev s -> set_set_s s)
                      ; Vdom.Attr.style (Css_gen.width (`Px 240))
                      ]
                    ()
                ; Vdom.Node.button
                    ~attrs:
                      [ Vdom.Attr.on_click on_make_set
                      ; Vdom.Attr.style (Css_gen.margin_left (`Px 8))
                      ]
                    [ Vdom.Node.text "Make set" ]
                ]
            ; Vdom.Node.div
                [ Vdom.Node.input
                    ~attrs:
                      [ Vdom.Attr.placeholder "run: indices e.g. 2 3 4"
                      ; Vdom.Attr.value run_s
                      ; Vdom.Attr.on_input (fun _ev s -> set_run_s s)
                      ; Vdom.Attr.style (Css_gen.width (`Px 240))
                      ]
                    ()
                ; Vdom.Node.button
                    ~attrs:
                      [ Vdom.Attr.on_click on_make_run
                      ; Vdom.Attr.style (Css_gen.margin_left (`Px 8))
                      ]
                    [ Vdom.Node.text "Make run" ]
                ]
            ; Vdom.Node.div
                [ Vdom.Node.input
                    ~attrs:
                      [ Vdom.Attr.placeholder "layoff: card index"
                      ; Vdom.Attr.value lay_c
                      ; Vdom.Attr.on_input (fun _ev s -> set_lay_c s)
                      ; Vdom.Attr.style (Css_gen.width (`Px 140))
                      ]
                    ()
                ; Vdom.Node.input
                    ~attrs:
                      [ Vdom.Attr.placeholder "on meld index"
                      ; Vdom.Attr.value lay_m
                      ; Vdom.Attr.on_input (fun _ev s -> set_lay_m s)
                      ; Vdom.Attr.style
                          (Css_gen.concat
                             [ Css_gen.width (`Px 140)
                             ; Css_gen.margin_left (`Px 6)
                             ])
                      ]
                    ()
                ; Vdom.Node.button
                    ~attrs:
                      [ Vdom.Attr.on_click on_layoff
                      ; Vdom.Attr.style (Css_gen.margin_left (`Px 8))
                      ]
                    [ Vdom.Node.text "Layoff card on meld" ]
                ]
            ; Vdom.Node.button
                ~attrs:
                  [ Vdom.Attr.on_click on_skip
                  ; Vdom.Attr.style (Css_gen.margin_top (`Px 8))
                  ]
                [ Vdom.Node.text "Skip to Discard" ]
            ]

          | T.Discard ->
            let p = st.players.(st.current) in
            let on_discard_click _ =
              let idx0 =
                match Int.of_string_opt (String.strip disc_i) with
                | Some k when k >= 1 -> k - 1
                | _ -> 0
              in
              match nth idx0 p.hand with
              | None -> Ui_effect.Ignore
              | Some card ->
                (match E.discard ~action:(T.Discard_card card) st with
                 | Ok st' | End_round st' -> apply_and_maybe_ai st'
                 | Error _ -> Ui_effect.Ignore)
            in
            [ Vdom.Node.h3 [ Vdom.Node.text "Discard phase" ]
            ; Vdom.Node.div
                [ Vdom.Node.input
                    ~attrs:
                      [ Vdom.Attr.placeholder "discard: card index"
                      ; Vdom.Attr.value disc_i
                      ; Vdom.Attr.on_input (fun _ev s -> set_disc_i s)
                      ; Vdom.Attr.style (Css_gen.width (`Px 160))
                      ]
                    ()
                ; Vdom.Node.button
                    ~attrs:
                      [ Vdom.Attr.on_click on_discard_click
                      ; Vdom.Attr.style (Css_gen.margin_left (`Px 8))
                      ]
                    [ Vdom.Node.text "Discard" ]
                ]
            ]

          | T.EndCheck ->
  let pass_message =
    if not vs_comp then
      Vdom.Node.p
        ~attrs:[ Vdom.Attr.style (Css_gen.color (`Name "red")) ]
        [ Vdom.Node.text "Please pass your device to the next player." ]
    else
      Vdom.Node.none
  in
  [ Vdom.Node.h3 [ Vdom.Node.text "End-of-turn checks" ]
  ; pass_message
  ; Vdom.Node.button
      ~attrs:[ Vdom.Attr.on_click on_end_check ]
      [ Vdom.Node.text "Continue / End check" ]
  ]
        in

        (* main board panel *)
        let p = st.players.(st.current) in
Vdom.Node.div
  [ Vdom.Node.h1 [ Vdom.Node.text "" ]

  ; Vdom.Node.p
      [ Vdom.Node.text
          (Printf.sprintf "Current: %s (phase: %s)"
             p.name (string_of_phase st.phase)) ]

  ; Vdom.Node.p
      [ Vdom.Node.text
          (Printf.sprintf "Deck: %d   Discard: %d"
             (List.length st.deck) (List.length st.discard)) ]

; (if Poly.(st.T.phase = T.EndCheck) then
     Vdom.Node.div
       [ Vdom.Node.h3
           [ Vdom.Node.text "Please pass your device to the next player." ] ]
   else
     Vdom.Node.div
       [ Vdom.Node.h3 [ Vdom.Node.text (p.name ^ "'s hand") ]
       ; view_cards p.hand
       ])

  (* show discard pile *)
  ; view_discard st.discard
  

  (* NEW: show melds that have been laid on the table *)
  
  ; view_melds st.melds

  ; Vdom.Node.hr ()

  ; Vdom.Node.div controls

  ; (match hist with
     | _ :: _ ->
       Vdom.Node.button
         ~attrs:
           [ Vdom.Attr.on_click on_back
           ; Vdom.Attr.style (Css_gen.margin_top (`Px 10))
           ]
         [ Vdom.Node.text "⬅ Back" ]
     | [] -> Vdom.Node.none)
  ]
    )

let () =
  Bonsai_web.Start.start
    ~bind_to_element_with_id:"app"
    component