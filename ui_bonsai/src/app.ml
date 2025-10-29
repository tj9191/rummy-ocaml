open! Core
open! Bonsai
open! Bonsai_web

module Vdom  = Virtual_dom.Vdom
module RE    = Rummy_engine
module T     = RE.Types
module E     = RE.Engine
module Setup = Rummy_engine.Setup
module AI    = Rummy_engine.Ai

(* ---------- helpers ---------- *)

let string_of_card (c : T.card) = RE.Types.string_of_card c

let string_of_phase = function
  | T.Draw     -> "Draw"
  | T.Play     -> "Play"
  | T.Discard  -> "Discard"
  | T.EndCheck -> "EndCheck"

let rank_order (r : T.rank) : int =
  match r with
  | T.Ace   -> 14
  | T.King  -> 13
  | T.Queen -> 12
  | T.Jack  -> 11
  | T.Ten   -> 10
  | T.Nine  -> 9
  | T.Eight -> 8
  | T.Seven -> 7
  | T.Six   -> 6
  | T.Five  -> 5
  | T.Four  -> 4
  | T.Three -> 3
  | T.Two   -> 2

let suit_order (s : T.suit) : int =
  match s with
  | Hearts   -> 0
  | Diamonds -> 1
  | Clubs    -> 2
  | Spades   -> 3

let sort_hand_for_display (hand : T.card list) : T.card list =
  List.sort hand ~compare:(fun a b ->
    let sa = suit_order a.suit
    and sb = suit_order b.suit in
    if Int.(sa <> sb) then Int.compare sa sb
    else Int.compare (rank_order a.rank) (rank_order b.rank)
  )

(* ---------- initial state ---------- *)

let h r = { T.suit = Hearts;   rank = r }
let s r = { T.suit = Spades;   rank = r }
let d r = { T.suit = Diamonds; rank = r }
let c r = { T.suit = Clubs;    rank = r }

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
  let used_cards = p0.hand @ p1.hand in
  let card_equal (a : T.card) (b : T.card) =
    Poly.(a.suit = b.suit && a.rank = b.rank)
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

(* ---------- style helper ---------- *)
(* Older Bonsai / js_of_ocaml doesn't always ship Css_gen helpers.
   We'll directly attach a "style" attribute string. *)
let style (s : string) : Vdom.Attr.t =
  Vdom.Attr.create "style" s

(* ---------- view helpers ---------- *)

(* single face-up card *)
let view_faceup_card ~selected ~on_click (card : T.card) =
  let border_color = if selected then "gold" else "#333" in
  let border_px    = if selected then "3px" else "1px" in
  let card_css =
    Printf.sprintf {|
      display:flex;
      flex-direction:column;
      align-items:center;
      justify-content:center;
      width:60px;
      height:90px;
      margin:4px;
      border:%s solid %s;
      border-radius:6px;
      background-color:white;
      box-shadow:0px 2px 4px rgba(0,0,0,0.3);
      cursor:pointer;
      font-family:sans-serif;
      font-size:14px;
      text-align:center;
      color:black;          /* <-- add this */
    |} border_px border_color
  in
  Vdom.Node.div
    ~attrs:[
      style card_css;
      Vdom.Attr.on_click on_click
    ]
    [ Vdom.Node.div [ Vdom.Node.text (string_of_card card) ] ]

(* facedown deck *)
let view_facedown_card ~count ~on_click =
  let label =
    if count > 0 then Printf.sprintf "Deck (%d)" count
    else "Empty"
  in
  let deck_css = {|
    display:flex;
    flex-direction:column;
    align-items:center;
    justify-content:center;
    width:60px;
    height:90px;
    margin:4px;
    border:2px solid #003366;
    border-radius:6px;
    background-color:#0055aa;
    box-shadow:0px 2px 4px rgba(0,0,0,0.5);
    cursor:pointer;
    font-family:sans-serif;
    font-size:13px;
    color:white;
    text-align:center;
  |} in
  Vdom.Node.div
    ~attrs:[
      style deck_css;
      Vdom.Attr.on_click on_click
    ]
    [ Vdom.Node.div [ Vdom.Node.text label ] ]

(* discard pile fan, head of list is top *)
let view_discard_pile ~(discard : T.card list) ~on_click_card =
  let container_css = {|
    position:relative;
    width:120px;
    height:110px;
    font-family:sans-serif;
  |} in
  let empty_css = {|
    display:flex;
    align-items:center;
    justify-content:center;
    width:100px;
    height:90px;
    border:2px dashed #555;
    border-radius:6px;
    color:#555;
    font-size:12px;
  |} in
  let card_nodes =
    List.mapi discard ~f:(fun i card ->
      let left_px = 5 * i in
      let wrapper_css =
        Printf.sprintf {|
          position:absolute;
          left:%dpx;
          top:0px;
          cursor:pointer;
        |} left_px
      in
      Vdom.Node.div
        ~attrs:[
          style wrapper_css;
          Vdom.Attr.on_click (on_click_card i)
        ]
        [ view_faceup_card
            ~selected:false
            ~on_click:(fun _ -> Ui_effect.Ignore)
            card
        ]
    )
  in
  Vdom.Node.div
    ~attrs:[ style container_css ]
    (if List.is_empty discard then
       [ Vdom.Node.div
           ~attrs:[ style empty_css ]
           [ Vdom.Node.text "Discard (empty)" ] ]
     else
       card_nodes
    )

(* bottom hand row *)
let view_hand_row
    ~(hand : T.card list)
    ~(selected_idxs : int list)
    ~(on_click_card : int -> unit Ui_effect.t)
  =
  let row_css = {|
    display:flex;
    flex-wrap:wrap;
    justify-content:center;
    align-items:flex-end;
    padding:10px;
    background-color:#0a0a0a;
    color:white;
    min-height:140px;
    font-family:sans-serif;
    border-top:2px solid #333;
  |} in
  Vdom.Node.div
    ~attrs:[ style row_css ]
    (List.mapi hand ~f:(fun i card ->
       let is_sel = List.mem selected_idxs i ~equal:Int.equal in
       view_faceup_card
         ~selected:is_sel
         ~on_click:(fun _ -> on_click_card i)
         card
    ))

let view_melds_top (melds : T.meld list) =
  let outer_css = {|
    background-color:#1a1a1a;
    color:white;
    padding:10px;
    font-family:sans-serif;
    border-bottom:2px solid #333;
  |} in
  let title_css = {|
    margin-bottom:4px;
    font-size:16px;
  |} in
  let meld_css = {|
    font-size:13px;
  |} in
  let meld_nodes =
    if List.is_empty melds then
      [ Vdom.Node.div
          ~attrs:[ style "color:#888;" ]
          [ Vdom.Node.text "(no melds yet)" ] ]
    else
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
        Vdom.Node.div
          ~attrs:[ style meld_css ]
          [ Vdom.Node.text
              (Printf.sprintf "%d) %s: %s" (i+1) kind_str cards_str)
          ])
  in
  Vdom.Node.div
    ~attrs:[ style outer_css ]
    ([ Vdom.Node.h3
         ~attrs:[ style title_css ]
         [ Vdom.Node.text "Melds on Table" ]
     ]
     @ meld_nodes)

let view_status_top ~(st : T.state) =
  let row_css = {|
    display:flex;
    flex-direction:row;
    flex-wrap:wrap;
    justify-content:space-between;
    align-items:center;
    padding:10px;
    background-color:#111;
    color:white;
    font-family:sans-serif;
    font-size:14px;
    border-bottom:2px solid #333;
  |} in
  let current_p = st.players.(st.current) in
  Vdom.Node.div
    ~attrs:[ style row_css ]
    [ Vdom.Node.div
        [ Vdom.Node.text
            (Printf.sprintf "Current: %s (phase: %s)"
               current_p.name (string_of_phase st.phase))
        ]
    ; Vdom.Node.div
        [ Vdom.Node.text
            (Printf.sprintf "Deck: %d | Discard: %d"
               (List.length st.deck)
               (List.length st.discard))
        ]
    ]

(* ---------- main component ---------- *)

let component graph =
  let rng = Random.State.make [| 0xC0FFEE |] in

  (* screen, vs_comp, st, selected_idxs, hist *)
  let state =
    Bonsai.state
      ( `Intro
      , false
      , initial_state ~vs_computer:false ()
      , ([] : int list)
      , ([] : T.state list)
      )
      graph
  in

  Bonsai.Value.map state
    ~f:(fun ((screen, vs_comp, st, selected_idxs, hist), set_all) ->

      let set_all_full ~screen' ~vs_comp' ~st' ~selected' ~hist' =
        set_all (screen', vs_comp', st', selected', hist')
      in

      let set_screen scr =
        set_all (scr, vs_comp, st, selected_idxs, hist)
      in
      let set_vs b =
        set_all (screen, b, st, selected_idxs, hist)
      in
      let set_selected sel =
        set_all (screen, vs_comp, st, sel, hist)
      in

      let toggle_card_index i =
        let sel =
          if List.mem selected_idxs i ~equal:Int.equal
          then List.filter selected_idxs ~f:(fun j -> j <> i)
          else i :: selected_idxs
        in
        set_selected sel
      in

      (* AI turn driver *)
      let advance_ai_turn (st_start : T.state) : T.state =
        let max_steps = 20 in
        let rec loop steps st_ai =
          if st_ai.T.current = 0 then
            st_ai
          else if steps <= 0 then
            { st_ai with current = 0; phase = T.Draw }
          else
            match st_ai.T.phase with
            | T.EndCheck ->
              (match E.endcheck st_ai with
               | Ok st' | End_round st' ->
                 if st'.T.current = 0 then st' else loop (steps - 1) st'
               | Error _ ->
                 { st_ai with current = 0; phase = T.Draw })
            | T.Draw | T.Play | T.Discard ->
              (match AI.random_ai rng st_ai with
               | Some st' ->
                 if st'.T.current = 0 then st'
                 else loop (steps - 1) st'
               | None ->
                 (match E.endcheck st_ai with
                  | Ok st' | End_round st' ->
                    if st'.T.current = 0 then st'
                    else loop (steps - 1) st'
                  | Error _ ->
                    { st_ai with current = 0; phase = T.Draw }))
        in
        loop max_steps st_start
      in

      let apply_and_maybe_ai (st_after : T.state) : unit Ui_effect.t =
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
          ~selected':[]
          ~hist':hist'
      in

      let on_back _ev =
        match hist with
        | prev :: rest ->
          set_all_full
            ~screen':screen
            ~vs_comp':vs_comp
            ~st':prev
            ~selected':[]
            ~hist':rest
        | [] -> Ui_effect.Ignore
      in

      (* ---------- phase actions ---------- *)

      let current_player = st.players.(st.current) in

      let selected_cards_from_hand () : T.card list option =
        let hand = current_player.hand in
        let max_i = List.length hand - 1 in
        if List.exists selected_idxs ~f:(fun i -> i < 0 || i > max_i)
        then None
        else
          Some (List.map (List.sort selected_idxs ~compare:Int.compare)
                  ~f:(fun i -> List.nth_exn hand i))
      in

            (* Draw phase: click deck or discard pile *)
      let on_click_deck _ev =
        if Poly.(st.phase <> T.Draw) || st.current <> 0
        then Ui_effect.Ignore
        else (
          match E.draw ~source:T.FromDeck st with
          | Ok st' | End_round st' -> apply_and_maybe_ai st'
          | Error _ -> Ui_effect.Ignore
        )
      in

      let on_click_discard_card i_from_top _ev =
        if Poly.(st.phase <> T.Draw) || st.current <> 0
        then Ui_effect.Ignore
        else (
          let src =
            if i_from_top = 0
            then T.FromDiscard
            else T.FromDiscardN (i_from_top + 1)
          in
          match E.draw ~source:src st with
          | Ok st' | End_round st' -> apply_and_maybe_ai st'
          | Error _ -> Ui_effect.Ignore
        )
      in

      (* Play phase actions *)
      let on_sort_hand _ev =
        if st.current <> 0 then Ui_effect.Ignore
        else (
          let new_players = Array.copy st.players in
          let p0 = new_players.(0) in
          let sorted_hand = sort_hand_for_display p0.hand in
          new_players.(0) <- { p0 with hand = sorted_hand };
          let st' = { st with players = new_players } in
          set_all (screen, vs_comp, st', selected_idxs, hist)
        )
      in

      let on_meld_selected _ev =
        if Poly.(st.phase <> T.Play) || st.current <> 0
        then Ui_effect.Ignore
        else (
          match selected_cards_from_hand () with
          | None -> Ui_effect.Ignore
          | Some cs ->
            let try_set =
              match E.play ~action:(T.Make_set cs) st with
              | Ok st' | End_round st' -> Some st'
              | Error _ -> None
            in
            (match try_set with
             | Some st' -> apply_and_maybe_ai st'
             | None ->
               let try_run =
                 match E.play ~action:(T.Make_run cs) st with
                 | Ok st' | End_round st' -> Some st'
                 | Error _ -> None
               in
               (match try_run with
                | Some st'' -> apply_and_maybe_ai st''
                | None -> Ui_effect.Ignore))
        )
      in

      let on_skip_to_discard _ev =
        if Poly.(st.phase <> T.Play) || st.current <> 0
        then Ui_effect.Ignore
        else (
          match E.play ~action:T.Skip_to_discard st with
          | Ok st' | End_round st' -> apply_and_maybe_ai st'
          | Error _ -> Ui_effect.Ignore
        )
      in

      (* Discard phase *)
      let on_discard_click _ev =
        if Poly.(st.phase <> T.Discard) || st.current <> 0
        then Ui_effect.Ignore
        else (
          match selected_cards_from_hand () with
          | Some [card] ->
            (match E.discard ~action:(T.Discard_card card) st with
             | Ok st' | End_round st' -> apply_and_maybe_ai st'
             | Error _ -> Ui_effect.Ignore)
          | _ -> Ui_effect.Ignore
        )
      in

      (* EndCheck *)
      let on_end_check _ev =
        match E.endcheck st with
        | Ok st' | End_round st' -> apply_and_maybe_ai st'
        | Error _ -> Ui_effect.Ignore
      in

      (* ---------- render ---------- *)

      match screen with
      | `Intro ->
        let page_css = {|
          font-family:sans-serif;
          color:white;
          background-color:#000;
          min-height:100vh;
          display:flex;
          flex-direction:column;
          align-items:center;
          justify-content:center;
          gap:20px;
          text-align:center;
        |} in
        let btn_css = {|
          padding:8px 12px;
          font-size:16px;
          cursor:pointer;
          color:black;
          background-color:#eee;
          border-radius:4px;
          border:1px solid #666;
          margin:0 10px;
        |} in

        let start_pass_and_play _ev =
          let st0 = initial_state ~vs_computer:false () in
          Ui_effect.Many
            [ set_vs false
            ; set_all_full
                ~screen':`Playing
                ~vs_comp':false
                ~st':st0
                ~selected':[]
                ~hist':[]
            ]
        in
        let start_vs_computer _ev =
          let st0 = initial_state ~vs_computer:true () in
          Ui_effect.Many
            [ set_vs true
            ; set_all_full
                ~screen':`Playing
                ~vs_comp':true
                ~st':st0
                ~selected':[]
                ~hist':[]
            ]
        in

        Vdom.Node.div
          ~attrs:[ style page_css ]
          [ Vdom.Node.h1
              ~attrs:[ style "font-size:32px;margin:0;" ]
              [ Vdom.Node.text "Welcome to Rummy!" ]
          ; Vdom.Node.p
              [ Vdom.Node.text "Choose a mode to begin:" ]
          ; Vdom.Node.div
              [ Vdom.Node.button
                  ~attrs:[ Vdom.Attr.on_click start_pass_and_play
                          ; style btn_css ]
                  [ Vdom.Node.text "Pass & Play" ]
              ; Vdom.Node.button
                  ~attrs:[ Vdom.Attr.on_click start_vs_computer
                          ; style btn_css ]
                  [ Vdom.Node.text "Play vs Computer" ]
              ]
          ]

      | `Playing ->
        let screen_css = {|
          display:flex;
          flex-direction:column;
          min-height:100vh;
          background-color:#000;
          color:white;
          font-family:sans-serif;
        |} in
        let middle_wrap_css = {|
          flex-grow:1;
          display:flex;
          flex-direction:column;
          align-items:center;
          justify-content:center;
          background-color:#0f0f0f;
          color:white;
          font-family:sans-serif;
          padding:20px;
        |} in
        let table_css = {|
          display:flex;
          flex-direction:row;
          align-items:flex-start;
          justify-content:center;
          gap:40px;
          padding:20px;
          background-color:#1a1a1a;
          border:2px solid #333;
          border-radius:8px;
          box-shadow:0px 4px 10px rgba(0,0,0,0.6);
        |} in
        let bottom_css = {|
          background-color:#000;
          border-top:2px solid #333;
          padding:10px;
          color:white;
          font-family:sans-serif;
          text-align:center;
        |} in
        let hand_title_css = {|
          color:white;
          font-size:16px;
          margin-bottom:6px;
        |} in
        let controls_row_css = {|
          display:flex;
          flex-direction:row;
          flex-wrap:wrap;
          justify-content:center;
          gap:10px;
          margin-top:8px;
        |} in
        let btn_small_css = {|
          padding:6px 10px;
          cursor:pointer;
          background-color:#eee;
          color:black;
          border-radius:4px;
          border:1px solid #666;
          font-size:14px;
        |} in
        let endcheck_wrap_css = {|
          display:flex;
          flex-direction:column;
          align-items:center;
          justify-content:center;
          gap:10px;
          margin-top:8px;
          text-align:center;
        |} in

        (* phase_controls depends on st.phase *)
        let phase_controls =
          match st.phase with
          | T.Draw ->
            Vdom.Node.div
              [ Vdom.Node.div
                  ~attrs:[ style "color:white;font-size:14px;margin-top:8px;" ]
                  [ Vdom.Node.text
                      "Draw phase: click Deck or Discard pile above." ]
              ]

          | T.Play ->
            Vdom.Node.div
              ~attrs:[ style controls_row_css ]
              [ Vdom.Node.button
                  ~attrs:[ Vdom.Attr.on_click on_sort_hand
                         ; style btn_small_css ]
                  [ Vdom.Node.text "SORT HAND" ]
              ; Vdom.Node.button
                  ~attrs:[ Vdom.Attr.on_click on_meld_selected
                         ; style btn_small_css ]
                  [ Vdom.Node.text "MELD THESE CARDS" ]
              ; Vdom.Node.button
                  ~attrs:[ Vdom.Attr.on_click on_skip_to_discard
                         ; style btn_small_css ]
                  [ Vdom.Node.text "SKIP TO DISCARD PHASE" ]
              ]

          | T.Discard ->
            Vdom.Node.div
              ~attrs:[ style controls_row_css ]
              [ Vdom.Node.button
                  ~attrs:[ Vdom.Attr.on_click on_discard_click
                         ; style btn_small_css ]
                  [ Vdom.Node.text "DISCARD" ]
              ]

          | T.EndCheck ->
            let pass_msg =
              if not vs_comp then
                Vdom.Node.p
                  ~attrs:[ style "color:red;font-size:14px;margin-top:8px;" ]
                  [ Vdom.Node.text
                      "Please pass your device to the next player." ]
              else
                Vdom.Node.none
            in
            Vdom.Node.div
              ~attrs:[ style endcheck_wrap_css ]
              [ pass_msg
              ; Vdom.Node.button
                  ~attrs:[ Vdom.Attr.on_click on_end_check
                         ; style btn_small_css ]
                  [ Vdom.Node.text "CONTINUE" ]
              ]
        in

        let human_hand = st.players.(0).hand in

        let top_area =
          Vdom.Node.div
            [ view_status_top ~st
            ; view_melds_top st.melds
            ]
        in

        let middle_area =
          Vdom.Node.div
            ~attrs:[ style middle_wrap_css ]
            [ Vdom.Node.div
                ~attrs:[ style table_css ]
                [ view_facedown_card
                    ~count:(List.length st.deck)
                    ~on_click:on_click_deck
                ; view_discard_pile
                    ~discard:st.discard
                    ~on_click_card:on_click_discard_card
                ]
            ]
        in

        let bottom_area =
          Vdom.Node.div
            ~attrs:[ style bottom_css ]
            [ Vdom.Node.h3
                ~attrs:[ style hand_title_css ]
                [ Vdom.Node.text "Player 1's Hand" ]
            ; view_hand_row
                ~hand:human_hand
                ~selected_idxs:selected_idxs
                ~on_click_card:(fun i ->
                  if st.current = 0 then toggle_card_index i
                  else Ui_effect.Ignore)
            ; phase_controls
            ]
        in

        let undo_button =
          match hist with
          | _ :: _ ->
            Vdom.Node.div
              ~attrs:[ style "padding:10px;text-align:center;" ]
              [ Vdom.Node.button
                  ~attrs:[ Vdom.Attr.on_click on_back
                         ; style btn_small_css ]
                  [ Vdom.Node.text "⬅ UNDO" ]
              ]
          | [] -> Vdom.Node.none
        in

        let screen_node_css = screen_css in
        Vdom.Node.div
          ~attrs:[ style screen_node_css ]
          [ top_area
          ; middle_area
          ; bottom_area
          ; undo_button
          ]
    )

let () =
  Bonsai_web.Start.start
    ~bind_to_element_with_id:"app"
    component