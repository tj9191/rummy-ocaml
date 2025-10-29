open! Core
open! Bonsai
open! Bonsai_web

module Vdom  = Virtual_dom.Vdom
module RE    = Rummy_engine
module T     = RE.Types
module E     = Rummy_engine.Engine
module Setup = Rummy_engine.Setup
module AI    = Rummy_engine.Ai

(* Ensure global PRNG is randomized in the browser *)
let () = Random.self_init ()

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

let suit_symbol = function
  | T.Hearts -> "♥"
  | T.Diamonds -> "♦"
  | T.Clubs -> "♣"
  | T.Spades -> "♠"

let sort_hand_for_display (hand : T.card list) : T.card list =
  List.sort hand ~compare:(fun a b ->
    let sa = suit_order a.suit
    and sb = suit_order b.suit in
    if Int.(sa <> sb) then Int.compare sa sb
    else Int.compare (rank_order a.rank) (rank_order b.rank)
  )

(* Robust Fisher–Yates shuffle using a dedicated RNG state *)
let shuffle_with_state (st : Random.State.t) (xs : 'a list) : 'a list =
  let a = Array.of_list xs in
  for i = Array.length a - 1 downto 1 do
    let j = Random.State.int st (i + 1) in
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t
  done;
  Array.to_list a

(* Deal helper: take k from deck *)
let deal_n k (deck : T.card list) : T.card list * T.card list =
  let rec loop n acc d =
    if n = 0 then (List.rev acc, d)
    else match d with
      | [] -> (List.rev acc, [])
      | x :: xs -> loop (n - 1) (x :: acc) xs
  in
  loop k [] deck

(* ---------- initial state ---------- *)

let initial_state ~vs_computer () : T.state =
  (* Fresh, self-initialized RNG for each new game *)
  let deck_rng = Random.State.make_self_init () in
  let shuffled = Setup.all_cards |> shuffle_with_state deck_rng in
  let p0_hand, deck1 = deal_n 7 shuffled in
  let p1_hand, deck2 = deal_n 7 deck1 in

  let p0 = { T.id = 0; name = "Player 1"; hand = p0_hand } in
  let p1_name = if vs_computer then "Computer" else "Player 2" in
  let p1 = { T.id = 1; name = p1_name; hand = p1_hand } in

  { T.deck            = deck2
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
      color:black;
    |} border_px border_color
  in
  Vdom.Node.div
    ~attrs:[ style card_css; Vdom.Attr.on_click on_click ]
    [ Vdom.Node.text (string_of_card card) ]

(* facedown draw pile *)
let view_facedown_card ~count ~on_click =
  let label = if count > 0 then Printf.sprintf "Deck (%d)" count else "Empty" in
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
    ~attrs:[ style deck_css; Vdom.Attr.on_click on_click ]
    [ Vdom.Node.div [ Vdom.Node.text label ] ]

(* Discard pile:
   - Empty: centered text
   - 1 card: show face-up; clicking stack draws top only
   - >=2: show stack (top + white layers) + a horizontal bar of text icons (top->bottom).
           Clicking stack draws top; clicking icon i draws FromDiscardN(i+1). *)
let view_discard_pile
    ~(discard : T.card list)
    ~on_click_stack
    ~on_click_bar
  =
  let outer_css = {|
    display:flex;flex-direction:column;align-items:center;
    font-family:sans-serif;min-width:260px;
  |} in
  if List.is_empty discard then
    Vdom.Node.div
      ~attrs:[ style outer_css ]
      [ Vdom.Node.div
          ~attrs:[ style "width:120px;height:100px;display:flex;align-items:center;justify-content:center;border:2px dashed #555;border-radius:6px;color:#777;" ]
          [ Vdom.Node.text "Discard (empty)" ] ]
  else
    let n = List.length discard in
    let top_card = List.hd_exn discard in
    (* stack visuals *)
    let stack_css =
      "position:relative;width:80px;height:100px;cursor:pointer;margin-top:8px;"
    in
    let layer_nodes =
      if n <= 1 then []
      else
        let num_layers = Int.min (n - 1) 3 in
        List.init num_layers ~f:(fun i ->
          let top_px = 3 * i in
          let left_px = 3 * i in
          let css = Printf.sprintf
              "position:absolute;top:%dpx;left:%dpx;width:60px;height:90px;background:white;border:1px solid #ccc;border-radius:6px;z-index:%d;"
              top_px left_px i
          in
          Vdom.Node.div ~attrs:[ style css ] [])
    in
    let top_node =
      Vdom.Node.div
        ~attrs:[ style "position:absolute;top:0;left:0;z-index:999;" ]
        [ view_faceup_card ~selected:false ~on_click:on_click_stack top_card ]
    in
    (* bar: top → bottom *)
    let bar_css = {|
      display:flex;flex-direction:row;justify-content:center;flex-wrap:wrap;
      gap:4px;margin-bottom:6px;max-width:360px;
    |} in
    let rank_text = function
      | T.Two->"2"|Three->"3"|Four->"4"|Five->"5"|Six->"6"
      | Seven->"7"|Eight->"8"|Nine->"9"|Ten->"10"|Jack->"J"
      | Queen->"Q"|King->"K"|Ace->"A"
    in
    let bar_nodes =
      List.mapi discard ~f:(fun i card ->
        let icon = rank_text card.rank ^ suit_symbol card.suit in
        let css =
          "padding:2px 6px;border:1px solid #444;border-radius:4px;\
           background:#eee;color:#000;font-size:11px;cursor:pointer;user-select:none;"
        in
        Vdom.Node.div
          ~attrs:[ style css; Vdom.Attr.on_click (on_click_bar i) ]
          [ Vdom.Node.text icon ])
    in
    Vdom.Node.div
      ~attrs:[ style outer_css ]
      ([ if n > 1 then Vdom.Node.div ~attrs:[ style bar_css ] bar_nodes else Vdom.Node.none ]
       @ [ Vdom.Node.div ~attrs:[ style stack_css ] (layer_nodes @ [ top_node ]) ])

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
       let border_color = if is_sel then "gold" else "#333" in
       let wrapper = Printf.sprintf "border:2px solid %s;border-radius:8px;" border_color in
       Vdom.Node.div
         ~attrs:[ style wrapper; Vdom.Attr.on_click (fun _ -> on_click_card i) ]
         [ view_faceup_card ~selected:is_sel ~on_click:(fun _ -> Ui_effect.Ignore) card ]))

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
      [ Vdom.Node.div ~attrs:[ style "color:#888;" ] [ Vdom.Node.text "(no melds yet)" ] ]
    else
      List.mapi melds ~f:(fun i m ->
        let kind_str = match m.kind with T.Set -> "Set" | T.Run -> "Run" in
        let cards_str = String.concat ~sep:", " (List.map m.cards ~f:string_of_card) in
        Vdom.Node.div ~attrs:[ style meld_css ]
          [ Vdom.Node.text (Printf.sprintf "%d) %s: %s" (i+1) kind_str cards_str) ])
  in
  Vdom.Node.div
    ~attrs:[ style outer_css ]
    ([ Vdom.Node.h3 ~attrs:[ style title_css ] [ Vdom.Node.text "Melds on Table" ] ]
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
               (List.length st.deck) (List.length st.discard))
        ]
    ]

(* ---------- modal popup ---------- *)

let view_modal ~message ~on_close =
  let overlay = {|
    position:fixed;inset:0;background:rgba(0,0,0,0.6);
    display:flex;align-items:center;justify-content:center;z-index:9999;
  |} in
  let card = {|
    background:#fff;color:#000;border-radius:8px;padding:16px 20px;
    width:min(520px,90vw);box-shadow:0 10px 24px rgba(0,0,0,0.35);
    font-family:sans-serif;
  |} in
  let btn = {|
    margin-top:12px;padding:6px 10px;border:1px solid #444;border-radius:6px;
    background:#eee;color:#000;cursor:pointer;font-size:14px;
  |} in
  Vdom.Node.div
    ~attrs:[ style overlay; Vdom.Attr.on_click on_close ]
    [ Vdom.Node.div
        ~attrs:[ style card; Vdom.Attr.on_click (fun _ -> Ui_effect.Ignore) ]
        [ Vdom.Node.h3 [ Vdom.Node.text "Heads up" ]
        ; Vdom.Node.p [ Vdom.Node.text message ]
        ; Vdom.Node.button ~attrs:[ style btn; Vdom.Attr.on_click on_close ] [ Vdom.Node.text "OK" ]
        ]
    ]

(* ---------- tutorial content ---------- *)

let tutorial_text : string = {|
Deck: Standard 52-card deck
Players: 2
Deal: 7 cards to each player

Goal

Earn the most amount of points by making melds. A game ends once a player discards their last card.

Melds

You may lay down cards in either of these ways:

Sets (same rank): e.g., 7♣ 7♥ 7♦
Runs (same suit, consecutive rank): e.g., 9♠ 10♠ J♠

Note: If you are playing a run with ACES, it must be A♠ 2♠ 3♠ OR Q♠ K♠ A♠. The ACE cannot be K♠ A♠ 2♠

Card Values
2–9: 5 points each
10, J, Q, K: 10 points each
A: 15 points

Turn Structure

On your turn, you DRAW → PLAY → DISCARD.

DRAW
• Take the top card from the draw pile, or
• Take from the discard pile:
  • Click the stack to take only the top card, or
  • Click a card icon in the horizontal bar to take that card and all cards above it.
• If you take more than one from the discard, you must use the bottom-most picked card in a meld during this turn before you can discard.

PLAY
• Lay down any valid sets or runs from your hand.

DISCARD
• End your turn by discarding one card to the discard pile.
• If you’ve played all your cards but have not discarded, you must continue until you can make a legal discard. The round ends only after a discard.
|}

(* ---------- main component ---------- *)

let component graph =
  (* AI RNG: its own self-initialized stream *)
  let rng = Random.State.make_self_init () in

  (* app state:
     screen: [`Intro | `Tutorial | `Playing]
     vs_comp: bool (true = vs computer; false = pass & play)
     st: engine state
     selected_idxs: selected hand indices
     hist: undo stack
     show_popup: bool (rule popup) *)
  let state =
    Bonsai.state
      ( `Intro
      , false
      , initial_state ~vs_computer:false ()
      , ([] : int list)
      , ([] : T.state list)
      , false (* show_popup *)
      )
      graph
  in

  Bonsai.Value.map state
    ~f:(fun ((screen, vs_comp, st, selected_idxs, hist, show_popup), set_all) ->

      let set_all_full ~screen' ~vs_comp' ~st' ~selected' ~hist' ~popup' =
        set_all (screen', vs_comp', st', selected', hist', popup')
      in

      let set_vs b = set_all (screen, b, st, selected_idxs, hist, show_popup) in
      let set_selected sel = set_all (screen, vs_comp, st, sel, hist, show_popup) in
      let set_popup b = set_all (screen, vs_comp, st, selected_idxs, hist, b) in

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
          ~popup':false
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
            ~popup':false
        | [] -> Ui_effect.Ignore
      in

      (* whose turn is allowed to act? *)
      let human_turn () = if vs_comp then st.current = 0 else true in

      (* In pass & play, during EndCheck we hide the hand so next player can't peek *)
      let hide_hand_now () = (not vs_comp) && Poly.(st.phase = T.EndCheck) in

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

      (* Draw phase: click deck OR discard pile interactions *)
      let on_click_deck _ev =
        if Poly.(st.phase <> T.Draw) || not (human_turn ())
        then Ui_effect.Ignore
        else (
          match E.draw ~source:T.FromDeck st with
          | Ok st' | End_round st' -> apply_and_maybe_ai st'
          | Error _ -> Ui_effect.Ignore
        )
      in

      (* Clicking the visible stack always draws top only *)
      let on_click_discard_stack _ev =
        if Poly.(st.phase <> T.Draw) || not (human_turn ()) then Ui_effect.Ignore
        else
          match E.draw ~source:T.FromDiscard st with
          | Ok st' | End_round st' -> apply_and_maybe_ai st'
          | Error _ -> Ui_effect.Ignore
      in

      (* Clicking an icon in the bar (index from TOP) draws that card and all above it *)
      let on_click_discard_bar i_from_top _ev =
        if Poly.(st.phase <> T.Draw) || not (human_turn ()) then Ui_effect.Ignore
        else
          match E.draw ~source:(T.FromDiscardN (i_from_top + 1)) st with
          | Ok st' | End_round st' -> apply_and_maybe_ai st'
          | Error _ -> Ui_effect.Ignore
      in

      (* Play phase actions *)
      let on_sort_hand _ev =
        if not (human_turn ()) then Ui_effect.Ignore
        else (
          let new_players = Array.copy st.players in
          let p_curr = new_players.(st.current) in
          let sorted_hand = sort_hand_for_display p_curr.hand in
          new_players.(st.current) <- { p_curr with hand = sorted_hand };
          let st' = { st with players = new_players } in
          set_all (screen, vs_comp, st', selected_idxs, hist, show_popup)
        )
      in

      let on_meld_selected _ev =
        if Poly.(st.phase <> T.Play) || not (human_turn ())
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

      (* If the engine still has required_to_use set, they haven't satisfied the
         "must use the bottom-most picked card" rule. Show popup instead of skipping. *)
      let on_skip_to_discard _ev =
        if Poly.(st.phase <> T.Play) || not (human_turn ()) then Ui_effect.Ignore
        else (
          match st.required_to_use with
          | Some _ ->
            set_popup true
          | None ->
            (match E.play ~action:T.Skip_to_discard st with
             | Ok st' | End_round st' -> apply_and_maybe_ai st'
             | Error _ -> Ui_effect.Ignore)
        )
      in

      (* Discard phase *)
      let on_discard_click _ev =
        if Poly.(st.phase <> T.Discard) || not (human_turn ())
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
          padding:20px;
        |} in

        let start_game vs_flag _ev =
          let st0 = initial_state ~vs_computer:vs_flag () in
          Ui_effect.Many
            [ set_vs vs_flag
            ; set_all_full
                ~screen':`Playing
                ~vs_comp':vs_flag
                ~st':st0
                ~selected':[]
                ~hist':[]
                ~popup':false
            ]
        in

        let go_tutorial _ev =
          set_all_full
            ~screen':`Tutorial
            ~vs_comp':vs_comp
            ~st':st
            ~selected':selected_idxs
            ~hist':hist
            ~popup':false
        in

        Vdom.Node.div
          ~attrs:[ style page_css ]
          [ Vdom.Node.h1
              ~attrs:[ style "font-size:32px;margin:0;" ]
              [ Vdom.Node.text "Rummy" ]
          ; Vdom.Node.p [ Vdom.Node.text "Choose a mode to begin:" ]
          ; Vdom.Node.div
              [ Vdom.Node.button
                  ~attrs:[ Vdom.Attr.on_click (start_game false); style btn_css ]
                  [ Vdom.Node.text "Pass & Play" ]
              ; Vdom.Node.button
                  ~attrs:[ Vdom.Attr.on_click (start_game true); style btn_css ]
                  [ Vdom.Node.text "Play vs Computer" ]
              ]
          ; Vdom.Node.hr ()
          ; Vdom.Node.button
              ~attrs:[ Vdom.Attr.on_click go_tutorial; style btn_css ]
              [ Vdom.Node.text "Tutorial" ]
          ]

      | `Tutorial ->
        let page_css = {|
          font-family:sans-serif;color:white;background:#000;min-height:100vh;
          padding:20px;display:flex;flex-direction:column;align-items:center;gap:16px;
        |} in
        let text_css = {|
          white-space:pre-wrap;background:#111;border:1px solid #333;border-radius:8px;
          padding:14px;max-width:900px;line-height:1.35;
        |} in
        let back _ev =
          set_all_full
            ~screen':`Intro ~vs_comp':vs_comp ~st':st
            ~selected':selected_idxs ~hist':hist ~popup':false
        in
        Vdom.Node.div
          ~attrs:[ style page_css ]
          [ Vdom.Node.h2 [ Vdom.Node.text "How to Play" ]
          ; Vdom.Node.div ~attrs:[ style text_css ] [ Vdom.Node.text tutorial_text ]
          ; Vdom.Node.button ~attrs:[ style btn_css; Vdom.Attr.on_click back ] [ Vdom.Node.text "Back" ]
          ]

      | `Playing ->
        let screen_css = {|
          display:flex;flex-direction:column;min-height:100vh;
          background-color:#000;color:white;font-family:sans-serif;
        |} in
        let middle_wrap_css = {|
          flex-grow:1;display:flex;flex-direction:column;align-items:center;
          justify-content:center;background-color:#0f0f0f;color:white;padding:20px;
        |} in
        let table_css = {|
          display:flex;flex-direction:row;align-items:flex-start;justify-content:center;
          gap:40px;padding:20px;background-color:#1a1a1a;border:2px solid #333;
          border-radius:8px;box-shadow:0px 4px 10px rgba(0,0,0,0.6);
        |} in
        let bottom_css = {|
          background-color:#000;border-top:2px solid #333;padding:10px;
          color:white;text-align:center;
        |} in
        let hand_title_css = {|
          color:white;font-size:16px;margin-bottom:6px;
        |} in
        let controls_row_css = {|
          display:flex;flex-direction:row;flex-wrap:wrap;justify-content:center;
          gap:10px;margin-top:8px;
        |} in
        let btn_small_css = {|
          padding:6px 10px;cursor:pointer;background-color:#eee;color:black;
          border-radius:4px;border:1px solid #666;font-size:14px;
        |} in
        let endcheck_wrap_css = {|
          display:flex;flex-direction:column;align-items:center;justify-content:center;
          gap:10px;margin-top:8px;text-align:center;
        |} in

        (* phase_controls depends on st.phase *)
        let phase_controls =
          match st.phase with
          | T.Draw ->
            Vdom.Node.div
              [ Vdom.Node.div
                  ~attrs:[ style "color:white;font-size:14px;margin-top:8px;" ]
                  [ Vdom.Node.text
                      "Draw: click the Deck (top only), the Discard stack (top only), or a card icon in the bar to take that card and all above it." ]
              ]

          | T.Play ->
            Vdom.Node.div
              ~attrs:[ style controls_row_css ]
              [ Vdom.Node.button
                  ~attrs:[ Vdom.Attr.on_click on_sort_hand; style btn_small_css ]
                  [ Vdom.Node.text "SORT HAND" ]
              ; Vdom.Node.button
                  ~attrs:[ Vdom.Attr.on_click on_meld_selected; style btn_small_css ]
                  [ Vdom.Node.text "MELD THESE CARDS" ]
              ; Vdom.Node.button
                  ~attrs:[ Vdom.Attr.on_click on_skip_to_discard; style btn_small_css ]
                  [ Vdom.Node.text "SKIP TO DISCARD PHASE" ]
              ]

          | T.Discard ->
            Vdom.Node.div
              ~attrs:[ style controls_row_css ]
              [ Vdom.Node.button
                  ~attrs:[ Vdom.Attr.on_click on_discard_click; style btn_small_css ]
                  [ Vdom.Node.text "DISCARD" ]
              ]

          | T.EndCheck ->
            let pass_msg =
              if not vs_comp then
                Vdom.Node.p
                  ~attrs:[ style "color:red;font-size:14px;margin-top:8px;" ]
                  [ Vdom.Node.text "Please pass your device to the next player." ]
              else
                Vdom.Node.none
            in
            Vdom.Node.div
              ~attrs:[ style endcheck_wrap_css ]
              [ pass_msg
              ; Vdom.Node.button
                  ~attrs:[ Vdom.Attr.on_click on_end_check; style btn_small_css ]
                  [ Vdom.Node.text "CONTINUE" ]
              ]
        in

        (* active player's info *)
        let active_hand = st.players.(st.current).hand in
        let active_name = st.players.(st.current).name in

        let top_area =
          Vdom.Node.div [ view_status_top ~st; view_melds_top st.melds ]
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
                    ~on_click_stack:on_click_discard_stack
                    ~on_click_bar:on_click_discard_bar
                ]
            ]
        in

        (* bottom_area hides cards during EndCheck in pass & play *)
        let bottom_area =
          let hand_area_node =
            if hide_hand_now () then
              let hidden_css = {|
                display:flex;align-items:center;justify-content:center;min-height:140px;
                background-color:#0a0a0a;border-top:2px solid #333;color:#888;
                font-family:sans-serif;font-size:14px;padding:10px;
              |} in
              Vdom.Node.div
                ~attrs:[ style hidden_css ]
                [ Vdom.Node.text "Hand hidden. Please pass your device." ]
            else
              view_hand_row
                ~hand:active_hand
                ~selected_idxs:selected_idxs
                ~on_click_card:(fun i ->
                  if human_turn () && (not (hide_hand_now ()))
                  then toggle_card_index i
                  else Ui_effect.Ignore)
          in
          Vdom.Node.div
            ~attrs:[ style bottom_css ]
            [ Vdom.Node.h3 ~attrs:[ style hand_title_css ]
                [ Vdom.Node.text (active_name ^ "'s Hand") ]
            ; hand_area_node
            ; phase_controls
            ]
        in

        let undo_button =
          match hist with
          | _ :: _ ->
            Vdom.Node.div
              ~attrs:[ style "padding:10px;text-align:center;" ]
              [ Vdom.Node.button
                  ~attrs:[ Vdom.Attr.on_click on_back; style btn_small_css ]
                  [ Vdom.Node.text "⬅ UNDO" ]
              ]
          | [] -> Vdom.Node.none
        in

        let modal =
          if show_popup then
            let msg =
              "You took multiple cards from the discard pile. \
               You must use the BOTTOM-MOST picked card in a meld \
               this turn before you can go to the discard phase."
            in
            view_modal ~message:msg ~on_close:(fun _ -> set_popup false)
          else Vdom.Node.none
        in

        Vdom.Node.div
          ~attrs:[ style screen_css ]
          [ top_area; middle_area; bottom_area; undo_button; modal ]
    )

let () =
  Bonsai_web.Start.start
    ~bind_to_element_with_id:"app"
    component