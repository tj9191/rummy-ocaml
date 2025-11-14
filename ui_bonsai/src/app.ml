open! Core
open! Bonsai
open! Bonsai_web
open! Async_kernel

module Vdom     = Virtual_dom.Vdom
module RE       = Rummy_engine
module T        = RE.Types
module E        = Rummy_engine.Engine
module Setup    = RE.Setup
module Firebase = RE.Firebase_client

let online_mode = ref false
let my_index    = ref 0

(* No lobby_id argument anymore – single fixed document *)
let push_state_effect =
  Effect.of_deferred_fun (fun (st : T.state) ->
      Firebase.push_state st
    )

let pull_state_effect =
  Effect.of_deferred_fun (fun () ->
      Firebase.pull_state ()
    )
  

let () = Random.self_init ()
let deck_depletion_count = ref 0


(* ---------- helpers ---------- *)


let string_of_card (c : T.card) = RE.Types.string_of_card c
let string_of_phase = function
  | T.Draw     -> "Draw"
  | T.Play     -> "Play"
  | T.Discard  -> "Discard"
  | T.EndCheck -> "EndCheck"

(* scoring helper *)
let points_of_rank (r : T.rank) : int =
  match r with
  | T.Ace -> 15
  | T.King | T.Queen | T.Jack | T.Ten -> 10
  | _ -> 5

let points_of_cards (cs : T.card list) : int =
  List.fold cs ~init:0 ~f:(fun acc c -> acc + points_of_rank c.rank)

(* penalty = deadwood in hand *)
let penalty_of_hand (hand : T.card list) : int =
  List.fold hand ~init:0 ~f:(fun acc c -> acc + points_of_rank c.rank)

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
      let ra = rank_order a.rank
      and rb = rank_order b.rank in
      if Int.(ra <> rb) then Int.compare ra rb
      else Int.compare (suit_order a.suit) (suit_order b.suit))

let shuffle_with_state (st : Random.State.t) (xs : 'a list) : 'a list =
  let a = Array.of_list xs in
  for i = Array.length a - 1 downto 1 do
    let j = Random.State.int st (i + 1) in
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t
  done;
  Array.to_list a

let deal_n k (deck : T.card list) : T.card list * T.card list =
  let rec loop n acc d =
    if n = 0 then (List.rev acc, d)
    else
      match d with
      | [] -> (List.rev acc, [])
      | x :: xs -> loop (n - 1) (x :: acc) xs
  in
  loop k [] deck

(* ---------- initial state ---------- *)

let initial_state ~vs_computer () : T.state =
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

(* deep-copy the game state so undo really restores it *)
let copy_state (st : T.state) : T.state =
  { st with
    players =
      Array.map st.players ~f:(fun p ->
          { p with hand = List.map p.hand ~f:(fun c -> c) });
    scores  = Array.copy st.scores;
    melds   =
      List.map st.melds ~f:(fun m ->
          { m with cards = List.map m.cards ~f:(fun c -> c) });
    deck    = List.map st.deck ~f:(fun c -> c);
    discard = List.map st.discard ~f:(fun c -> c);
  }

(* ---------- style helper ---------- *)
let style (s : string) : Vdom.Attr.t = Vdom.Attr.create "style" s

(* ---------- view helpers ---------- *)

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

let view_discard_pile
    ~(discard : T.card list)
    ~on_click_stack
    ~on_click_bar
  =
  (* changed to width:100% and max-width for mobile *)
  let outer_css = {|
    display:flex;flex-direction:column;align-items:center;
    font-family:sans-serif;
    width:100%;max-width:360px;
  |} in
  if List.is_empty discard then
    Vdom.Node.div
      ~attrs:[ style outer_css ]
      [ Vdom.Node.div
          ~attrs:[ style {|
            width:120px;
            height:100px;
            display:flex;
            align-items:center;
            justify-content:center;
            border:2px dashed #555;
            border-radius:6px;
            color:#777;
            text-align:center;
            font-size:13px;
            line-height:100px;
          |} ]
          [ Vdom.Node.text "Discard (empty)" ] ]
  else
    let n = List.length discard in
    let top_card = List.hd_exn discard in
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
            let css =
              Printf.sprintf
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
    let bar_css = {|
      display:flex;flex-direction:row;justify-content:center;flex-wrap:wrap;
      gap:4px;margin-bottom:6px;max-width:360px;
    |} in
    let rank_text = function
      | T.Two -> "2"
      | Three -> "3"
      | Four -> "4"
      | Five -> "5"
      | Six -> "6"
      | Seven -> "7"
      | Eight -> "8"
      | Nine -> "9"
      | Ten -> "10"
      | Jack -> "J"
      | Queen -> "Q"
      | King -> "K"
      | Ace -> "A"
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

(* ---------- top status (with points, aware of vs_comp) ---------- *)

let view_status_top ~(st : T.state) ~(vs_comp : bool) =
  let row_css = {|
    display:flex;
    flex-direction:row;
    flex-wrap:wrap;
    justify-content:space-between;
    align-items:center;
    padding:10px;
    background-color:#000;
    color:white;
    font-family:sans-serif;
    font-size:14px;
    border-bottom:2px solid #333;
  |} in
  let current_p = st.players.(st.current) in
  let p2_label = if vs_comp then "Computer points" else "Player 2 points" in
  Vdom.Node.div
    ~attrs:[ style row_css ]
    [ Vdom.Node.div
        [ Vdom.Node.text
            (Printf.sprintf "Current: %s (phase: %s)"
               current_p.name (string_of_phase st.phase))
        ]
    ; Vdom.Node.div
        [ Vdom.Node.text
            (Printf.sprintf
               "Deck: %d | Discard: %d | Player 1 points: %d | %s: %d"
               (List.length st.deck)
               (List.length st.discard)
               st.scores.(0)
               p2_label
               st.scores.(1))
        ]
    ]

(* ---------- melds (right panel) with LAY HERE ---------- *)
let view_melds_right_panel ~(melds : T.meld list) ~(on_layoff : int -> unit Ui_effect.t) =
  (* made width:100% and added mobile-full class from our global style *)
  let outer_css = {|
    background:#111;
    border:1px solid #333;
    border-radius:8px;
    padding:10px 12px;
    align-self:flex-start;
    max-width:360px;
    width:100%;
  |} in
  let title_css = {|
    margin:0 0 6px 0;
    font-size:14px;
    font-weight:700;
  |} in
  let row_css = {|
    display:flex;
    flex-direction:row;
    justify-content:space-between;
    gap:6px;
    align-items:center;
    margin-bottom:4px;
  |} in
  let btn_css = {|
    padding:2px 6px;
    font-size:11px;
    background:#eee;
    color:#000;
    border-radius:4px;
    border:1px solid #666;
    cursor:pointer;
  |} in
  let body =
    if List.is_empty melds then
      [ Vdom.Node.div ~attrs:[ style "color:#ddd;font-size:12px;" ]
          [ Vdom.Node.text "No melds on table" ] ]
    else
      List.mapi melds ~f:(fun i m ->
          let kind_str = match m.kind with T.Set -> "Set" | T.Run -> "Run" in
          let cards_str =
            m.cards |> List.map ~f:string_of_card |> String.concat ~sep:", "
          in
          Vdom.Node.div
            ~attrs:[ style row_css ]
            [ Vdom.Node.div
                [ Vdom.Node.text (Printf.sprintf "%d) %s: %s" (i+1) kind_str cards_str) ]
            ; Vdom.Node.button
                ~attrs:[ style btn_css; Vdom.Attr.on_click (fun _ -> on_layoff i) ]
                [ Vdom.Node.text "LAY HERE" ]
            ])
  in
  Vdom.Node.div
    ~attrs:[ style outer_css; Vdom.Attr.class_ "mobile-full" ]
    ([ Vdom.Node.h3 ~attrs:[ style title_css ]
         [ Vdom.Node.text "Melds (click LAY HERE to add 1 selected card)" ] ]
     @ body)

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
        ; Vdom.Node.button ~attrs:[ style btn; Vdom.Attr.on_click on_close ]
            [ Vdom.Node.text "OK" ]
        ]
    ]

let tutorial_text : string = {|
Here are the rules for this version of Rummy.

──────────────────────────────
GOAL OF THE GAME
──────────────────────────────
Your goal is to earn the most points by forming “melds” — valid combinations of cards —
and reducing the total value of unmelded cards left in your hand.

A game ends when one player uses all their cards, either by melding or discarding their last one.

In this version, if a player uses all their cards without needing to discard, that also ends the round — this is called a “rummy without a discard.”

──────────────────────────────
CARD VALUES
──────────────────────────────
Each card has a point value:
• 2 through 9 → 5 points
• 10, Jack, Queen, King → 10 points
• Ace → 15 points

These points are used both for scoring melds and for penalties at the end of the round.

Aces are always high in runs (so Q–K–A is valid, but A–2–3 is not).

──────────────────────────────
MELDS
──────────────────────────────
A “meld” is a group of cards that forms either a set or a run:

• SET → 3 or more cards of the same rank (e.g. 7♠ 7♦ 7♣)
• RUN → 3 or more consecutive cards of the same suit (e.g. 5♥ 6♥ 7♥)

You can also add (lay off) cards onto existing melds if they fit logically.
For example, if a meld of 7♠ 8♠ 9♠ is already on the table, you can lay off 6♠ or 10♠.

You can only lay off one card at a time by clicking “LAY HERE.”
Aces cannot be placed below a 2 in a run, since they are treated as high only.

──────────────────────────────
TURN STRUCTURE
──────────────────────────────
Each turn has three phases:
	1.	DRAW
Choose one:
-Draw the top card from the Deck
-OR draw the top card from the Discard Pile
-OR draw MULTIPLE cards from the Discard Pile
If you take multiple cards from the discard pile, you must use the bottom-most one in a meld before you can discard.
If you forget to use the bottom-most card, a reminder will appear and you will not be able to discard until you do.
	2.	PLAY
	•	Make a meld with selected cards from your hand (choose 3+ and click “MELD THESE CARDS”),
	•	Add a single card onto an existing meld by clicking “LAY HERE,”
	•	Sort your hand, or
	•	When done, click “DISCARD” to move to the next phase.
	3.	DISCARD
	•	Choose one card to discard, ending your turn.
	•	The turn passes to the other player (or the computer).

You can undo your last action during a turn using the “UNDO” button (except immediately after drawing a card).

──────────────────────────────
END OF GAME
──────────────────────────────
The game ends when:
	•	A player discards their final card (a “rummy”), or
	•	A player goes out without a discard (“rummy without a discard”), or
	•	The deck runs out twice in one game.

Each player’s total score is then updated:
	•	Points for all melded cards are added.
	•	Points for remaining unmelded cards (deadwood) are subtracted.

If the deck runs out twice, the game ends in a stalemate. Both players lose points equal to the value of their remaining unmelded cards.

──────────────────────────────
WINNING
──────────────────────────────
The player with the higher score at the end of the final round wins the game.

In a stalemate, the player with the higher remaining score still wins.
A “winner” message will appear at the end of the round, showing both players’ point totals.
|}

(* --- last-move parsing --- *)

let picked_up_from_last_move (s : string) : string =
  let parts = String.split ~on:'|' s |> List.map ~f:String.strip in
  let rec find_last_draw = function
    | [] -> "—"
    | x :: xs_rev ->
      if String.is_prefix x ~prefix:"Drew: Discard (top)" then x
      else if String.is_prefix x ~prefix:"Drew: Discard (multi" then x
      else if String.is_prefix x ~prefix:"Drew: Deck" then "Hidden Card"
      else find_last_draw xs_rev
  in
  find_last_draw (List.rev parts)

let discarded_from_last_move (s : string) : string =
  let parts = String.split ~on:'|' s |> List.map ~f:String.strip in
  let rec find_last_discard = function
    | [] -> "—"
    | x :: xs_rev ->
      if String.is_prefix x ~prefix:"Discarded:" then
        String.drop_prefix x (String.length "Discarded: ")
      else
        find_last_discard xs_rev
  in
  if String.equal s "Rummy without a discard" then
    "Rummy without a discard"
  else
    find_last_discard (List.rev parts)

(* ---------- main component ---------- *)

let component graph =
  let rng = Random.State.make_self_init () in

  let state =
    Bonsai.state
      ( `Intro
      , false
      , initial_state ~vs_computer:false ()
      , ([] : int list)
      , ([] : (T.state * bool * string array) list)
      , false
      , false
      , [| "—"; "—" |]
      , true
      , (None : string option)
      , (None : string option)  (* lobby_id_opt *)
      , 0                       (* my_player_index *)
      )
      graph
  in

  Bonsai.Value.map state
    ~f:(fun ((screen, vs_comp, st, selected_idxs, hist,
              show_popup, last_draw_multi, last_moves, undo_allowed,
              winner_msg, lobby_id_opt, my_player_index),
             set_all) ->

      let set_all_full
          ~screen' ~vs_comp' ~st' ~selected' ~hist' ~popup'
          ~last_draw_multi' ~last_moves' ~undo_allowed'
          ~winner_msg'
        =
        let new_model =
          ( screen'
          , vs_comp'
          , st'
          , selected'
          , hist'
          , popup'
          , last_draw_multi'
          , last_moves'
          , undo_allowed'
          , winner_msg'
          , lobby_id_opt      (* keep same lobby id for now *)
          , my_player_index   (* keep same player index *)
          )
        in
        Ui_effect.Many
          [ set_all new_model
          ; push_state_effect st'
          ]
      in

      let rec maybe_reshuffle_or_stalemate
          ~(screen : [> `Intro ])
          ~(vs_comp : bool)
          ~(hist : (T.state * bool * string array) list)
          ~(last_moves : string array)
          ~(set_all_full :
              screen':[> `Intro ] ->
              vs_comp':bool ->
              st':T.state ->
              selected':int list ->
              hist':(T.state * bool * string array) list ->
              popup':bool ->
              last_draw_multi':bool ->
              last_moves':string array ->
              undo_allowed':bool ->
              winner_msg':string option ->
              unit Ui_effect.t)
          (st' : T.state)
        : T.state option
        =
        if not (List.is_empty st'.deck) then
          Some st'
        else (
          incr deck_depletion_count;
          if !deck_depletion_count = 1 then (
            let discard_cards = st'.discard in
            if List.is_empty discard_cards then
              Some { st' with deck = [] }
            else
              let rng = Random.State.make_self_init () in
              let new_deck = shuffle_with_state rng discard_cards in
              Some { st' with deck = new_deck; discard = [] }
          ) else (
            deck_depletion_count := 0;
            let scores' = Array.copy st'.scores in
            let p0_hand = st'.players.(0).hand in
            let p1_hand = st'.players.(1).hand in
            let p0_pen = penalty_of_hand p0_hand in
            let p1_pen = penalty_of_hand p1_hand in
            scores'.(0) <- Int.max 0 (scores'.(0) - p0_pen);
            scores'.(1) <- Int.max 0 (scores'.(1) - p1_pen);

            let p0_name = st'.players.(0).name in
            let p1_name = st'.players.(1).name in

            let msg =
              if scores'.(0) > scores'.(1) then
                Printf.sprintf
                  "Stalemate. %s wins! %s: %d | %s: %d"
                  p0_name p0_name scores'.(0) p1_name scores'.(1)
              else if scores'.(1) > scores'.(0) then
                Printf.sprintf
                  "Stalemate. %s wins! %s: %d | %s: %d"
                  p1_name p0_name scores'.(0) p1_name scores'.(1)
              else
                Printf.sprintf
                  "Stalemate. Tie! %s: %d | %s: %d"
                  p0_name scores'.(0) p1_name scores'.(1)
            in

            let _ =
              set_all_full
                ~screen':screen
                ~vs_comp':vs_comp
                ~st':{ st' with scores = scores' }
                ~selected':[]
                ~hist':hist
                ~popup':false
                ~last_draw_multi':false
                ~last_moves':last_moves
                ~undo_allowed':false
                ~winner_msg':(Some msg)
            in
            None
          )
        )
      in

      let set_vs b =
        set_all
          ( screen
          , b
          , st
          , selected_idxs
          , hist
          , show_popup
          , last_draw_multi
          , last_moves
          , undo_allowed
          , winner_msg
          , lobby_id_opt
          , my_player_index
          )
      in

      let set_selected sel =
        set_all
          ( screen
          , vs_comp
          , st
          , sel
          , hist
          , show_popup
          , last_draw_multi
          , last_moves
          , undo_allowed
          , winner_msg
          , lobby_id_opt
          , my_player_index
          )
      in

      let set_popup b =
        set_all
          ( screen
          , vs_comp
          , st
          , selected_idxs
          , hist
          , b
          , last_draw_multi
          , last_moves
          , undo_allowed
          , winner_msg
          , lobby_id_opt
          , my_player_index
          )
      in

      let update_last_move_for_player player_idx msg =
        let arr = Array.copy last_moves in
        arr.(player_idx) <- msg;
        arr
      in

      let toggle_card_index i =
        let sel =
          if List.mem selected_idxs i ~equal:Int.equal then
            List.filter selected_idxs ~f:(fun j -> j <> i)
          else
            i :: selected_idxs
        in
        set_selected sel
      in

      (* ===== AI one-step ===== *)
      let ai_step_once
          ~(screen : [> `Intro ])
          ~(vs_comp : bool)
          ~(hist : (T.state * bool * string array) list)
          ~(last_moves : string array)
          ~(set_all_full :
              screen':[> `Intro ] ->
              vs_comp':bool ->
              st':T.state ->
              selected':int list ->
              hist':(T.state * bool * string array) list ->
              popup':bool ->
              last_draw_multi':bool ->
              last_moves':string array ->
              undo_allowed':bool ->
              winner_msg':string option ->
              unit Ui_effect.t)
          (st_ai : T.state)
        : (T.state * string option * T.card list option) option
        =
        match st_ai.T.phase with
        | T.Draw ->
            let discard_nonempty = not (List.is_empty st_ai.T.discard) in
            let coin = Random.State.int rng 2 in

            let try_deck () =
              match E.draw ~source:T.FromDeck st_ai with
              | Ok st' | End_round st' ->
                (match maybe_reshuffle_or_stalemate
                         ~screen
                         ~vs_comp
                         ~hist
                         ~last_moves
                         ~set_all_full
                         st'
                 with
                 | None -> None
                 | Some st_after ->
                   Some (st_after, Some "Drew: Deck", None))
              | Error _ -> None
            in

            let try_discard () =
              match st_ai.T.discard with
              | top :: _ ->
                (match E.draw ~source:T.FromDiscard st_ai with
                 | Ok st' | End_round st' ->
                   let msg =
                     Printf.sprintf "Drew: Discard (top) %s" (string_of_card top)
                   in
                   Some (st', Some msg, None)
                 | Error _ -> None)
              | [] -> None
            in

            if coin = 0 then
              match try_deck () with
              | Some r -> Some r
              | None ->
                if discard_nonempty then try_discard () else None
            else
              if discard_nonempty then
                match try_discard () with
                | Some r -> Some r
                | None -> try_deck ()
              else
                try_deck ()
        | T.Play ->
            let p = st_ai.players.(st_ai.current) in
            let rec combinations_k k xs =
              if k = 0 then [ [] ]
              else
                match xs with
                | [] -> []
                | y :: ys ->
                  let with_y =
                    List.map (combinations_k (k - 1) ys) ~f:(fun rest -> y :: rest)
                  in
                  let without = combinations_k k ys in
                  with_y @ without
            in
            let all_3plus hand =
              let n = List.length hand in
              List.concat_map (List.range 3 (n + 1)) ~f:(fun k ->
                  combinations_k k hand)
            in
            let combos = all_3plus p.hand in
            let rec try_melds = function
              | [] -> None
              | cs :: rest ->
                (match E.play ~action:(T.Make_set cs) st_ai with
                 | Ok st' | End_round st' -> Some (st', None, Some cs)
                 | Error _ ->
                   (match E.play ~action:(T.Make_run cs) st_ai with
                    | Ok st' | End_round st' -> Some (st', None, Some cs)
                    | Error _ -> try_melds rest))
            in
            (match try_melds combos with
             | Some r -> Some r
             | None ->
               (match E.play ~action:T.Skip_to_discard st_ai with
                | Ok st' | End_round st' -> Some (st', None, None)
                | Error _ -> None))
        | T.Discard ->
            let p = st_ai.players.(st_ai.current) in
            (match p.hand with
             | [] -> None
             | hand ->
               let idx = Random.State.int rng (List.length hand) in
               let card = List.nth_exn hand idx in
               (match E.discard ~action:(T.Discard_card card) st_ai with
                | Ok st' | End_round st' ->
                  let msg = Printf.sprintf "Discarded: %s" (string_of_card card) in
                  Some (st', Some msg, None)
                | Error _ -> None))
        | T.EndCheck ->
            None
      in

      (* AI driver: do AI until human again *)
      let rec advance_ai_turn
          (st_ai : T.state)
          (lm_ai : string array)
        : T.state * string array
        =
        if st_ai.T.current = 0 then
          let st_ai =
            match st_ai.T.phase with
            | T.Draw -> st_ai
            | _ -> { st_ai with phase = T.Draw }
          in
          let lm_ai =
            let arr = Array.copy lm_ai in
            if String.equal arr.(1) "—" then
              arr.(1) <- "Computer: turn ended";
            arr
          in
          (st_ai, lm_ai)
        else
          match st_ai.T.phase with
          | T.EndCheck ->
            (match E.endcheck st_ai with
             | Ok st' | End_round st' -> advance_ai_turn st' lm_ai
             | Error _ ->
               ({ st_ai with current = 0; phase = T.Draw }, lm_ai))
          | T.Draw | T.Play | T.Discard ->
            (match ai_step_once
                     ~screen
                     ~vs_comp
                     ~hist:[]
                     ~last_moves:lm_ai
                     ~set_all_full
                     st_ai
             with
             | Some (st', maybe_msg, maybe_melded) ->
               let st' =
                 match maybe_melded with
                 | None -> st'
                 | Some cards ->
                   let pts = points_of_cards cards in
                   let scores' = Array.copy st'.scores in
                   scores'.(1) <- scores'.(1) + pts;
                   { st' with scores = scores' }
               in
               let lm' =
                 match maybe_msg with
                 | None -> lm_ai
                 | Some msg ->
                   let arr = Array.copy lm_ai in
                   let old = arr.(1) in
                   let combined =
                     if String.equal old "—" then msg else old ^ " | " ^ msg
                   in
                   arr.(1) <- combined;
                   arr
               in
               advance_ai_turn st' lm'
             | None ->
               (st_ai, lm_ai))
      in

      (* ---------- end-of-round / winner ---------- *)
      let end_round_with_winner ~(winner_idx:int) (st_after : T.state) =
        let loser_idx = 1 - winner_idx in
        let loser_hand = st_after.players.(loser_idx).hand in
        let penalty = penalty_of_hand loser_hand in
        let scores' = Array.copy st_after.scores in
        let loser_new = Int.max 0 (scores'.(loser_idx) - penalty) in
        scores'.(loser_idx) <- loser_new;
        let winner_name = st_after.players.(winner_idx).name in
        let msg =
          Printf.sprintf
            "%s wins! Player 1 points: %d | %s: %d"
            winner_name
            scores'.(0)
            st_after.players.(1).name
            scores'.(1)
        in
        set_all_full
          ~screen':screen
          ~vs_comp':vs_comp
          ~st':{ st_after with scores = scores' }
          ~selected':[]
          ~hist':hist
          ~popup':false
          ~last_draw_multi':false
          ~last_moves':last_moves
          ~undo_allowed':false
          ~winner_msg':(Some msg)
      in

      (* ---------- apply_and_maybe_ai ---------- *)
      let apply_and_maybe_ai
          ?(last_multi = false)
          ?(last_moves_opt = None)
          ?(undo_allowed_opt = None)
          (st_after : T.state)
        =
        let hist' =
          (copy_state st, last_draw_multi, Array.copy last_moves) :: hist
        in
        let ai_ran, final_state, final_last_moves =
          if vs_comp && st_after.T.current = 1 then
            let (ai_st, ai_lm) = advance_ai_turn st_after last_moves in
            (true, ai_st, ai_lm)
          else
            (false, st_after,
             Option.value last_moves_opt ~default:last_moves)
        in
        let p0_empty = List.is_empty final_state.players.(0).hand in
        let p1_empty = List.is_empty final_state.players.(1).hand in
        if p0_empty && not p1_empty then
          end_round_with_winner ~winner_idx:0 final_state
        else if p1_empty && not p0_empty then
          end_round_with_winner ~winner_idx:1 final_state
        else
          let lm' =
            if ai_ran then final_last_moves
            else Option.value last_moves_opt ~default:final_last_moves
          in
          let undo_allowed' = Option.value undo_allowed_opt ~default:true in
          set_all_full
            ~screen':screen
            ~vs_comp':vs_comp
            ~st':final_state
            ~selected':[]
            ~hist':hist'
            ~popup':false
            ~last_draw_multi':last_multi
            ~last_moves':lm'
            ~undo_allowed':undo_allowed'
            ~winner_msg':None
      in

      (* ---------- UNDO ---------- *)
      let on_back _ev =
        if not undo_allowed then Ui_effect.Ignore
        else
          match hist with
          | (prev_st, prev_last_draw_multi, prev_last_moves) :: rest ->
            set_all_full
              ~screen':screen
              ~vs_comp':vs_comp
              ~st':prev_st
              ~selected':[]
              ~hist':rest
              ~popup':false
              ~last_draw_multi':prev_last_draw_multi
              ~last_moves':prev_last_moves
              ~undo_allowed':true
              ~winner_msg':winner_msg
          | [] ->
            Ui_effect.Ignore
      in

      let human_turn () =
        (* Online: only the local player (my_index) may act.
           Local: old behavior. *)
        if !online_mode then
          st.current = !my_index
        else if vs_comp then
          st.current = 0
        else
          true
      in
      let hide_hand_now () = (not vs_comp) && Poly.(st.phase = T.EndCheck) in

      let current_player = st.players.(st.current) in

      let selected_cards_from_hand () : T.card list option =
        let hand = current_player.hand in
        let max_i = List.length hand - 1 in
        if List.exists selected_idxs ~f:(fun i -> i < 0 || i > max_i)
        then None
        else
          Some
            (List.map
               (List.sort selected_idxs ~compare:Int.compare)
               ~f:(fun i -> List.nth_exn hand i))
      in

      (* ====== CLICK HANDLERS ====== *)

      let on_click_deck _ev =
        if Poly.(st.phase <> T.Draw) || not (human_turn ()) then
          Ui_effect.Ignore
        else
          let hist' =
            (copy_state st, last_draw_multi, Array.copy last_moves) :: hist
          in
          match E.draw ~source:T.FromDeck st with
          | Error _ ->
            Ui_effect.Ignore
          | Ok st' | End_round st' ->
            (match maybe_reshuffle_or_stalemate
                     ~screen
                     ~vs_comp
                     ~hist:hist'
                     ~last_moves
                     ~set_all_full
                     st'
             with
             | None ->
               Ui_effect.Ignore
             | Some st_after ->
               let prev = last_moves.(st.current) in
               let lm =
                 update_last_move_for_player st.current
                   (if String.equal prev "—" then "Drew: Deck"
                    else prev ^ " | Drew: Deck")
               in
               let _undo_allowed' = false in
               let ai_ran, final_state, final_last_moves =
                 if vs_comp && st_after.T.current = 1 then
                   let (ai_st, ai_lm) = advance_ai_turn st_after last_moves in
                   (true, ai_st, ai_lm)
                 else
                   (false, st_after, lm)
               in
               let p0_empty = List.is_empty final_state.players.(0).hand in
               let p1_empty = List.is_empty final_state.players.(1).hand in
               if p0_empty && not p1_empty then
                 end_round_with_winner ~winner_idx:0 final_state
               else if p1_empty && not p0_empty then
                 end_round_with_winner ~winner_idx:1 final_state
               else
                 let lm' = if ai_ran then final_last_moves else lm in
                 set_all_full
                   ~screen':screen
                   ~vs_comp':vs_comp
                   ~st':final_state
                   ~selected':[]
                   ~hist':hist'
                   ~popup':false
                   ~last_draw_multi':false
                   ~last_moves':lm'
                   ~undo_allowed':false
                   ~winner_msg':None)
      in

      let on_click_discard_stack _ev =
        if Poly.(st.phase <> T.Draw) || not (human_turn ()) then
          Ui_effect.Ignore
        else
          let hist' =
            (copy_state st, last_draw_multi, Array.copy last_moves) :: hist
          in
          let picked_card_str =
            match st.discard with
            | top :: _ -> Printf.sprintf "Drew: Discard (top) %s" (string_of_card top)
            | [] -> "Drew: Discard (top)"
          in
          match E.draw ~source:T.FromDiscard st with
          | Ok st' | End_round st' ->
            let lm =
              update_last_move_for_player st.current picked_card_str
            in
            let _undo_allowed' = true in
            let ai_ran, final_state, final_last_moves =
              if vs_comp && st'.T.current = 1 then
                let (ai_st, ai_lm) = advance_ai_turn st' last_moves in
                (true, ai_st, ai_lm)
              else
                (false, st', lm)
            in
            let p0_empty = List.is_empty final_state.players.(0).hand in
            let p1_empty = List.is_empty final_state.players.(1).hand in
            if p0_empty && not p1_empty then
              end_round_with_winner ~winner_idx:0 final_state
            else if p1_empty && not p0_empty then
              end_round_with_winner ~winner_idx:1 final_state
            else
              let lm' = if ai_ran then final_last_moves else lm in
              set_all_full
                ~screen':screen
                ~vs_comp':vs_comp
                ~st':final_state
                ~selected':[]
                ~hist':hist'
                ~popup':false
                ~last_draw_multi':false
                ~last_moves':lm'
                ~undo_allowed':true
                ~winner_msg':None
          | Error _ ->
            Ui_effect.Ignore
      in

      let on_click_discard_bar i_from_top _ev =
        if Poly.(st.phase <> T.Draw) || not (human_turn ()) then
          Ui_effect.Ignore
        else
          let hist' =
            (copy_state st, last_draw_multi, Array.copy last_moves) :: hist
          in
          match E.draw ~source:(T.FromDiscardN (i_from_top + 1)) st with
          | Ok st' | End_round st' ->
            let count = i_from_top + 1 in
            let msg = Printf.sprintf "Drew: Discard (multi %d)" count in
            let prev = last_moves.(st.current) in
            let lm =
              update_last_move_for_player st.current
                (if String.equal prev "—" then msg else prev ^ " | " ^ msg)
            in
            let last_multi' = true in
            let _undo_allowed' = true in
            let ai_ran, final_state, final_last_moves =
              if vs_comp && st'.T.current = 1 then
                let (ai_st, ai_lm) = advance_ai_turn st' last_moves in
                (true, ai_st, ai_lm)
              else
                (false, st', lm)
            in
            let p0_empty = List.is_empty final_state.players.(0).hand in
            let p1_empty = List.is_empty final_state.players.(1).hand in
            if p0_empty && not p1_empty then
              end_round_with_winner ~winner_idx:0 final_state
            else if p1_empty && not p0_empty then
              end_round_with_winner ~winner_idx:1 final_state
            else
              let lm' = if ai_ran then final_last_moves else lm in
              set_all_full
                ~screen':screen
                ~vs_comp':vs_comp
                ~st':final_state
                ~selected':[]
                ~hist':hist'
                ~popup':false
                ~last_draw_multi':last_multi'
                ~last_moves':lm'
                ~undo_allowed':true
                ~winner_msg':None
          | Error _ ->
            Ui_effect.Ignore
      in

      (* --- PULL FROM FIRESTACK / FIREBASE --- *)
let on_sync_from_cloud _ev =
  let%bind.Effect remote_opt = pull_state_effect () in
  match remote_opt with
  | None ->
      Effect.return ()
  | Some remote_st ->
      set_all
        ( screen
        , vs_comp
        , remote_st
        , selected_idxs
        , hist
        , show_popup
        , last_draw_multi
        , last_moves
        , undo_allowed
        , winner_msg
        , lobby_id_opt
        , my_player_index
        )
in

      let on_sort_hand _ev =
        if not (human_turn ()) then Ui_effect.Ignore
        else
          let new_players = Array.copy st.players in
          let p_curr = new_players.(st.current) in
          let sorted_hand = sort_hand_for_display p_curr.hand in
          new_players.(st.current) <- { p_curr with hand = sorted_hand };
          let st' = { st with players = new_players } in
          apply_and_maybe_ai
            ~last_multi:last_draw_multi
            ~undo_allowed_opt:(Some true)
            st'
      in

      let on_layoff ~meld_idx =
        if Poly.(st.phase <> T.Play) || not (human_turn ()) then
          Ui_effect.Ignore
        else
          match selected_cards_from_hand () with
          | Some [ card ] ->
            (match E.play ~action:(T.Lay_off (card, meld_idx)) st with
             | Ok st' | End_round st' ->
               let pts = points_of_cards [ card ] in
               let scores' = Array.copy st'.scores in
               scores'.(st.current) <- scores'.(st.current) + pts;
               let st'' = { st' with scores = scores' } in
               apply_and_maybe_ai
                 ~last_multi:last_draw_multi
                 ~undo_allowed_opt:(Some true)
                 st''
             | Error _ -> Ui_effect.Ignore)
          | _ -> Ui_effect.Ignore
      in

      let on_meld_selected _ev =
        if Poly.(st.phase <> T.Play) || not (human_turn ()) then
          Ui_effect.Ignore
        else
          match selected_cards_from_hand () with
          | None -> Ui_effect.Ignore
          | Some cs ->
            let try_set =
              match E.play ~action:(T.Make_set cs) st with
              | Ok st' | End_round st' -> Some st'
              | Error _ -> None
            in
            let try_run =
              match E.play ~action:(T.Make_run cs) st with
              | Ok st' | End_round st' -> Some st'
              | Error _ -> None
            in
            let st_opt =
              match try_set, try_run with
              | Some st', _ -> Some st'
              | None, Some st' -> Some st'
              | None, None -> None
            in
            (match st_opt with
             | Some st' ->
               let pts = points_of_cards cs in
               let scores' = Array.copy st'.scores in
               scores'.(st.current) <- scores'.(st.current) + pts;
               let st'' = { st' with scores = scores' } in
               apply_and_maybe_ai
                 ~last_multi:last_draw_multi
                 ~undo_allowed_opt:(Some true)
                 st''
             | None -> Ui_effect.Ignore)
      in

      let on_discard_from_play _ev =
        if Poly.(st.phase <> T.Play) || not (human_turn ()) then
          Ui_effect.Ignore
        else
          match st.required_to_use, last_draw_multi with
          | Some _, true ->
            set_popup true
          | _ ->
            (match selected_cards_from_hand () with
             | Some [ card ] ->
               (match E.play ~action:T.Skip_to_discard st with
                | Ok st_d | End_round st_d ->
                  (match E.discard ~action:(T.Discard_card card) st_d with
                   | Ok st_after_disc | End_round st_after_disc ->
                     let prev = last_moves.(st.current) in
                     let lm =
                       update_last_move_for_player st.current
                         (if String.equal prev "—"
                          then Printf.sprintf "Discarded: %s"
                                 (string_of_card card)
                          else
                            prev ^ " | " ^
                            Printf.sprintf "Discarded: %s"
                              (string_of_card card))
                     in
                     let hand_after = st_after_disc.players.(st_after_disc.current).hand in
                     if List.is_empty hand_after then
                       end_round_with_winner
                         ~winner_idx:st_after_disc.current
                         st_after_disc
                     else if not vs_comp then
                       apply_and_maybe_ai
                         ~last_multi:false
                         ~last_moves_opt:(Some lm)
                         ~undo_allowed_opt:(Some true)
                         { st_after_disc with phase = T.EndCheck }
                     else
                       (match E.endcheck st_after_disc with
                        | Ok st_ec | End_round st_ec ->
                          apply_and_maybe_ai
                            ~last_multi:false
                            ~last_moves_opt:(Some lm)
                            ~undo_allowed_opt:(Some true)
                            st_ec
                        | Error _ ->
                          apply_and_maybe_ai
                            ~last_multi:false
                            ~last_moves_opt:(Some lm)
                            ~undo_allowed_opt:(Some true)
                            st_after_disc)
                   | Error _ -> Ui_effect.Ignore)
                | Error _ -> Ui_effect.Ignore)
             | _ -> Ui_effect.Ignore)
      in

      let on_discard_phase_finish _ev =
        if not vs_comp then
          apply_and_maybe_ai
            ~last_multi:false
            ~undo_allowed_opt:(Some true)
            { st with phase = T.EndCheck }
        else
          match E.endcheck st with
          | Ok st' | End_round st' ->
            apply_and_maybe_ai
              ~last_multi:false
              ~undo_allowed_opt:(Some true)
              st'
          | Error _ -> Ui_effect.Ignore
      in

      let on_end_check _ev =
        match E.endcheck st with
        | Ok st' | End_round st' ->
          apply_and_maybe_ai
            ~last_multi:false
            ~undo_allowed_opt:(Some true)
            st'
        | Error _ -> Ui_effect.Ignore
      in

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

        (* Local start: pass & play / vs computer *)
        let start_game vs_flag _ev =
          online_mode := false;
          my_index := 0;
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
                ~last_draw_multi':false
                ~last_moves':[| "—"; "—" |]
                ~undo_allowed':true
                ~winner_msg':None
            ]
        in

        (* Host online: Player 1.
           Very rough: just start a fresh 2-human game and push it. *)
        let host_online_attrs =
          Vdom.Attr.on_click (fun _ev ->
            online_mode := true;
            my_index := 0;
            let st0 = initial_state ~vs_computer:false () in
            Ui_effect.Many
              [ set_vs false
              ; set_all_full
                  ~screen':`Playing
                  ~vs_comp':false
                  ~st':st0
                  ~selected':[]
                  ~hist':[]
                  ~popup':false
                  ~last_draw_multi':false
                  ~last_moves':[| "—"; "—" |]
                  ~undo_allowed':true
                  ~winner_msg':None
              ]
          )
        in

        (* Join online: Player 2.
           Rough version: just go to Playing; other device will host,
           and you press “Sync from Cloud” once they’ve drawn. *)
        let join_online_attrs =
          Vdom.Attr.on_click (fun _ev ->
            online_mode := true;
            my_index := 1;
            set_all_full
              ~screen':`Playing
              ~vs_comp':false
              ~st':st
              ~selected':selected_idxs
              ~hist':hist
              ~popup':false
              ~last_draw_multi':last_draw_multi
              ~last_moves':last_moves
              ~undo_allowed':undo_allowed
              ~winner_msg':winner_msg
          )
        in

        (* Tutorial screen *)
        let go_tutorial _ev =
          set_all_full
            ~screen':`Tutorial
            ~vs_comp':vs_comp
            ~st':st
            ~selected':selected_idxs
            ~hist':hist
            ~popup':false
            ~last_draw_multi':last_draw_multi
            ~last_moves':last_moves
            ~undo_allowed':undo_allowed
            ~winner_msg':winner_msg
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
          ; Vdom.Node.div
              [ Vdom.Node.button
                  ~attrs:[ host_online_attrs; style btn_css ]
                  [ Vdom.Node.text "Host online game" ]
              ; Vdom.Node.button
                  ~attrs:[ join_online_attrs; style btn_css ]
                  [ Vdom.Node.text "Join online game" ]
              ]
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
          white-space:pre-wrap;
          background:#111;
          border:1px solid #333;
          border-radius:8px;
          padding:14px;
          max-width:900px;
          width:100%;
          line-height:1.35;
          word-break:break-word;
          overflow-wrap:anywhere;
          max-height:80vh;
          overflow-y:auto;
        |} in
        let back _ev =
          set_all_full
            ~screen':`Intro
            ~vs_comp':vs_comp
            ~st':st
            ~selected':selected_idxs
            ~hist':hist
            ~popup':false
            ~last_draw_multi':last_draw_multi
            ~last_moves':last_moves
            ~undo_allowed':undo_allowed
            ~winner_msg':winner_msg
        in
        Vdom.Node.div
          ~attrs:[ style page_css ]
          [ Vdom.Node.h2 [ Vdom.Node.text "How to Play" ]
          ; Vdom.Node.div ~attrs:[ style text_css ]
              [ Vdom.Node.text tutorial_text ]
          ; Vdom.Node.button
              ~attrs:[ style btn_css; Vdom.Attr.on_click back ]
              [ Vdom.Node.text "Back" ]
          ]

      | `Playing ->
        let screen_css = {|
          display:flex;flex-direction:column;min-height:100vh;
          background-color:#000;color:white;font-family:sans-serif;
        |} in
        (* new responsive middle layout *)
        let middle_wrap_css = {|
          flex-grow:1;
          display:flex;
          flex-direction:row;
          align-items:flex-start;
          justify-content:center;
          background-color:#0f0f0f;
          color:white;
          padding:12px;
          gap:12px;
          overflow-x:auto;
        |} in
        let table_css = {|
          display:flex;
          flex-direction:row;
          align-items:flex-start;
          justify-content:center;
          gap:14px;
          padding:14px;
          background-color:#1a1a1a;
          border:2px solid #333;
          border-radius:8px;
          box-shadow:0px 4px 10px rgba(0,0,0,0.6);
          max-width:420px;
          width:100%;
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

        let phase_controls =
          match st.phase with
          | T.Draw ->
            Vdom.Node.div
              [ Vdom.Node.div
                  ~attrs:[ style "color:white;font-size:14px;margin-top:8px;" ]
                  [ Vdom.Node.text "Draw: Deck (top) or Discard (top / multi)." ]
              ]
          | T.Play ->
            let human = human_turn () in


            
            if human then
              let hand_is_empty = List.is_empty st.players.(st.current).hand in
              let endturn_btn =
                if hand_is_empty then
                  Vdom.Node.button
                    ~attrs:[
                      Vdom.Attr.on_click (fun _ ->
                        let _ =
                          update_last_move_for_player st.current
                            "Rummy without a discard"
                        in
                        end_round_with_winner ~winner_idx:st.current st
                      );
                      style btn_small_css
                    ]
                    [ Vdom.Node.text "END TURN / RUMMY WITHOUT A DISCARD" ]
                else
                  Vdom.Node.none
              in
              Vdom.Node.div
                ~attrs:[ style controls_row_css ]
                [ Vdom.Node.button
                    ~attrs:[ Vdom.Attr.on_click on_sort_hand; style btn_small_css ]
                    [ Vdom.Node.text "SORT HAND" ]
                ; Vdom.Node.button
                    ~attrs:[ Vdom.Attr.on_click on_meld_selected; style btn_small_css ]
                    [ Vdom.Node.text "MELD THESE CARDS" ]
                ; Vdom.Node.button
                    ~attrs:[ Vdom.Attr.on_click on_discard_from_play; style btn_small_css ]
                    [ Vdom.Node.text "DISCARD" ]
                ; endturn_btn
                ]
            else
              Vdom.Node.div
                ~attrs:[ style "color:white;margin-top:8px;" ]
                [ Vdom.Node.text "Computer is playing..." ]
          | T.Discard ->
            if human_turn () then
              Vdom.Node.div
                ~attrs:[ style endcheck_wrap_css ]
                [ Vdom.Node.p [ Vdom.Node.text "Finish your turn." ]
                ; Vdom.Node.button
                    ~attrs:[ Vdom.Attr.on_click on_discard_phase_finish; style btn_small_css ]
                    [ Vdom.Node.text "CONTINUE" ]
                ]
            else
              Vdom.Node.div [ Vdom.Node.text "Computer discarding..." ]
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

        let active_hand = st.players.(st.current).hand in
        let active_name = st.players.(st.current).name in

        let opponent_idx = 1 - st.current in
        let opponent = st.players.(opponent_idx) in
        let opp_last_move = last_moves.(opponent_idx) in
        let picked_up_display = picked_up_from_last_move opp_last_move in
        let discarded_display = discarded_from_last_move opp_last_move in

        let top_opponent_bar =
          let outer_css = {|
            background-color:#111;
            color:white;
            padding:10px;
            font-family:sans-serif;
            border-bottom:2px solid #333;
            display:flex;
            flex-direction:column;
            gap:4px;
          |} in
          Vdom.Node.div
            ~attrs:[ style outer_css ]
            [ Vdom.Node.h3
                ~attrs:[ style "margin:0;font-size:14px;font-weight:700;" ]
                [ Vdom.Node.text
                  (Printf.sprintf "%s (%d cards)" opponent.name (List.length opponent.hand))
                ]
            ; (if vs_comp then
                 Vdom.Node.p
                   ~attrs:[ style "margin:0;font-size:12px;color:#ddd;" ]
                   [ Vdom.Node.text "_____________________" ]
               else
                 Vdom.Node.none)
            ; Vdom.Node.p
                ~attrs:[ style "margin:0;font-size:12px;color:#ddd;" ]
                [ Vdom.Node.text ("Picked Up: " ^ picked_up_display) ]
            ; Vdom.Node.p
                ~attrs:[ style "margin:0;font-size:12px;color:#ddd;" ]
                [ Vdom.Node.text ("Discarded: " ^ discarded_display) ]
            ]
        in

        let top_area =
          let sync_btn =
            Vdom.Node.button
              ~attrs:[
                Vdom.Attr.on_click on_sync_from_cloud;
                style btn_small_css
              ]
              [ Vdom.Node.text "Sync from Cloud" ]
          in
          Vdom.Node.div
            [ view_status_top ~st ~vs_comp
            ; top_opponent_bar
            ; Vdom.Node.div
                ~attrs:[ style "padding:8px 10px;text-align:center;" ]
                [ sync_btn ]
            ]
        in

        let middle_area =
          Vdom.Node.div
            ~attrs:[
              style middle_wrap_css;
              Vdom.Attr.class_ "mobile-column mobile-hide-scroll"
            ]
            [
              Vdom.Node.div
                ~attrs:[
                  style "display:flex;flex-direction:row;justify-content:center;align-items:flex-start;gap:14px;width:100%;";
                  Vdom.Attr.class_ "mobile-center"
                ]
                [
                  Vdom.Node.div
                    ~attrs:[ style table_css; Vdom.Attr.class_ "mobile-full" ]
                    [
                      view_facedown_card
                        ~count:(List.length st.deck)
                        ~on_click:on_click_deck;
                      view_discard_pile
                        ~discard:st.discard
                        ~on_click_stack:on_click_discard_stack
                        ~on_click_bar:on_click_discard_bar;
                    ]
                ];

              view_melds_right_panel
                ~melds:st.melds
                ~on_layoff:(fun idx -> on_layoff ~meld_idx:idx);
            ]
        in

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
                    if human_turn () && not (hide_hand_now ()) then
                      toggle_card_index i
                    else
                      Ui_effect.Ignore)
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
          | _ :: _ when not (Poly.(st.phase = T.Draw)) && undo_allowed ->
            Vdom.Node.div
              ~attrs:[ style "padding:10px;text-align:center;" ]
              [ Vdom.Node.button
                  ~attrs:[ Vdom.Attr.on_click on_back; style btn_small_css ]
                  [ Vdom.Node.text "⬅ UNDO" ]
              ]
          | _ -> Vdom.Node.none
        in

        let modal =
          if show_popup then
            let msg =
              "You took multiple cards from the discard pile. \
               You must use the BOTTOM-MOST picked card in a meld \
               this turn before you can discard."
            in
            view_modal ~message:msg ~on_close:(fun _ -> set_popup false)
          else
            Vdom.Node.none
        in

        let winner_modal =
          match winner_msg with
          | None -> Vdom.Node.none
          | Some msg ->
            view_modal
              ~message:msg
              ~on_close:(fun _ -> Ui_effect.Ignore)
        in

        Vdom.Node.div
          ~attrs:[ style screen_css ]
          [ top_area
          ; middle_area
          ; bottom_area
          ; undo_button
          ; modal
          ; winner_modal
          ]
    )

let () =
  Bonsai_web.Start.start
    ~bind_to_element_with_id:"app"
    component