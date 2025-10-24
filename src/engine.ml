open Types

(* ---------- Errors & step result ---------- *)

type error =
  | Illegal_phase of phase
  | Empty_deck
  | Empty_discard
  | Both_sources_empty
  | Illegal_play of string
  | Illegal_discard of string

type step_result =
  | Ok of state
  | End_round of state
  | Error of error

(* ---------- Core utils ---------- *)

let top_and_rest = function
  | [] -> None
  | x :: xs -> Some (x, xs)

let with_current (st : state) : player =
  st.players.(st.current)

let replace_current (st : state) (p : player) : state =
  st.players.(st.current) <- p; st

let next_player_index (st : state) =
  (st.current + 1) mod Array.length st.players

(* take the first n elements and the rest *)
let take_n n xs =
  let rec go k acc rest =
    if k = 0 then (List.rev acc, rest)
    else match rest with
      | [] -> (List.rev acc, [])
      | y :: ys -> go (k - 1) (y :: acc) ys
  in
  if n <= 0 then ([], xs) else go n [] xs

let rec last_exn = function
  | [] -> failwith "last_exn: empty list"
  | [x] -> x
  | _ :: xs -> last_exn xs

(* ---------- Hand helpers (multiset-aware) ---------- *)

let remove_one (hand : card list) (c : card) : (card list, string) result =
  let rec go acc = function
    | [] -> Result.Error "card not in hand"
    | x :: xs ->
        if x = c then Result.Ok (List.rev_append acc xs)
        else go (x :: acc) xs
  in
  go [] hand

let remove_many (hand : card list) (to_take : card list) : (card list, string) result =
  List.fold_left
    (fun acc c ->
       match acc with
       | Result.Error e -> Result.Error e
       | Result.Ok h -> remove_one h c)
    (Result.Ok hand) to_take

let replace_nth (xs : 'a list) (idx : int) (x : 'a) : ('a list, string) result =
  let rec go i acc = function
    | [] -> Result.Error "meld index out of range"
    | _ :: xs when i = 0 -> Result.Ok (List.rev acc @ (x :: xs))
    | y :: xs -> go (i - 1) (y :: acc) xs
  in
  if idx < 0 then Result.Error "meld index out of range" else go idx [] xs

(* When a meld/run/layoff uses cards, clear the "required_to_use" if satisfied *)
let clear_required_if_used (st : state) (used : card list) : state =
  match st.required_to_use with
  | None -> st
  | Some r ->
      if List.exists ((=) r) used
      then { st with required_to_use = None }
      else st

(* ---------- Play actions ---------- *)

let apply_make_set (cards : card list) (st : state) : (state, error) result =
  if List.length cards < 3 then Error (Illegal_play "set must have ≥3 cards") else
  if not (Rules.is_set cards) then Error (Illegal_play "not a valid set") else
  let p = with_current st in
  match remove_many p.hand cards with
  | Result.Error _ -> Error (Illegal_play "cards not all in hand")
  | Result.Ok new_hand ->
      let meld = { kind = Set; cards } in
      let p' = { p with hand = new_hand } in
      let st' = replace_current { st with melds = meld :: st.melds } p' in
      let st'' = clear_required_if_used st' cards in
      Ok st''

let apply_make_run (cards : card list) (st : state) : (state, error) result =
  if List.length cards < 3 then Error (Illegal_play "run must have ≥3 cards") else
  if not (Rules.is_run ~ace_policy:st.ace_policy cards) then
    Error (Illegal_play "not a valid run")
  else
    let p = with_current st in
    match remove_many p.hand cards with
    | Result.Error _ -> Error (Illegal_play "cards not all in hand")
    | Result.Ok new_hand ->
        let meld = { kind = Run; cards } in
        let p' = { p with hand = new_hand } in
        let st' = replace_current { st with melds = meld :: st.melds } p' in
        let st'' = clear_required_if_used st' cards in
        Ok st''

let apply_layoff (card, meld_idx) (st : state) : (state, error) result =
  let p = with_current st in
  (* 1) card must be in hand *)
  match remove_one p.hand card with
  | Result.Error _ -> Error (Illegal_play "layoff card not in hand")
  | Result.Ok hand_after_pop ->
      (* 2) fetch target meld *)
      (match List.nth_opt st.melds meld_idx with
       | None -> Error (Illegal_play "invalid meld index")
       | Some m ->
           (* 3) adding card must keep meld valid *)
           if not (Rules.can_layoff ~ace_policy:st.ace_policy card m)
           then Error (Illegal_play "cannot lay off on that meld")
           else
             let updated_meld = { m with cards = card :: m.cards } in
             (* 4) replace meld in list *)
             (match replace_nth st.melds meld_idx updated_meld with
              | Result.Error _ -> Error (Illegal_play "invalid meld index")
              | Result.Ok melds' ->
                  let p' = { p with hand = hand_after_pop } in
                  let st' = replace_current { st with melds = melds' } p' in
                  let st'' = clear_required_if_used st' [card] in
                  Ok st''))

(* ---------- Draw transition ---------- *)

let draw ~(source : draw_source) (st : state) : step_result =
  match st.phase with
  | Draw -> (
      match source with
      | FromDeck -> (
          match top_and_rest st.deck with
          | None ->
              (match st.discard with
               | [] -> End_round st
               | _  -> Error Empty_deck)
          | Some (card, rest) ->
              let p  = with_current st in
              let p' = { p with hand = card :: p.hand } in
              let st' = replace_current { st with deck = rest; phase = Play } p' in
              Ok st'
        )
      | FromDiscard -> (
          match top_and_rest st.discard with
          | None -> Error Empty_discard
          | Some (card, rest) ->
              let p  = with_current st in
              let p' = { p with hand = card :: p.hand } in
              let st' =
                replace_current
                  { st with discard = rest; phase = Play }
                  p'
              in
              Ok st'
        )
      | FromDiscardN n -> (
          if n <= 0 then Error (Illegal_play "N must be ≥ 1") else
          let (taken, rest) = take_n n st.discard in
          match taken with
          | [] -> Error Empty_discard
          | _ ->
              let bottom = last_exn taken in
              let p = with_current st in
              (* put taken on top of hand; order doesn't matter for rules *)
              let p' = { p with hand = List.rev_append taken p.hand } in
              let st' =
                replace_current
                  { st with
                      discard = rest;
                      phase = Play;
                      required_to_use = Some bottom;
                  }
                  p'
              in
              Ok st'
        )
    )
  | ph -> Error (Illegal_phase ph)

(* ---------- Play transition ---------- *)

let play ~(action : play_action) (st : state) : step_result =
  match st.phase with
  | Play ->
      let res =
        match action with
        | Make_set cs -> apply_make_set cs st
        | Make_run cs -> apply_make_run cs st
        | Lay_off (card, idx) -> apply_layoff (card, idx) st
        | Skip_to_discard ->
            (match st.required_to_use with
             | Some _ ->
                 Error (Illegal_play "You must use the bottom picked discard card before discarding.")
             | None ->
                 Ok { st with phase = Discard })
      in
      (match res with
       | Ok st' -> Ok st'           (* remain in Play after meld/layoff/ok skip *)
       | Error e -> Error e)
  | ph -> Error (Illegal_phase ph)

(* ---------- Discard transition ---------- *)

let discard ~(action : discard_action) (st : state) : step_result =
  match st.phase with
  | Discard ->
      (match st.required_to_use with
       | Some _ ->
           Error (Illegal_discard "You must use the required card before discarding.")
       | None ->
           let (Discard_card c) = action in
           let p = with_current st in
           (match remove_one p.hand c with
            | Result.Error _ -> Error (Illegal_discard "card not in hand")
            | Result.Ok new_hand ->
                let p' = { p with hand = new_hand } in
                let st' =
                  replace_current
                    { st with discard = c :: st.discard; phase = EndCheck }
                    p'
                in
                Ok st'))
  | ph -> Error (Illegal_phase ph)

(* ---------- EndCheck transition ---------- *)

let endcheck (st : state) : step_result =
  match st.phase with
  | EndCheck ->
      let p = with_current st in
      if p.hand = [] then
        End_round st   (* ✅ player went out *)
      else
        let st' = { st with current = next_player_index st; phase = Draw } in
        Ok st'
  | ph -> Error (Illegal_phase ph)