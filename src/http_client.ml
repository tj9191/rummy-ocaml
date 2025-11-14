
open Async_kernel

let get (url : string) : string Deferred.t =
  (* This prints to browser console when compiled to JS *)
  Stdio.printf "HTTP GET (stub): %s\n%!" url;
  (* Return a minimal JSON that parse_state_from_firestore can safely handle *)
  let dummy_json = {|{"fields":{"state":{"stringValue":""}}}|} in
  Deferred.return dummy_json

let patch (url : string) (body : string) : unit Deferred.t =
  Stdio.printf "HTTP PATCH (stub): %s\nBODY: %s\n%!" url body;
  Deferred.unit