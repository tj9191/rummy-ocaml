(* src/http_client.ml *)

open Core
open Async_kernel

(* TEMP placeholder so it compiles; you’ll fill real HTTP later *)
let get (_url : string) : string Deferred.t =
  failwith "Http_client.get not implemented yet"

let patch (_url : string) (_body : string) : unit Deferred.t =
  failwith "Http_client.patch not implemented yet"