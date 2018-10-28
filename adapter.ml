module type S = sig
  val normalize : Yojson.Safe.json -> Yojson.Safe.json
  val restore : Yojson.Safe.json -> Yojson.Safe.json
end

module Singleton : S = struct
    open Yojson.Safe

    let normalize (x : json) : json =
      match x with
	  | `List [value; `Int 0] -> value
	  | `List [value; `Float 0.0] -> value
	  | malformed -> malformed

    let restore (x : json) : json =
      match x with
	  | `Int _ as i -> `List [ i; `Int 0]
	  | `Float _ as f -> `List [ f; `Float 0.0]
	  | malformed -> malformed
end
