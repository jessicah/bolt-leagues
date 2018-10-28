module type S = sig
  val normalize : Yojson.Safe.json -> Yojson.Safe.json
  val restore : Yojson.Safe.json -> Yojson.Safe.json
end

module Type_field = struct
  module type Param = sig
    val type_field_name : string
  end

  module Make (Param : Param) : S = struct
    open Yojson.Safe

    open Param

    let normalize (x : json) : json =
      match x with
      | `Assoc fields ->
          (match List.assoc type_field_name fields with
           | `String type_ -> `List [ `String type_; x ]
           | exception Not_found -> x
           | _ -> x (* malformed *)
          )
      | `String type_ as x -> x
      | malformed -> malformed

    let restore (x : json) : json =
      match x with
      | `List [ `String type_; `Assoc fields ] ->
          let fields =
            (type_field_name, `String type_) ::
            List.filter (fun (k, v) -> k <> type_field_name) fields
          in
          `Assoc fields
      | `String type_ as x -> x
      | malformed -> malformed
  end

  module Default_param : Param = struct
    let type_field_name = "type"
  end

  include Make (Default_param)
end