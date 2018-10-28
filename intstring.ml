type t = int

let wrap : string -> t = function
	| s -> 0

let unwrap : t -> string = function
	| 0 -> ""
	| i -> string_of_int i
