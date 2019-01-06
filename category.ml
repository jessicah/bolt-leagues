type t = A | B | C | D | E | U

let wrap = function
	| "A" -> A
	| "B" -> B
	| "C" -> C
	| "D" -> D
	| "E" -> E
	| "U" -> U
	| _ -> failwith "invalid category"

let unwrap = function
	| A -> "A"
	| B -> "B"
	| C -> "C"
	| D -> "D"
	| E -> "E"
	| U -> "U"

let int_of_category = function
	| A -> 0
	| B -> 1
	| C -> 2
	| D -> 3
	| E -> 4
	| U -> 5

let categories = [| A | B | C | D | E | U |]
