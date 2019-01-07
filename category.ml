type t = A | B | C | D | E | U | Flagged of string

let wrap = function
	| "A" -> A
	| "B" -> B
	| "C" -> C
	| "D" -> D
	| "E" -> E
	| "U" -> U
	| other -> Flagged other

let unwrap = function
	| A -> "A"
	| B -> "B"
	| C -> "C"
	| D -> "D"
	| E -> "E"
	| U -> "U"
	| Flagged other -> other

let int_of_category = function
	| A -> 0
	| B -> 1
	| C -> 2
	| D -> 3
	| E -> 4
	| U -> 5
	| Flagged _ -> 6

let categories = [| A; B; C; D; E; U |]
