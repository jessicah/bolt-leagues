type t = A | B | C | D | E

let wrap = function
	| "A" -> A
	| "B" -> B
	| "C" -> C
	| "D" -> D
	| "E" -> E
	| _ -> failwith "invalid category"

let unwrap = function
	| A -> "A"
	| B -> "B"
	| C -> "C"
	| D -> "D"
	| E -> "E"

let int_of_category = function
	| A -> 0
	| B -> 1
	| C -> 2
	| D -> 3
	| E -> 4
