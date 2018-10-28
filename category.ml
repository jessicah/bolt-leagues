type t = CatA | CatB | CatC | CatD | CatE

let wrap = function
	| "A" -> CatA
	| "B" -> CatB
	| "C" -> CatC
	| "D" -> CatD
	| "E" -> CatE
	| _ -> failwith "invalid category"

let unwrap = function
	| CatA -> "A"
	| CatB -> "B"
	| CatC -> "C"
	| CatD -> "D"
	| CatE -> "E"
