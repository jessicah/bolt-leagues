
(* BRT Racing Leagues *)

let wild =
    [| 129382, 129383; 132853, 132859; 135191, 136832; 139644, 139681; 143745, 143781; 148232, 148267; 152682, 152719; 156636, 156671 |]

let wild_first, wild_second = wild.(Array.length wild - 1)

let wild_previous =
    List.take ((Array.length wild - 1) * 2)
        (Array.map (fun (f,s) -> [string_of_int f; string_of_int s]) wild |> Array.to_list |> List.concat)

let get_round n =
    let event_pairs = List.take n (Array.to_list wild) in
    let first, second = wild.(n-1) in
    let previous = List.take ((n-1) * 2)
        (Array.map (fun (f,s) -> [string_of_int f; string_of_int s]) wild |> Array.to_list |> List.concat) in
    first, second, previous
;;

let all_wild = Array.map (fun (f,s) -> [f; s]) wild |> Array.to_list |> List.concat |> Array.of_list

let ages =
    [| 126030; 130133; 134347; 138991; 143180; 147547; 152009; 156086 |]

let criterium =
    [| 125399; 129606; 133725; 138387; 142687; 146930; 151443; 155620 |]

let irongoat =
    [| 127227; 131352; 136011; 140320; 144388; 148984; 153414; 157188 |]

let timetrial =
    [| 124741; 128934; 133123; 137731; 142036; 146246; 150755; 155102 |]

(* fuzion == ages + criterium + irongoat + timetrial, but currently including training races as well? *)
