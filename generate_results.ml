
type event_id = string;;
type racer_id = string;;

module Cat = struct
    type t = A | B | C | D | E

    let of_string = function
    | "A" -> A
    | "B" -> B
    | "C" -> C
    | "D" -> D
    | "E" -> E
    | otherwise -> failwith "Invalid category"

    let to_string = function
    | A -> "A"
    | B -> "B"
    | C -> "C"
    | D -> "D"
    | E -> "E"
end;;

type event_key = {
    event_id : event_id;
    category : Cat.t;
};;

type racer = {
    rid : int;
    rname : string;
    mutable rpoints : int list;
    rcategory : Cat.t;
}

type event_racer = {
    id : int;
    name : string;
    position : int;
    mutable points : int;
}

type event = {
    mutable racers : (racer_id, event_racer) Hashtbl.t;
    mutable total_participants : int;
}

let events : (event_key, event) Hashtbl.t = Hashtbl.create 40;;
let racers : (racer_id, racer) Hashtbl.t = Hashtbl.create 40;;

let results_ic = Csv.of_channel (open_in "temp/results.csv");;

let position_to_points position = function
    | n when n <= 5 -> List.nth [10; 7; 5; 3; 1] (position - 1)
    | n when n <= 10 -> List.nth [18; 15; 13; 11; 9; 7; 5; 3; 2; 1] (position - 1)
    | n when n <= 15 -> List.nth [26; 23; 21; 19; 17; 15; 13; 11; 9; 7; 5; 4; 3; 2; 1] (position - 1)
    | n when n <= 20 -> List.nth [34; 31; 29; 27; 25; 23; 21; 19; 17; 15; 13; 11; 9; 7; 6; 5; 4; 3; 2; 1] (position - 1)
    | n when n <= 25 -> List.nth [42; 39; 37; 35; 33; 31; 29; 27; 25; 23; 21; 19; 17; 15; 13; 11; 9; 8; 7; 6; 5; 4; 3; 2; 1] (position - 1)
    | n when n <= 30 -> List.nth [50; 47; 45; 43; 41; 39; 37; 35; 33; 31; 29; 27; 25; 23; 21; 19; 17; 15; 13; 11; 10; 9; 8; 7; 6; 5; 4; 3; 2; 1] (position - 1)
    | n when n <= 35 -> List.nth [58; 55; 53; 51; 49; 47; 45; 43; 41; 39; 37; 35; 33; 31; 29; 27; 25; 23; 21; 19; 17; 15; 13; 12; 11; 10; 9; 8; 7; 6; 5; 4; 3; 2; 1] (position - 1)
    | n when n <= 40 -> List.nth [66; 63; 61; 59; 57; 55; 53; 51; 49; 47; 45; 43; 41; 39; 37; 35; 33; 31; 29; 27; 25; 23; 21; 19; 17; 15; 14; 13; 12; 11; 10; 9; 8; 7; 6; 5; 4; 3; 2; 1] (position - 1)
    | n when n <= 45 -> List.nth [74; 71; 69; 67; 65; 63; 61; 59; 57; 55; 53; 51; 49; 47; 45; 43; 41; 39; 37; 35; 33; 31; 29; 27; 25; 23; 21; 19; 17; 16; 15; 14; 13; 12; 11; 10; 9; 8; 7; 6; 5; 4; 3; 2; 1] (position - 1)
    | n when position <= 50 -> List.nth [82; 79; 77; 75; 73; 71; 69; 67; 65; 63; 61; 59; 57; 55; 53; 51; 49; 47; 45; 43; 41; 39; 37; 35; 33; 31; 29; 27; 25; 23; 21; 19; 18; 17; 16; 15; 14; 13; 12; 11; 10; 9; 8; 7; 6; 5; 4; 3; 2; 1] (position - 1)
    | _ -> 0

let top_6_positions positions =
    let rec take = function
    | _, 0 -> []
    | [], _ -> []
    | x::xs, n -> x :: take (xs, n-1)
    in take (List.sort (fun left right -> -(compare left right)) positions, 6)

let () =
    (* columns needed: zid, position_in_cat, category, name
                       5,   7,               27,       8 *)
    ignore (Csv.next results_ic); (* skip header *)

    Csv.iter (fun row ->
            let event_key = {
                event_id = List.nth row 5;
                category = Cat.of_string (List.nth row 27);
            } in
            match Hashtbl.find_opt events event_key with
            | None ->
                let new_event = {
                    racers = Hashtbl.create 7;
                    total_participants = 1;
                } in
                Hashtbl.add new_event.racers (List.nth row 8) {
                        id = int_of_string (List.nth row 11);
                        name = List.nth row 8;
                        position = int_of_string (List.nth row 7);
                        points = 0;
                    };
                Hashtbl.add events event_key new_event;
            | Some event ->
                event.total_participants <- event.total_participants + 1;
                Hashtbl.add event.racers (List.nth row 8) {
                        id = int_of_string (List.nth row 11);
                        name = List.nth row 8;
                        position = int_of_string (List.nth row 7);
                        points = 0;
                    }
        ) results_ic;
    
    (* generate points *)
    Hashtbl.iter (fun event_key event ->
        Hashtbl.iter (fun racer_key event_racer ->
            event_racer.points <- position_to_points event_racer.position event.total_participants)
            event.racers)
        events;
    
    (* total for the league *)
    Hashtbl.iter (fun event_key event ->
        Hashtbl.iter (fun racer_key event_racer ->
                match Hashtbl.find_opt racers racer_key with
                | None ->
                    Hashtbl.add racers event_racer.name {
                            rid = event_racer.id;
                            rname = event_racer.name;
                            rpoints = [event_racer.points];
                            rcategory = event_key.category;
                    }
                | Some racer ->
                    racer.rpoints <- event_racer.points :: racer.rpoints)
            event.racers)
        events;
    
    (* print results *)
    Hashtbl.iter (fun racer_id racer ->
        Printf.printf "%s [%s]: %d points\n" racer.rname (Cat.to_string racer.rcategory) (List.fold_left (+) 0 (top_6_positions racer.rpoints));
        Printf.printf "  Completed %d races, points: " (List.length racer.rpoints);
        List.iter (fun p -> Printf.printf " %d" p) racer.rpoints;
        Printf.printf "\n  Best results: ";
        List.iter (fun p -> Printf.printf " %d" p) (top_6_positions racer.rpoints);
        Printf.printf "\n";
    )
        racers;
;;

module Zwifter = Set.Make(struct
    type t = racer

    let compare left right = compare left.rid right.rid
end);;

let to_set table =
    Hashtbl.fold (fun k v s -> Zwifter.add v s) table Zwifter.empty;;

let cat_racers =
    let cats = [| Cat.A; Cat.B; Cat.C; Cat.D; Cat.E|] in
    let all = to_set racers in
    let sets = Array.make 5 all in
    Array.mapi (fun ix set ->
        Zwifter.filter (fun zwifter ->
            zwifter.rcategory = cats.(ix)) set) sets;;
