#!/usr/bin/env ocaml

#use "topfind";;
#require "csv";;

type event_id = string;;
type racer_id = string;;

type category = string;;

type event_key = {
    event_id : event_id;
    category : category;
};;

type racer = {
    rname : string;
    mutable rpoints : int list;
    rcategory : category;
    mutable sprints : int;
}

type event_racer = {
    name : string;
    position : int;
    mutable points : int;
}

type event = {
    mutable racers : (racer_id, event_racer) Hashtbl.t;
    mutable total_participants : int;
}

let race_categories = ["A"; "B"; "C"; "D"; "E"];;

let events : (event_key, event) Hashtbl.t = Hashtbl.create 40;;
let racers : (racer_id, racer) Hashtbl.t = Hashtbl.create 40;;
let categories = Hashtbl.create 5;;

let results_ic = Csv.of_channel (open_in "temp/results.csv");;
let sprints_ic = Csv.of_channel (open_in "temp/sprints.csv");;

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
    | _ -> 1

let top_6_positions positions =
    let rec take = function
    | _, 0 -> []
    | [], _ -> []
    | x::xs, n -> x :: take (xs, n-1)
    in take (List.sort (fun left right -> -(compare left right)) positions, 6);;

let rider_points all_points = List.fold_left (+) 0 (top_6_positions all_points);;
let total_points r = rider_points r.rpoints + r.sprints;;

let get_field f pos list = match List.nth_opt list pos with Some x -> f x | None -> "";;
let get_name list pos = get_field (fun r -> r.rname) pos list;;
let get_races list pos = get_field (fun r -> string_of_int (List.length r.rpoints)) pos list;;
let get_sprints list pos = get_field (fun r -> string_of_int r.sprints) pos list;;
let get_total list pos = get_field (fun r -> string_of_int (total_points r)) pos list;;

let () =
    (* columns needed: zid, position_in_cat, category, name
                       5,   7,               27,       8 *)
    ignore (Csv.next results_ic); (* skip header *)

    Csv.iter (fun row ->
            let event_key = {
                event_id = List.nth row 5;
                category = List.nth row 27;
            } in
            match Hashtbl.find_opt events event_key with
            | None ->
                let new_event = {
                    racers = Hashtbl.create 7;
                    total_participants = 1;
                } in
                Hashtbl.add new_event.racers (List.nth row 8) {
                        name = List.nth row 8;
                        position = int_of_string (List.nth row 7);
                        points = 0;
                    };
                Hashtbl.add events event_key new_event;
            | Some event ->
                event.total_participants <- event.total_participants + 1;
                Hashtbl.add event.racers (List.nth row 8) {
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
                            rname = event_racer.name;
                            rpoints = [event_racer.points];
                            rcategory = event_key.category;
                            sprints = 0;
                    }
                | Some racer ->
                    racer.rpoints <- event_racer.points :: racer.rpoints)
            event.racers)
        events;
    
    Hashtbl.iter (fun racer_name racer_details ->
            (* hashtables actually store a list of elements, so we can blindly add *)
            Hashtbl.add categories racer_details.rcategory racer_details)
        racers;
    
    (* now process sprint points... *)
    (* columns needed: zid, position_in_cat, category, name
                       4,   6,               18,       7 *)
    ignore (Csv.next sprints_ic); (* skip header *)

    Csv.iter (fun row ->
            let racer = Hashtbl.find racers (List.nth row 7) in
            racer.sprints <- racer.sprints + match (List.nth row 6) with
                | "1" -> 3
                | "2" -> 2
                | "3" -> 1
                | _ -> 0
        ) sprints_ic;

    (* print results by category *)
    List.iter (fun category ->
            let sorted_racers = List.sort (fun left right -> compare (rider_points right.rpoints) (rider_points left.rpoints))
                (Hashtbl.find_all categories category) in
            Printf.printf "Results for category %s:\n" category;
            List.iteri (fun index racer ->
                    Printf.printf "%3d: %s: %d points (%d sprint points) from %d races; results: [%s]\n"
                        (index + 1) racer.rname (rider_points racer.rpoints + racer.sprints)
                        racer.sprints (List.length racer.rpoints)
                        (String.concat ", " (List.map string_of_int racer.rpoints)))
                    sorted_racers)
            race_categories;
    
    let standings_oc = Csv.to_channel (open_out "standings.csv") in
    let standings = List.map (fun category ->
            List.sort (fun left right -> compare (total_points right) (total_points left))
                    (Hashtbl.find_all categories category))
        race_categories in
    
    let num_rows = List.fold_left max 0 (List.map List.length standings) in
    let [catA; catB; catC; catD; catE] = standings in

    Csv.output_record standings_oc ["Pos";"Category A";"R";"S";"TP";"Pos";"Category B";"R";"S";"TP";"Pos";"Category C";"R";"S";"TP";"Pos";"Category D";"R";"S";"TP";"Pos";"Category E";"R";"S";"TP";];
    for index = 0 to num_rows - 1 do
        let position = string_of_int (index + 1) in
            let row = [
                position; get_name catA index; get_races catA index; get_sprints catA index; get_total catA index;
                position; get_name catB index; get_races catB index; get_sprints catB index; get_total catB index;
                position; get_name catC index; get_races catC index; get_sprints catC index; get_total catC index;
                position; get_name catD index; get_races catD index; get_sprints catD index; get_total catD index;
                position; get_name catE index; get_races catE index; get_sprints catE index; get_total catE index;]
            in
            Csv.output_record standings_oc row;
    done;
    Csv.close_out standings_oc;
;;
