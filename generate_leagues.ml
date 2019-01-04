#!/usr/bin/env ocaml

#use "topfind";;
#require "atdgen";;
#require "curl";;
#load "category.cmo";;
#load "results_j.cmo";;
#load "unix.cma";;

open Results_t;;
module Cat = Category;;

module List = struct
    include List

    let rec take n list = match n, list with
    | 0, _ | _, [] -> []
    | _, x::xs -> x :: take (n-1) xs
end;;

let int_of_category = Category.int_of_category;;
let num_categories = 5;;

type edited_rider_t = {
    e_category: Cat.t;
    e_flag: string;
    e_power_type: int;
    e_name: string;
    e_zwid: int;
    e_uid: string;
    e_penalty: int;
    e_notes: string;
}

let post url data =
    let buf = Buffer.create 80 in
    let c = Curl.init () in
    Curl.set_verbose c true;
    Curl.set_writefunction c (fun s -> Buffer.add_string buf s; String.length s);
    Curl.set_url c url;
    Curl.set_cookiejar c "zwiftpower.cookies";
    Curl.set_cookiefile c "zwiftpower.cookies";
    Curl.set_httpheader c ["Content-Type: application/x-www-form-urlencoded"];
    Curl.set_postfields c data;
    Curl.set_postfieldsize c (String.length data);
    Curl.perform c;
    let rc = Curl.get_responsecode c in
    Curl.cleanup c;
    rc, (Buffer.contents buf);;

let edit_riders event_id riders =
    let format rider =
        Printf.sprintf "%s, %s, %d, %s, %d, %s, %d, %s"
            (Cat.unwrap rider.e_category) rider.e_flag rider.e_power_type rider.e_name
            rider.e_zwid rider.e_uid rider.e_penalty rider.e_notes;
    in
    let data = String.concat "\n" (List.map format riders) in
    post
        "https://www.zwiftpower.com/ucp.php?mode=login"
        (Printf.sprintf "username=jessica.l.hamilton@gmail.com&password=%s&autologin=1&redirect=./index.php?&login="
            (Unix.getenv "ZP_PASSWORD"));
    post
        (Printf.sprintf "https://www.zwiftpower.com/zz.php?do=edit_results&act=save&zwift_event_id=%d" event_id)
        ("edit_results=" ^ data);;

let read_file name =
	let ic = open_in name in
	let length = in_channel_length ic in
	really_input_string ic length

(* this is only data for a single race, not a league... *)
let race = Results_j.race_of_string (read_file "ages.results.json");;
let sprints = Results_j.sprints_of_string (read_file "ages.sprints.json");;

let categories = [|Cat.A; Cat.B; Cat.C; Cat.D; Cat.E|];;

let num_riders = Array.map List.length race.race_results;;

let segment_names =
    let module StringSet = Set.Make(String) in
    StringSet.elements (Array.fold_left (fun set list ->
        List.fold_left (fun set rider -> Hashtbl.fold (fun k v s -> StringSet.add k s) rider.s_msec set) set list)
        StringSet.empty sprints.sprint_results);;

type sprint_t = {
        sprint_rider : sprint_results;
        segment_name : string;
        segment_time : float;
    }

module SprintSet = Set.Make(struct
    type t = sprint_t

    let compare left right =
        let result = compare left.segment_time right.segment_time in
        if result = 0 then
            compare left.sprint_rider.s_zwid right.sprint_rider.s_zwid
        else
            result
end);;

let segment_results category =
    let results = Array.make (List.length segment_names) SprintSet.empty in
    List.iteri (fun index segment ->
        List.iter (fun rider ->
            let t = {
                sprint_rider = rider; segment_name = segment; segment_time = Hashtbl.find rider.s_msec segment;
            } in
            results.(index) <- SprintSet.add t results.(index)) sprints.sprint_results.(int_of_category category))
        segment_names;
    Array.map SprintSet.elements results;;

let print_sprint pos sprinter = Printf.printf "%d: %s - %f\n" pos sprinter.sprint_rider.s_name sprinter.segment_time;;

let top3 category segment = match (segment_results category).(segment) with
    | a :: b :: c :: _ -> print_sprint 1 a; print_sprint 2 b; print_sprint 3 c
    | a :: b :: _ -> print_sprint 1 a; print_sprint 2 b; Printf.printf " - end of list -\n"
    | a :: _ -> print_sprint 1 a; Printf.printf " - end of list -\n"
    | _ -> Printf.printf " - no riders -\n"
;;

let () =
    List.iteri (fun ix name -> Printf.printf "Segment %s:\n" name; top3 Cat.A ix) segment_names;;

module ZwiftSet = Set.Make(struct
    type t = Results_t.rider_result
    let compare left right =
        compare left.r_zwid right.r_zwid
end);;

let zwifters = Array.make 5 ZwiftSet.empty;;

let () =
    Array.iter (fun cats ->
        List.iter (fun zwifter ->
                zwifters.(int_of_category zwifter.r_category) <- ZwiftSet.add zwifter zwifters.(int_of_category zwifter.r_category))
            cats)
       race.race_results;;

let upgraded cat1 cat2 = ZwiftSet.inter zwifters.(int_of_category cat1) zwifters.(int_of_category cat2);;

let upgraded_list cat1 cat2 = ZwiftSet.elements (upgraded cat1 cat2);;

let above_limits lim cat =
    ZwiftSet.filter (fun zwifter -> zwifter.r_avg_wkg > lim) zwifters.(int_of_category cat);;

let above_limits_list lim cat = ZwiftSet.elements (above_limits lim cat);;

type points_data = {
    zwiftid : int;
    points_race_data : rider_result;
    points_sprints_data : sprint_results;
    mutable total_points : int;
    points_category : Category.t;
}

let cat_a_sprints = segment_results Cat.A;;
let sprint_1 = cat_a_sprints.(0);;
List.iteri (fun ix elt -> Printf.printf "%d: %s - %f\n" (succ ix) elt.sprint_rider.s_name (Hashtbl.find elt.sprint_rider.s_msec (List.hd segment_names))) sprint_1;;

type rider_data_t = {
    rider_id : int; (* zwid *)
    mutable completed_races : int;
    mutable race_points : int list;
    mutable sprint_points : int list;
    mutable race_category : Cat.t;
}

module IntsDesc = Set.Make(struct type t = int let compare left right = compare right left end);;

let points = List.map IntsDesc.of_list [
    [9; 7; 5; 3; 1];
    [17; 15; 13; 11; 2];
    [25; 23; 21; 19; 4];
    [33; 31; 29; 27; 6];
    [41; 39; 37; 35; 8];
    [49; 47; 45; 43; 10];
    [57; 55; 53; 51; 12];
    [65; 63; 61; 59; 14];
    [73; 71; 69; 67; 16];
    [81; 79; 77; 75; 18];
]

let position_to_points position participants =
    if position > participants then raise (Invalid_argument "position cannot exceed participants");
    let bonus = if position = 1 then 1 else 0 in
    let position = position - 1 in
    let points = List.fold_left IntsDesc.union IntsDesc.empty (List.take ((participants-1) / 5 + 1) points) in
    match List.nth_opt (IntsDesc.elements points) position with
    | None -> 1
    | Some i -> i + bonus;;

let best_results rider =
    let rec take = function
    | _, 0 -> []
    | [], _ -> []
    | x::xs, n -> x :: take (xs, n-1)
    in take (List.rev (List.sort compare rider.race_points), 6);;

let contains_substring pat str =
    let rec search i j =
        if j >= String.length pat then true
        else if i + j >= String.length str then false
        else if str.[i + j] = pat.[j] then search i (j+1)
        else search (i+1) 0
    in search 0 0;;

let filenames dirname =
    let handle = Unix.opendir dirname in
    let rec loop (results, sprints) = try
            let filename = Unix.readdir handle in
            match filename with
            | "." | ".." -> loop (results, sprints)
            | _ ->
                if contains_substring ".results.json" filename then
                    loop (filename::results, sprints)
                else if contains_substring ".sprints.json" filename then
                    loop (results, filename::sprints)
                else begin
                    print_endline ("warning: unsupported filename: " ^ filename);
                    loop (results, sprints)
                end
        with End_of_file ->
            Unix.closedir handle;
            (results, sprints)
    in loop ([], []);;

(* we have temp/ages, temp/crit, temp/irongoat, and temp/tt leagues *)

(* ages: each grade is based solely on age *)
(* crit: each grade is based on FTP W/kg *)
(* irongoat: each grade is based on FTP W/kg *)
(* tt: single grade *)

(* another thing we need to do is regenerate the final standings for each race *)


(*
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

let rider_points all_points = List.fold_left (+) 0 (top_6_positions all_points);;
let total_points r = rider_points r.rpoints + r.sprints;;


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
*)