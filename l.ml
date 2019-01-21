#!/usr/bin/env ocaml

#use "topfind";;
#require "atdgen";;
#require "curl";;
#require "str";;
#require "netstring";;
#directory "bin";;
#load "map2.cmo";;
#load "category.cmo";;
#load "results_j.cmo";;
#load "unix.cma";;
#load "encoding.cmo";;
#load "utils.cmo";;

open Results_t;;
module Cat = Category;;

module List = struct
    include List

    let rec take n list = match n, list with
    | 0, _ | _, [] -> []
    | _, x::xs -> x :: take (n-1) xs

    let rec take_while p = function
    | x :: xs when p x -> x :: take_while p xs
    | _ :: xs -> []
    | [] -> []

    let rec take_until p = function
    | x :: xs when not (p x) -> x :: take_until p xs
    | _ -> []

    let rec drop_while p = function
    | x :: xs when p x -> drop_while p xs
    | l -> l

    let rec drop_until p = function
    | x :: xs when not (p x) -> drop_until p xs
    | l -> l

    let rec max_by p = function
    | a :: b :: cs when p a b -> max_by p (a :: cs)
    | a :: c :: cs -> max_by p (c :: cs)
    | x :: [] -> x
    | [] -> failwith "empty list"

    let rec uniq_by p = function
    | a :: b :: cs when p a b -> a :: uniq_by p cs
    | x :: xs -> x :: uniq_by p xs
    | [] -> []

    let hd_opt = function
    | x :: _ -> Some x
    | _ -> None
end;;

module IntMap = Map2.Make(struct type t = int let compare = compare end);;

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
    let points = List.fold_left IntsDesc.union IntsDesc.empty (List.take (((participants-1)/5)+1) points) in
    match List.nth_opt (IntsDesc.elements points) position with
    | None -> 1
    | Some i -> i + bonus;;

(*let best_results rider =
    let rec take = function
    | _, 0 -> []
    | [], _ -> []
    | x::xs, n -> x :: take (xs, n-1)
    in take (List.rev (List.sort compare rider.race_points), 6);;*)

let int_of_category = Category.int_of_category;;
let num_categories = 5;;

let race_categories = [Cat.A; Cat.B; Cat.C; Cat.D];;

type result = {
    zwift_id: int;
    original_place: placing;
    mutable place: placing;
    entry_cat: Category.t;
    result_cat: Category.t;
}

let best_cat_results results =
    match List.hd_opt
        (List.drop_while ((=) [])
            (List.map (fun cat ->
                List.filter (fun r -> cat = r.result_cat) results)
                race_categories))
    with None -> [] | Some x -> x
;;

let get_name result zwifters =
    (List.find (fun z -> z.zwid = result.place.p_zwid) zwifters).name
;;

let format_place zwifters result =
    let place = result.place in
    Printf.sprintf "%s, %s, %d, %s, %s, %s, %d,"
        (Cat.unwrap result.result_cat) place.p_flag place.p_power_type (get_name result zwifters)
        place.p_zwid result.original_place.p_uid place.p_penalty
;;

let format_place_check zwifters result =
    let place = result.place in
    Printf.sprintf "%s => %s, %s, %d, %s, %s, %s, %d, %s"
        (Cat.unwrap result.entry_cat) (Cat.unwrap result.result_cat) place.p_flag place.p_power_type (get_name result zwifters)
        place.p_zwid result.original_place.p_uid place.p_penalty (if result.entry_cat <> result.result_cat then "UPG" else "")
;;

let check_places results zwifters =
    print_endline (String.concat "\n" (List.map (format_place_check zwifters) results))
;;

let edit_places event_id results zwifters =
    let data = Utils.replace_escapes (String.concat "\n" (List.map (format_place zwifters) results)) in
    try
        Str.search_forward (Str.regexp "&[a-z]+;") data 0;
        print_endline data;
        failwith "HTML entities found!"
    with Not_found ->
        ignore (Utils.submit_results event_id data)
;;

let edit_places_check event_id results zwifters =
    let data = Utils.replace_escapes (String.concat "\n" (List.map (format_place zwifters) results)) in
    try
        Str.search_forward (Str.regexp "&[a-z]+;") data 0;
        print_endline data;
        failwith "HTML entities found!"
    with Not_found ->
        print_endline data
;;

(* so we don't hit the API repeatedly *)
let zwift_places = ref IntMap.empty;;

let get_placings zwift_id = match IntMap.find_opt zwift_id !zwift_places with
    | Some placings -> placings
    | None ->
        let placings = Utils.fetch_placings (Printf.sprintf
            "https://www.zwiftpower.com/api3.php?do=profile_results&z=%d&type=all" zwift_id)
        in
        zwift_places := IntMap.add zwift_id placings !zwift_places;
        placings
;;

(* used for women, as we don't penalise upgrades *)
let results_prior_to_event zwift_id event_id =
    let placings = get_placings zwift_id in
    let placings = List.sort (fun p1 p2 ->
        match p1.p_event_date, p2.p_event_date with
        | Some d1, Some d2 -> compare d1 d2 | _ -> failwith "missing date") (List.concat (Array.to_list placings.placings))
    in
    (* last 30 days *)
    let last30 = List.take_while (fun p -> p.p_event_date > (Some (int_of_float (Unix.time ()) - 2592000))) (List.rev placings) in
    (* last 10 races if nothing in the past month *)
    let placings = if last30 = [] then
            List.take 10 (List.rev placings)
        else
            last30
    in
    (*let placings = List.take_while (fun p -> p.p_zid <> event_id) (List.rev placings) in*)
    let placings = List.take_until (fun p -> p.p_zid = string_of_int event_id) (List.rev placings) in
    (* this seems wrong... *)
    let placings = List.take 30 (List.rev placings) in
    placings
;;

let results_prior_week zwift_id event_ids =
    let first, second = event_ids in
    let placings = get_placings zwift_id in
    let first_result = List.find_opt (fun placing -> placing.p_zid = (string_of_int first))
        (List.concat (Array.to_list placings.placings))
    in
    let second_result = List.find_opt (fun placing -> placing.p_zid = (string_of_int second))
        (List.concat (Array.to_list placings.placings))
    in
    match first_result, second_result with 
    | Some r, None -> Some r
    | None, Some r -> Some r
    | None, None -> None
    | Some l, Some r -> Some { l with p_category = min l.p_category r.p_category; p_wkg_ftp = max l.p_wkg_ftp r.p_wkg_ftp }
;;

module type RaceType = sig
    val power_to_cat : float -> Category.t
end;;

module type RaceFunctions = sig
    val power_to_cat : float -> Category.t

    val cats_for_placings : Results_t.placing list -> int -> (int * int) option -> result list

    val cats_for_event : int -> (int * int) option -> result list

    val sort_into_cats : Results_t.placing list -> Results_t.placing list array
end;;

module Women = struct
    let power_to_cat wkg =
        if wkg >= 3.7 then Category.A
        else if wkg >= 3.2 then Category.B
        else if wkg >= 2.5 then Category.C
        else Category.D
end

module Mixed = struct
    let power_to_cat wkg =
        if wkg >= 4.0 then Category.A
        else if wkg >= 3.2 then Category.B
        else if wkg >= 2.5 then Category.C
        else Category.D
end

let fetch_event event_id =
    Utils.flatten_placings (Utils.fetch_placings (Printf.sprintf
        "https://www.zwiftpower.com/api3.php?do=event_results&zid=%d" event_id))
;;

module Results (T : RaceType) = struct
    let power_to_cat = T.power_to_cat

    let cats_for_placings placings event_id prev_event_ids =
        List.map (fun placing ->
            begin try
                let zwift_id = int_of_string placing.p_zwid in
                let prev_event_results = match prev_event_ids with
                        | None ->  None
                        | Some evt_ids -> results_prior_week zwift_id evt_ids
                in
                match prev_event_results with
                    | None ->
                        let results = results_prior_to_event (int_of_string placing.p_zwid) event_id in
                        {
                            zwift_id = zwift_id;
                            original_place = placing;
                            place = placing;
                            entry_cat = placing.p_category;
                            result_cat = min (* min here actually works out to be max *)
                                (T.power_to_cat (max (List.max_by (fun p1 p2 -> p1.p_wkg_ftp > p2.p_wkg_ftp) results).p_wkg_ftp placing.p_wkg_ftp))
                                placing.p_category
                            (* max of previous results, entered category, and 95% 20 min W/kg *)
                        }
                    | Some place ->
                        {
                            zwift_id = zwift_id;
                            original_place = placing;
                            place = place;
                            entry_cat = placing.p_category;
                            result_cat = min place.p_category (T.power_to_cat placing.p_wkg_ftp)
                        }
            with
            | Failure "empty list" -> {
                    zwift_id = int_of_string placing.p_zwid;
                    original_place = placing;
                    place = placing;
                    entry_cat = placing.p_category;
                    result_cat = min (T.power_to_cat placing.p_wkg_ftp) placing.p_category;
                }
            | exn ->
                Printf.printf "error processing results for %s\n" placing.p_zwid;
                raise exn
            end
        ) placings
    ;;

    let cats_for_event event_id prev_event_ids = cats_for_placings (fetch_event event_id) event_id prev_event_ids;;

    let sort_into_cats results =
        let arr = Array.make 7 [] in
            List.iter (fun result ->
                let ix = Category.int_of_category result.p_category in
                arr.(ix) <- result :: arr.(ix)) results;
            arr
    ;;
end;;

let points cat_results =
    let num_riders = Array.map List.length cat_results in
    Array.mapi (fun ix results ->
            List.map (fun placing ->
                    (*Printf.printf "%d : %d = %d\n" placing.p_position_in_cat num_riders.(ix) (position_to_points placing.p_position_in_cat num_riders.(ix));*)
                    placing, position_to_points placing.p_position_in_cat num_riders.(ix))
                results)
        cat_results
;;

type team_t = {
    t_tname: string;
    t_tc: string;
    t_tbc: string;
    t_tbd: string;
    t_tid: string;
}

let team_of_placing p = {
    t_tname = p.p_tname;
    t_tc = p.p_tc;
    t_tbc = p.p_tbc;
    t_tbd = p.p_tbd;
    t_tid = p.p_tid;
}

module TeamMap = Map2.Make(struct type t = team_t let compare lhs rhs = compare lhs.t_tid rhs.t_tid end);;

(* all_points : Results_t.placing list array list *)
let team_points all_points =
    let team_cats = Array.make 4 TeamMap.empty in
    List.iter (fun race ->
        Array.iteri (fun ix results ->
            List.iter (fun (placing,points) ->
                let team = team_of_placing placing in
                let points = points + 1 in
                if String.length team.t_tname > 0 then begin
                    team_cats.(ix) <- TeamMap.update team (function
                    | None -> Some (points, 1)
                    | Some (pts,evts) -> Some (pts+points,evts+1)) team_cats.(ix)
                end) results
        ) race
    ) all_points;
    team_cats
;;

let best_points race1 race2 =
    let cats = Array.map2 (fun a b -> List.concat [a;b]) race1 race2 in
    let results = Array.map (fun results -> List.sort (fun (p1,s1) (p2,s2) ->
            if p1.p_zwid = p2.p_zwid then begin
                if s2 > s1 then 1
                else if s1 = s2 then 0
                else -1
            end else if p1.p_zwid > p2.p_zwid then 1 else 0)
        results) cats
    in
    Array.map (fun results -> List.sort (fun (_,s1) (_,s2) -> compare s2 s1) (List.uniq_by (fun a b ->
        (fst a).p_zwid = (fst b).p_zwid) results)) results
;;

(*let round1 = best_points
    (points (sort_into_cats (fetch_event "129382")))
    (points (sort_into_cats (fetch_event "129383")));;*)

(*let race1 = sort_into_cats (fetch_event "129382");;
let race2 = sort_into_cats (fetch_event "129383");;

let team = team_points [points race1; points race2];;*)

let ix_to_string = function
| 0 -> "A" | 1 -> "B" | 2 -> "C" | 3 -> "D" | _ -> "INV";;

let print_table data =
    Array.iteri (fun ix results ->
        Printf.printf "Category %s\n" (ix_to_string ix);
        List.iteri (fun iy (placing, points) ->
                Printf.printf "%d. %s: %d\n" (iy+1) placing.p_name points)
            results) data
;;

let write_csv oc data =
    Printf.fprintf oc "category,events,points,tname,tc,tbc,tbd,name,flag,zwid\n";
    Array.iteri (fun ix results ->
        List.iteri (fun iy (placing, points) ->
                Printf.fprintf oc "%s,%d,%d,%s,%s,%s,%s,%s,%s,%s\n"
                    (Category.unwrap placing.p_category)
                    1 points placing.p_tname placing.p_tc placing.p_tbc placing.p_tbd
                    placing.p_name placing.p_flag placing.p_zwid
                )
            results) data
;;

let write_team_csv oc data = 
    Printf.fprintf oc "category,events,points,tname,tc,tbc,tbd\n";
    Array.iteri (fun ix results ->
        List.iter (fun (team, (points, events)) ->
            Printf.fprintf oc "%s,%d,%d,%s,%s,%s,%s\n"
                    (ix_to_string ix)
                    events points team.t_tname team.t_tc team.t_tbc team.t_tbd
                )
        (List.sort (fun a b -> compare (fst (snd b)) (fst (snd a))) (TeamMap.bindings results))
    ) data
;;

let print_csv data = write_csv stdout data;;

let print_team_csv data = write_team_csv stdout data;;

let rec ok prompt =
    Printf.printf "%s: [y/N] " prompt;
    match read_line () with
    | "Y" | "y" | "yes" -> true
    | "N" | "n" | "no" | "" -> false
    | _ -> ok prompt
;;

let rec run () =
    let women = ok "Women" in
    let raceModule : (module RaceFunctions) = if women then (module Results(Women)) else (module Results(Mixed)) in
    let module M = (val raceModule : RaceFunctions) in
    Printf.printf "Race 1: ";
    let first = int_of_string (read_line ()) in
    Printf.printf "Prev Race 1: ";
    let first_prev = (match read_line () with | "" -> None | x -> Some (int_of_string x)) in
    Printf.printf "Race 2: ";
    let second = int_of_string (read_line ()) in
    Printf.printf "Prev Race 2: ";
    let second_prev = (match read_line () with | "" -> None | x -> Some (int_of_string x)) in
    let prevs = match first_prev, second_prev with
    | None, None -> None
    | Some a, Some b -> Some (a,b)
    | _ -> failwith "must have two events or no events"
    in
    if ok "Fetch and update categories" then begin
        let first_cats = M.cats_for_event first prevs in
        let zwifters = Utils.fetch_zwifters (Printf.sprintf "https://www.zwiftpower.com/api3.php?do=event_results_zwift&zid=%d" first) in
        check_places first_cats zwifters.zwifters;
        if ok (Printf.sprintf "Submit categories for %d to ZwiftPower" first) then begin
            edit_places first first_cats zwifters.zwifters;
        end;
        (*if ok "Show form data" then
            edit_places_check first first_cats;*)
        let second_cats = M.cats_for_event second prevs in
        let zwifters = Utils.fetch_zwifters (Printf.sprintf "https://www.zwiftpower.com/api3.php?do=event_results_zwift&zid=%d" second) in
        check_places second_cats zwifters.zwifters;
        if ok (Printf.sprintf "Submit categories for %d to ZwiftPower" second) then begin
            edit_places second second_cats zwifters.zwifters;
        end;
        (*if ok "Show form data" then
            edit_places_check first first_cats;*)
    end;
    let first_race = points (M.sort_into_cats (fetch_event first)) in
    let second_race = points (M.sort_into_cats (fetch_event second)) in
    if ok (Printf.sprintf "Show table for %d" first) then
        print_table first_race;
    if ok (Printf.sprintf "Show table for %d" second) then
        print_table second_race;
    if ok (Printf.sprintf "Show table for individual points") then
        print_table (best_points first_race second_race);
    if ok (Printf.sprintf "Dump CSV for individual points") then
        print_csv (best_points first_race second_race);
    let team = team_points [first_race; second_race] in
    if ok (Printf.sprintf "Dump CSV for team points") then
        print_team_csv team;
    if ok "Save to file" then begin
        let oc_ind = open_out "current_ind.csv" in
        let oc_team = open_out "current_team.csv" in
        write_csv oc_ind (best_points first_race second_race);
        write_team_csv oc_team team;
        close_out oc_ind;
        close_out oc_team;
    end;
    Printf.printf "Complete!\n"
;;

(* Results_t.placing list array *)
let league_results league =
    let module M = Results(Mixed) in
    Array.map2 (fun event_id placings ->
            M.cats_for_placings placings event_id)
        league (Array.map fetch_event league)
;;

(* Results_t.placing list array array *)
(*let league_sorted_results league =
    let module M = Results(Mixed) in
    let results = league_results league in
    Array.map (fun placings ->
        M.sort_into_cats placings) league
;;*)

exception Found of int;;

let array_index_of v a = try
        for i = 0 to Array.length a - 1 do
            if a.(i) = v then raise (Found i)
        done;
        -1 (* not found *)
    with Found i -> i
;;

let leagues_by_zwifter league =
    let results : Results_t.placing list array = league_results league in
    let event_zwifters = ref IntMap.empty in
    Array.iter2 (fun event_id placings -> (* results in an event *)
        List.iter (fun placing -> (* a single result in the event *)
            let ix = array_index_of event_id league in
            Printf.printf "event index for %d is %d\n" event_id ix;
            Printf.printf "updating data for zwifter %s\n" placing.p_zwid;
            match IntMap.find_opt (int_of_string placing.p_zwid) !event_zwifters with
            | None ->
                event_zwifters := IntMap.add (int_of_string placing.p_zwid)
                    (Array.mapi (fun i x -> if i = ix then Some placing else None) league)
                    !event_zwifters
            | Some array ->
                array.(ix) <- Some placing
        ) placings) league results;
    !event_zwifters
;;

let best_cat_results results =
    let list = List.map (function Some x -> x) (List.filter ((<>) None) (Array.to_list results)) in
    match List.hd_opt
        (List.drop_while ((=) [])
            (List.map (fun cat ->
                List.filter (fun p -> cat = p.p_category) list)
                race_categories))
    with None -> [] | Some x -> x
;;

let results leagues =
    let x = IntMap.bindings (leagues_by_zwifter leagues) in
    let y = List.map (fun (id,results) -> best_cat_results results) x in
    let z = List.concat y in
    let events = ref IntMap.empty in
    List.iter (fun place ->
        let id = int_of_string place.p_zid in
        events := IntMap.update id (function
            | None -> Some [place]
            | Some list -> Some (place :: list)) !events) z;
    !events
;;

(* also need to recalculate positions... or can we just push edits to ZP, then fetch again? *)

(* and then need to split back into events *)

(*(* this is only data for a single race, not a league... *)
let race = Results_j.race_of_string (Utils.read_file "ages.results.json");;
let sprints = Results_j.sprints_of_string (Utils.read_file "ages.sprints.json");;

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
*)*)
