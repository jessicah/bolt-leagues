#!/usr/bin/env ocaml

#use "topfind";;
#thread;;
#require "atdgen";;
#require "curl";;
#require "str";;
#require "netstring";;
#require "sqlexpr";;
#require "ppx_sqlexpr";;
#directory "bin";;
#load "map2.cmo";;
#load "category.cmo";;
#load "results_j.cmo";;
#load "unix.cma";;
#load "encoding.cmo";;
#load "utils.cmo";;
#use "db.ml";;

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

#use "events.ml";;

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

let all_upgrades = Hashtbl.create 16;;

let upgraded results =
    List.filter (fun result ->
        begin match result.result_cat with
            | Flagged reason ->
                Printf.printf "Rider %s disqualified for %s\n" result.original_place.p_zwid reason
            | _ -> ()
        end;
        if result.entry_cat <> result.result_cat then begin
            Printf.printf "Rider %s upgraded from %s to %s; (original: %1.2f) (place: %1.2f)\n" result.original_place.p_zwid
                (Category.unwrap result.entry_cat) (Category.unwrap result.result_cat)
                result.original_place.p_wkg_ftp result.place.p_wkg_ftp;
            Hashtbl.add all_upgrades result.original_place.p_zwid (result.entry_cat, result.result_cat);
        end;
        result.entry_cat <> result.result_cat || result.result_cat > Category.U) results
;;

let format_place zwifters result =
    let place = result.place in
    Printf.sprintf "%s, %s, %d, %s, %s, %s, %d,"
        (Cat.unwrap result.result_cat) place.p_flag place.p_power_type (get_name result zwifters)
        place.p_zwid result.original_place.p_uid place.p_penalty
;;

let format_place_check zwifters result =
    let place = result.place in
    let escape = if result.entry_cat <> result.result_cat then "\x1B[31m" else "" in
    Printf.sprintf "%s%s => %s, %s, %d, %s, %s, %s, %d, %s\x1B[39m"
        escape (Cat.unwrap result.entry_cat) (Cat.unwrap result.result_cat) place.p_flag place.p_power_type (get_name result zwifters)
        place.p_zwid result.original_place.p_uid place.p_penalty (if result.entry_cat <> result.result_cat then "UPG" else "")
;;

let check_places results zwifters =
    print_endline (String.concat "\n" (List.map (format_place_check zwifters) results))
;;

let edit_places event_id results zwifters =
    let results = upgraded results in
    let data = Utils.replace_escapes (String.concat "\n" (List.map (format_place zwifters) results)) in
    try
        Str.search_forward (Str.regexp "&[a-z]+;") data 0;
        print_endline data;
        failwith "HTML entities found!"
    with Not_found ->
        ignore (Utils.submit_results event_id data)
;;

let reset_places event_id =
    Utils.submit_results event_id "" |> ignore
;;

let edit_places_check event_id results zwifters =
    let results = upgraded results in
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
    | Some placings ->
        if !Utils.verbose then
            Printf.printf "\x1B[92mFetched https://www.zwiftpower.com/api3.php?do=profile_results&z=%d&type=all (from cache)...\x1B[39m\n%!" zwift_id;
        placings
    | None ->
        let placings = Utils.fetch_placings (Printf.sprintf
            "https://www.zwiftpower.com/api3.php?do=profile_results&z=%d&type=all" zwift_id)
        in
        zwift_places := IntMap.add zwift_id placings !zwift_places;
        placings
;;

let fetch_event event_id =
    Utils.flatten_placings (Utils.fetch_placings (Printf.sprintf
        "https://www.zwiftpower.com/api3.php?do=event_results&zid=%d" event_id))
;;

(* used for women, as we don't penalise upgrades *)
let results_prior_to_event zwift_id event_id =
    let placings = get_placings zwift_id in
    let placings = List.sort (fun p1 p2 ->
        match p1.p_event_date, p2.p_event_date with
        | Some d1, Some d2 -> compare d1 d2 | _ -> failwith "missing date") (List.concat (Array.to_list placings.placings))
    in
    let placings = List.filter (fun p -> p.p_category <> Flagged "ERROR") placings in
    let events_before_date = match (List.find (fun p -> p.p_zid = string_of_int event_id) placings).p_event_date with
        | None ->
            print_endline "WARNING: unable to find current event date!";
            Unix.time () |> int_of_float
        | Some date -> date
    in
    (* last 30 days *)
    let last30 = List.take_while (fun p -> p.p_event_date > (Some (events_before_date - 2592000))) (List.rev placings) in
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
    let placings = (get_placings zwift_id).placings |> Array.to_list |> List.concat in
    let results = List.map (fun event_id ->
        List.find_opt (fun placing -> placing.p_zid = event_id) placings) event_ids in
    let rec loop placing = function
    | None :: tail -> loop placing tail
    | p :: tail -> loop p tail
    | [] -> placing
    in loop None results
;;

module type RaceType = sig
    val power_to_cat : float -> Category.t

    val placing_to_cat : Results_t.placing -> Category.t

    val uses_race_history : bool
end;;

module type RaceFunctions = sig
    val power_to_cat : float -> Category.t

    val placing_to_cat : Results_t.placing -> Category.t

    val cats_for_placings : Results_t.placing list -> int -> string list -> result list

    val cats_for_event : int -> string list -> result list

    val sort_into_cats : Results_t.placing list -> Results_t.placing list array
end;;

module Women = struct
    let power_to_cat wkg =
        if wkg >= 3.7 then Category.A
        else if wkg >= 3.2 then Category.B
        else if wkg >= 2.5 then Category.C
        else Category.D
    
    let placing_to_cat p = power_to_cat p.p_wkg_ftp

    let uses_race_history = true
end

module Mixed = struct
    let power_to_cat wkg =
        if wkg >= 4.0 then Category.A
        else if wkg >= 3.2 then Category.B
        else if wkg >= 2.5 then Category.C
        else Category.D
    
    let placing_to_cat p = power_to_cat p.p_wkg_ftp

    let uses_race_history = true
end

module Ages = struct
    let power_to_cat age =
        if age < 39. then Category.A
        else if age < 47. then Category.B
        else if age < 52. then Category.C
        else Category.D
    
    let placing_to_cat p =
        if p.p_age <= 38 then Category.A
        else if p.p_age <= 46 then Category.B
        else if p.p_age <= 51 then Category.C
        else Category.D
    
    let uses_race_history = false
end

let max_by f a b = max (f a) (f b)
;;

module Results (T : RaceType) = struct
    let power_to_cat = T.power_to_cat

    let placing_to_cat = T.placing_to_cat

    let cats_for_placings placings event_id prev_event_ids =
        Printf.printf "Processing event %d\n" event_id;
        List.map (fun placing ->
            begin try
                let zwift_id = int_of_string placing.p_zwid in
                let prev_event_results = match prev_event_ids with
                        | [] ->  None
                        | evt_ids -> results_prior_week zwift_id evt_ids
                in
                match prev_event_results, T.uses_race_history with
                    | None, true ->
                        let results = results_prior_to_event (int_of_string placing.p_zwid) event_id in
                        (*let best : Results_t.placing = List.max_by (fun p1 p2 -> p1.p_wkg_ftp > p2.p_wkg_ftp) (placing :: results) in*)
                        let best_cat = List.fold_left (fun cat p -> min cat (T.placing_to_cat p)) Category.D (placing :: results) in
                        {
                            zwift_id = zwift_id;
                            original_place = placing;
                            place = placing;
                            entry_cat = placing.p_category;
                            result_cat = min (* min here actually works out to be max *)
                                best_cat
                                placing.p_category
                            (* max of previous results, entered category, and 95% 20 min W/kg *)
                        }
                    | Some place, _ ->
                        {
                            zwift_id = zwift_id;
                            original_place = placing;
                            place = place;
                            entry_cat = placing.p_category;
                            result_cat = min placing.p_category (min place.p_category (T.placing_to_cat placing))
                        }
                    | None, false ->
                        {
                            zwift_id = zwift_id;
                            original_place = placing;
                            place = placing;
                            entry_cat = placing.p_category;
                            result_cat = min placing.p_category (T.placing_to_cat placing)
                        }
            with
            | Failure "empty list" -> {
                    zwift_id = int_of_string placing.p_zwid;
                    original_place = placing;
                    place = placing;
                    entry_cat = placing.p_category;
                    result_cat = min (T.placing_to_cat placing) placing.p_category;
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

let rec merge_points = function
| (left_place, left_score) :: (right_place, right_score) :: rest when left_place.p_zwid = right_place.p_zwid ->
    (left_place, (max left_score right_score) + 2) :: merge_points rest
| (place, score) :: rest -> (place, score + 1) :: merge_points rest
| [] -> []
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

let best_points2 race1 race2 =
    let cats = Array.map2 (fun a b -> List.concat [a;b]) race1 race2 in
    let results = Array.map (fun results -> List.sort (fun (p1,s1) (p2,s2) ->
            if p1.p_zwid = p2.p_zwid then begin
                if s2 > s1 then 1
                else if s1 = s2 then 0
                else -1
            end else if p1.p_zwid > p2.p_zwid then 1 else 0)
        results) cats
    in
    Array.map (fun results -> List.sort (fun (_,s1) (_,s2) -> compare s2 s1) (merge_points results)) results
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
    Printf.fprintf oc "category,events,points,tname,tid,tc,tbc,tbd,name,flag,zwid\n";
    Array.iteri (fun ix results ->
        List.iteri (fun iy (placing, points) ->
                Printf.fprintf oc "%s,%d,%d,%s,%s,%s,%s,%s,%s,%s,%s\n"
                    (Category.unwrap placing.p_category)
                    1 points placing.p_tname placing.p_tid placing.p_tc placing.p_tbc placing.p_tbd
                    placing.p_name placing.p_flag placing.p_zwid
                )
            results) data
;;

let write_team_csv oc data = 
    Printf.fprintf oc "category,events,points,tid,tname,tc,tbc,tbd\n";
    Array.iteri (fun ix results ->
        List.iter (fun (team, (points, events)) ->
            Printf.fprintf oc "%s,%d,%d,%s,%s,%s,%s,%s\n"
                    (ix_to_string ix)
                    events points team.t_tid team.t_tname team.t_tc team.t_tbc team.t_tbd
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

module StringMap = Map2.Make(String);;

let podiums races =
    let map = ref StringMap.empty in
    let position_to_string = function 1 -> "gold" | 2 -> "silver" | 3 -> "bronze" | _ -> failwith "invalid position" in
    List.iter (fun race ->
        Array.iter (fun places ->
            List.iter (fun place ->
                match place.p_position_in_cat with
                | 1 | 2 | 3 ->
                    map := StringMap.update place.p_zwid (function
                        | None -> Some [position_to_string place.p_position_in_cat]
                        | Some list -> Some (position_to_string place.p_position_in_cat :: list)) !map
                | _ -> ()) places) race) races;
    !map
;; 

let print_podiums_csv table =
    StringMap.iter (fun zwid trophies ->
        let trophies = List.sort (fun left right ->
            if left = "gold" then -1
            else if right = "gold" then 1
            else if left = "silver" then -1
            else if right = "silver" then 1
            else if left = "bronze" then -1
            else 1) trophies in
        Printf.printf "%s,%s\n" zwid (String.concat " " trophies)) table
;;

let write_podiums_csv oc table =
    Printf.fprintf oc "zwid,podiums\n";
    StringMap.iter (fun zwid trophies ->
        let trophies = List.sort (fun left right ->
            if left = "gold" then -1
            else if right = "gold" then 1
            else if left = "silver" then -1
            else if right = "silver" then 1
            else if left = "bronze" then -1
            else 1) trophies in
        Printf.fprintf oc "%s,%s\n" zwid (String.concat " " trophies)) table
;;

let rec run first second previous =
    let women = ok "Women" in
    let ages = ok "ages" in
    let raceModule : (module RaceFunctions) = if women then (module Results(Women)) else if ages then (module Results(Ages)) else (module Results(Mixed)) in
    let module M = (val raceModule : RaceFunctions) in
    if ok "Fetch and update categories" then begin
        let first_cats = M.cats_for_event first previous in
        let zwifters = Utils.fetch_zwifters (Printf.sprintf "https://www.zwiftpower.com/api3.php?do=event_results_zwift&zid=%d" first) in
        check_places first_cats zwifters.zwifters;
        if ok (Printf.sprintf "Submit categories for %d to ZwiftPower" first) then begin
            edit_places first first_cats zwifters.zwifters;
        end;
        let second_cats = M.cats_for_event second previous in
        let zwifters = Utils.fetch_zwifters (Printf.sprintf "https://www.zwiftpower.com/api3.php?do=event_results_zwift&zid=%d" second) in
        check_places second_cats zwifters.zwifters;
        if ok (Printf.sprintf "Submit categories for %d to ZwiftPower" second) then begin
            edit_places second second_cats zwifters.zwifters;
        end;
    end;
    let first_race = fetch_event first |> M.sort_into_cats in
    let second_race = fetch_event second |> M.sort_into_cats in
    let first_race_points = points first_race in
    let second_race_points = points second_race in
    if ok (Printf.sprintf "Show table for %d" first) then
        print_table first_race_points;
    if ok (Printf.sprintf "Show table for %d" second) then
        print_table second_race_points;
    if ok (Printf.sprintf "Show table for individual points") then
        print_table (best_points2 first_race_points second_race_points);
    if ok (Printf.sprintf "Dump CSV for individual points") then
        print_csv (best_points2 first_race_points second_race_points);
    let team = team_points [first_race_points; second_race_points] in
    if ok (Printf.sprintf "Dump CSV for team points") then
        print_team_csv team;
    if ok "Dump CSV for podiums" then
        podiums [first_race; second_race] |> print_podiums_csv;
    if ok "Save to file" then begin
        let oc_ind = open_out "current_ind.csv" in
        let oc_team = open_out "current_team.csv" in
        let oc_podiums = open_out "current_podiums.csv" in
        write_csv oc_ind (best_points2 first_race_points second_race_points);
        write_team_csv oc_team team;
        write_podiums_csv oc_podiums (podiums [first_race; second_race]);
        close_out oc_ind;
        close_out oc_team;
        close_out oc_podiums;
    end;
    Printf.printf "Complete!\n"
;;

let process_single event previous functions =
    let module M = (val functions : RaceFunctions) in
    if ok (Printf.sprintf "Fetch and update categories for %d" event) then begin
        let cats = M.cats_for_event event previous in
        let zwifters = Utils.fetch_zwifters (Printf.sprintf "https://www.zwiftpower.com/api3.php?do=event_results_zwift&zid=%d" event) in
        check_places cats zwifters.zwifters;
        if ok (Printf.sprintf "Submit categories for %d to ZwiftPower" event) then begin
            edit_places event cats zwifters.zwifters;
        end
    end;
    let race = fetch_event event |> M.sort_into_cats in
    let race_points = points race in
    if ok (Printf.sprintf "Show table for individual points") then
        print_table race_points;
    if ok (Printf.sprintf "Dump CSV for individual points") then
        print_csv race_points;
    let team = team_points [race_points] in
    if ok (Printf.sprintf "Dump CSV for team points") then
        print_team_csv team;
    if ok "Dump CSV for podiums" then
        podiums [race] |> print_podiums_csv;
    (race, race_points, team, podiums [race])
;;

let process_ro event previous functions =
    let module M = (val functions : RaceFunctions) in
    let cats = M.cats_for_event event previous in
    let zwifters = Utils.fetch_zwifters (Printf.sprintf "https://www.zwiftpower.com/api3.php?do=event_results_zwift&zid=%d" event) in
    check_places cats zwifters.zwifters;
    let race = fetch_event event |> M.sort_into_cats in
    let race_points = points race in
    event, race
;; (* returns race sorted into cats, event_id * Results_t.placing list array *)

let process_rw event previous functions =
    let module M = (val functions : RaceFunctions) in
    (* ensure place data hasn't been tampered with prior to processing *)
    reset_places event;
    let cats = M.cats_for_event event previous in
    let zwifters = Utils.fetch_zwifters (Printf.sprintf "https://www.zwiftpower.com/api3.php?do=event_results_zwift&zid=%d" event) in
    edit_places event cats zwifters.zwifters;
    let race = fetch_event event |> M.sort_into_cats in
    let race_points = points race in
    event, race;;

let process f events =
    let women = ok "Women" in
    let ages = ok "ages" in
    let raceModule : (module RaceFunctions) =
        if women then (module Results(Women))
        else if ages then (module Results(Ages))
        else (module Results(Mixed))
    in
    let events = Array.to_list events in
    let rec loop acc previous = function
    | x :: xs ->
        let data = f x (List.map string_of_int previous) raceModule in
        loop (data :: acc) (List.append previous [x]) xs
    | [] -> print_endline "processed all events"; List.rev acc
    in loop [] [] events
;;

let gogogo db all_events =
    List.iter (fun (event_id, event_in_cats) ->
        Array.iteri (fun ix results_in_cat ->
            if ix < 6 then begin
            let category = Category.unwrap (Category.categories.(ix)) in
            List.iter (fun placing ->
                let zwid = int_of_string (placing.p_zwid) in
                let race = event_id in
                let position = placing.p_position_in_cat in
                let name = placing.p_name in
                let flag = if String.length placing.p_flag = 0 then None else Some placing.p_flag in
                let team = if String.length placing.p_tid = 0 then None else match Team.get db (int_of_string (placing.p_tid)) with
                | None ->
                    if String.length placing.p_tid > 0 then begin
                        let new_team = {
                            tid = int_of_string placing.p_tid; tname = placing.p_tname; tc = placing.p_tc; tbc = placing.p_tbc; tbd = placing.p_tbd
                        } in Team.insert db new_team;
                        Some new_team
                    end else None
                | t -> t
                in
                Individual.add_result db zwid race category position name flag team) results_in_cat
            end) event_in_cats) all_events
;;

(* example fetch from database *)

(* count of races * individual_id list *)
let completed_events db =
    Sqlexpr.select db
        [%sqlc "SELECT @d{COUNT(race_id)}, @d{individual_id} FROM race_result GROUP BY individual_id"]

let completed_events2 db =
    Sqlexpr.select db
        [%sqlc "SELECT @d{r.total}, @d{i.individual_id} FROM individual i LEFT JOIN (SELECT COUNT(race_id) AS total, individual_id FROM race_result r GROUP BY individual_id) as r on i.individual_id = r.individual_id"]

(* count of racers * category * event_id list *)
let total_racers db =
    Sqlexpr.select db
        [%sqlc "SELECT @d{COUNT(individual_id)}, @s{category}, @d{race_id} FROM race_result GROUP BY race_id, category"]

(* individual_id list ; may contain duplicates *)
let podiums db pos =
    Sqlexpr.select db
        [%sqlc "SELECT @d{individual_id} FROM race_result WHERE position = %d"] pos

(* individual_id * category list ; may contain duplicate individuaL_ids *)
let rider_and_category db =
    Sqlexpr.select db
        [%sqlc "SELECT DISTINCT @d{individual_id}, @s{category} FROM race_result GROUP BY individual_id, category ORDER BY individual_id, category"]

let rider_results db id =
    Sqlexpr.select db
        [%sqlc "SELECT @d{individual_id}, @s{category}, @d{race_id} FROM race_result WHERE individual_id = %d"] id

(* individual_id list ; may contain duplicates *)
let rec upgrades = function
    | (id1, cat1) :: (id2, cat2) :: rest when id1 = id2 ->
        Printf.printf "%d upgraded from %s to %s\n" id1 (max cat1 cat2) (min cat1 cat2);
        id1 :: upgrades ((id2, cat2) :: rest)
    | _ :: xs -> upgrades xs
    | [] -> []
;;

(* individual_id list *)
let racers db =
    Sqlexpr.select db
        [%sqlc "SELECT DISTINCT @d{individuaL_id} FROM race_result"]

let event_ids db =
    Sqlexpr.select db
        [%sqlc "SELECT DISTINCT @d{race_id} FROM race_result ORDER BY race_id"]

(* data we want for individuals:
   category, events, points, tname, tid, tc, tbc, tbd, name, flag, zwid, podiums

   points = best of two per round + events + number of upgrades *)

let bonuses db =
    let upgrades = rider_and_category db |> upgrades in
    let completed = completed_events db in
    racers db
    |> List.map (fun individual ->
        let upgrade_bonus = List.filter (fun id -> id = individual) upgrades |> List.length in
        individual, upgrade_bonus)
    |> List.map (fun (individual, points) ->
        let event_bonus = List.find (fun (events, id) -> id = individual) completed |> fst in
        individual, points + event_bonus)
;;

let no_bonuses db =
    racers db |> List.map (fun individual -> individual, 0)
;;

type result = {
    id : int;
    event : int;
    team : int option;
    category : string;
    mutable points : int;
}

let points db =
    let totals = total_racers db in
    Sqlexpr.select db
        [%sqlc "SELECT @d{individual_id}, @d{race_id}, @d?{team_id}, @d{position}, @s{category} FROM race_result"]
    |> List.map (fun (id, event_id, team_id, position, category) ->
        let participants = List.find (fun (_, cat, event) -> cat = category && event = event_id) totals |> (fun (total, _, _) -> total) in
        { id = id; event = event_id; team = team_id; category = category; points = position_to_points position participants})

let same_round event_list left right =
    Array.fold_left (fun is_same (first, second) ->
        if left = first && right = second then true else is_same) false event_list
;;

let best_points_wild event_list db =
    let all_points = points db |> List.sort (fun l r -> let v = compare l.id r.id in if v = 0 then compare l.event r.event else v) in
    let rec merge = function
    | a :: b :: cs when a.id = b.id && same_round event_list a.event b.event ->
        if a.points > b.points then
            a :: merge cs
        else
            b :: merge cs
    | x :: xs -> x :: merge xs
    | [] -> []
    in merge all_points
;;

let best_points_std db =
    points db |> List.sort (fun l r -> let v = compare l.id r.id in if v = 0 then compare l.event r.event else v)
;;

let halve_points_below_cat results =
    let best_cat = List.fold_left (fun cat result -> min cat result.category) "D" results in
    List.iter (fun result ->
        if result.category <> best_cat then result.points <- result.points / 2) results;
    results
;;

let dq_points_below_cat results =
    let best_cat = List.fold_left (fun cat result -> min cat result.category) "D" results in
    List.iter (fun result ->
        if result.category <> best_cat then result.points <- 0) results;
    results
;;

let total_points db bonuses_f best_results_f transform_points_f =
    (*let best_points = best_points db in*)
    let best_points = best_results_f db in
    let all_bonuses = bonuses_f db in
    racers db
    |> List.map (fun individual ->
        let points = List.filter (fun r -> r.id = individual) best_points
            (* probably here that we run a transform over points, e.g. races below best cat, points = 0, or points = half *)
            |> transform_points_f
            |> List.map (fun r -> r.points)
            (* take top 6 *)
            |> List.sort compare |> List.rev |> List.take 6
            (* sum *)
            |> List.fold_left (+) 0 in
        individual, points)
    |> List.map (fun (individual, points) ->
        let bonus = List.find (fun (id,_) -> id = individual) all_bonuses |> snd in
        individual, points + bonus)

let all_podiums db =
    let gold, silver, bronze = podiums db 1, podiums db 2, podiums db 3 in
    racers db |>
    List.map (fun individual ->
        let gold = gold |> List.filter ((=) individual) |> List.map (fun _ -> "gold") in
        let silver = silver |> List.filter ((=) individual) |> List.map (fun _ -> "silver") in
        let bronze = bronze |> List.filter ((=) individual) |> List.map (fun _ -> "bronze") in
        individual, String.concat " " (List.concat [gold; silver; bronze]))

let get_individuals db = Sqlexpr.select db [%sqlc "SELECT @d{i.individual_id}, @s{i.individual_name}, @s?{i.individual_flag}, @s?{team_id},  @s{r.category} FROM individual i INNER JOIN
  (SELECT individual_id, group_concat(team_id) as team_id, MIN(category) as category FROM race_result GROUP BY individual_id ORDER BY race_id) r ON i.individual_id = r.individual_id"];;

let final_results db points =
    let individuals = get_individuals db in
    let completed = completed_events db in
    let podiums = all_podiums db in
    let upgrades = rider_and_category db |> upgrades in
    List.map (fun (id, name, flag, teams, category) ->
        let zwid = string_of_int id in
        let flag = match flag with None -> "" | Some flag -> flag in
        let tid, tname, tc, tbc, tbd = match teams with
                | None -> "", "", "", "", ""
                | Some s -> match Str.split (Str.regexp ",") s |> List.rev |> List.hd |> int_of_string |> Team.get db with
                    | None -> "", "", "", "", ""
                    | Some t -> string_of_int t.tid, t.tname, t.tc, t.tbc, t.tbd
        in
        let points = List.find (fun (individual,_) -> id = individual) points |> (fun (_,points) -> string_of_int points) in
        let events = List.find (fun (_,individual) -> id = individual) completed |> (fun (events,_) -> string_of_int events) in
        let podiums = List.find (fun (individual,_) -> id = individual) podiums |> snd in
        let upgrades = List.filter (fun individual -> id = individual) upgrades |> List.length |> string_of_int in
        category :: events :: points :: tname :: tid :: tc :: tbc :: tbd :: name :: flag :: zwid :: podiums :: upgrades :: []) individuals
    |> List.sort (fun left right ->
        let v = compare (List.nth left 0) (List.nth right 0) in
        if v = 0 then compare (int_of_string (List.nth right 2)) (int_of_string (List.nth left 2)) else v);;

let team_points db with_bonus =
    let team_results =
        Sqlexpr.select db
            [%sql "SELECT @d{r.individual_id}, @d{r.team_id}, @d{r.race_id}, @s{r.category}, @d{r.position}, @d{t.total}
                    FROM race_result r INNER JOIN
                        (SELECT COUNT(individual_id) AS total, race_id, category FROM race_result GROUP BY race_id, category) t
                    ON r.race_id = t.race_id AND r.category = t.category
                    INNER JOIN team t ON r.team_id = t.team_id
                    WHERE r.team_id <> 0 AND t.team_name NOT LIKE ''
                    ORDER BY r.race_id, r.category, r.position"]
    in
    let table = Hashtbl.create 25 in
    (* we eventually need: category, events, points, tid, tname, tc, tbc, tbd *)
    team_results |> List.iter (fun (zwift_id, team_id, event_id, category, position, participants) ->
        match Hashtbl.find_opt table (team_id, category) with
        | None -> Hashtbl.add table (team_id, category) (position_to_points position participants, 1)
        | Some (points, events) -> Hashtbl.replace table (team_id, category) (points + position_to_points position participants, events + 1));
    Hashtbl.fold (fun team points acc -> (team, points) :: acc) table []
    |> List.map (fun (team, (points, events)) ->
        if with_bonus then (team, (points+events, events)) else (team, (points,events)))
    |> List.sort (fun ((_, category), (points, _)) ((_, category'), (points', _))->
        let v = compare category category' in if v = 0 then compare points' points else v)
    |> List.map (fun ((team_id, category), (points, events)) ->
        let tid, tname, tc, tbc, tbd = match Team.get db team_id with
            | None -> failwith "Unable to find team?!"
            | Some t -> string_of_int t.tid, t.tname, t.tc, t.tbc, t.tbd
        in
        category :: string_of_int events :: string_of_int points :: tname :: tid :: tc :: tbc :: tbd :: [])
;;

let print_results rows =
    print_endline "category,events,points,tname,tid,tc,tbc,tbd,name,flag,zwid,podiums";
    List.iter (fun [category; events; points; tname; tid; tc; tbc; tbd; name; flag; zwid; podiums; upgrades] ->
        Printf.printf "%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n"
            category events points tname tid tc tbc tbd name flag zwid podiums upgrades) rows;;

let print_team_results rows =
    print_endline "category,events,points,tid,tname,tc,tbc,tbd";
    List.iter (fun [category; events; points; tid; tname; tc; tbc; tbd] ->
        Printf.printf "%s,%s,%s,%s,%s,%s,%s,%s\n"
            category events points tname tid tc tbc tbd) rows;;

module Wild = struct
    let filtered () =
        all_wild |> Array.to_list
        |> List.map (fun id ->
            Utils.flatten_placings (Utils.fetch_placings (Printf.sprintf "https://www.zwiftpower.com/api3.php?do=event_results&zid=%d&type=filtered" id)))
        |> List.concat
        |> List.filter (fun r -> r.p_category = Category.Flagged "HR" || r.p_category = Category.Flagged "ZP")
    ;;

    let go name =
        let name = if String.length name = 0 then ":memory:" else name in
        let db = Sqlexpr.open_db name in
        Database.init_db db;
        let data = process process_ro all_wild in
        gogogo db data;
        (*let all_bonuses = bonuses db in*)
        let points = total_points db bonuses (best_points_wild wild) halve_points_below_cat
            (*|> List.map (fun (individual, points) ->
                let bonus = List.find (fun (id,_) -> id = individual) all_bonuses |> snd in
                individual, points + bonus)*)
        in
        final_results db points, team_points db true, db
;;
end

module Crit = struct
    let go () =
        Printexc.record_backtrace true;
        try
        let db = Sqlexpr.open_db ":memory:" in
        Database.init_db db;
        let data = process process_rw criterium in
        gogogo db data;
        let points = total_points db no_bonuses best_points_std dq_points_below_cat in
        final_results db points, team_points db false, db
        with exc -> Printexc.print_backtrace stdout; raise exc
end

module IronGoat = struct
    let go () =
        let db = Sqlexpr.open_db ":memory:" in
        Database.init_db db;
        let data = process process_rw irongoat in
        gogogo db data;
        let points = total_points db no_bonuses best_points_std dq_points_below_cat in
        final_results db points, team_points db false, db
end

module TimeTrial = struct
    let go () =
        let db = Sqlexpr.open_db ":memory:" in
        Database.init_db db;
        let data = process process_rw timetrial in
        gogogo db data;
        let points = total_points db no_bonuses best_points_std dq_points_below_cat in
        final_results db points, team_points db false, db
end

module Ages = struct
    let filtered () =
        ages |> Array.to_list
        |> List.map (fun id ->
            Utils.flatten_placings (Utils.fetch_placings (Printf.sprintf "https://www.zwiftpower.com/api3.php?do=event_results&zid=%d&type=filtered" id)))
        |> List.concat
        |> List.filter (fun r -> r.p_category = Category.Flagged "AGE")
        |> List.filter (fun r -> r.p_age = 52 || r.p_age = 47 || r.p_age = 39)
    ;;

    let fix_incorrectly_flagged_age () =
        Array.iter reset_places ages;
        let fixes = filtered ()
            |> List.map (fun placing -> {
                zwift_id = int_of_string placing.p_zwid; original_place = placing; place = placing; entry_cat = placing.p_category;
                result_cat = match placing.p_age with
                    | 52 -> Category.D
                    | 47 -> Category.C
                    | 39 -> Category.B
            })
        in
        ages
        |> Array.map (fun id ->
            id, List.filter (fun fix -> fix.place.p_zid = string_of_int id) fixes)
        |> Array.iter (fun (id, list) ->
            if list <> [] then begin
                let zwifters = Utils.fetch_zwifters (Printf.sprintf "https://www.zwiftpower.com/api3.php?do=event_results_zwift&zid=%d" id) in
                edit_places id list zwifters.zwifters
            end)
    ;;

    let go () =
        fix_incorrectly_flagged_age ();
        let db = Sqlexpr.open_db ":memory:" in
        Database.init_db db;
        let data = process process_ro ages in
        gogogo db data;
        let points = total_points db no_bonuses best_points_std dq_points_below_cat in
        final_results db points, team_points db false, db
end

let results_for id db =
    Sqlexpr.select db
        [%sqlc "SELECT @d{individual_id}, @d{race_id}, @d?{team_id}, @d{position}, @s{category} FROM race_result WHERE individual_id = %d"] id;;

let wild_podiums = [129382; 132853; 135191; 129383; 132859; 136832];;

let podiums_for event_ids =
    let module M = Results(Women) in
    let oc = open_out "wild_podiums.csv" in
    List.map (fun event_id -> fetch_event event_id |> M.sort_into_cats) event_ids
        |> podiums |> write_podiums_csv oc;
    close_out oc
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
