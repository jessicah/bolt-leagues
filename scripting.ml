
module AllCats = struct
    let get_ids_for_round db (event1, event2) =
        Sqlexpr.select db
            [%sqlc "SELECT DISTINCT @d{individual_id} FROM race_result WHERE race_id = %d OR race_id = %d"] event1 event2
    ;;

    module IntSet = Set.Make(struct type t = int let compare = compare end);;

    let ids = Array.map (get_ids_for_round db) wild |> Array.map (IntSet.of_list);;

    let data =
        Array.mapi (fun ix id_set ->
            let newcomers = ref id_set in
            let returning = ref IntSet.empty in
            for iy = 1 to ix do
                returning := IntSet.union !returning (IntSet.inter id_set ids.(iy-1));
                newcomers := IntSet.diff !newcomers ids.(iy-1);
            done;
            IntSet.elements !newcomers, IntSet.elements !returning) ids;
    ;;

    let new_and_returning = Array.map (fun (newcomers, returning) -> List.length newcomers, List.length returning) data;;
end

module ByCat = struct
    let get_ids_for_round db category (event1, event2) =
        Sqlexpr.select db
            [%sqlc "SELECT DISTINCT @d{individual_id} FROM race_result WHERE (race_id = %d OR race_id = %d) AND category = %s"] event1 event2 category
    ;;

    module IntSet = Set.Make(struct type t = int let compare = compare end);;

    let ids category = Array.map (get_ids_for_round db category) wild |> Array.map (IntSet.of_list);;

    let data category =
        Array.mapi (fun ix id_set ->
            let newcomers = ref id_set in
            let returning = ref IntSet.empty in
            for iy = 1 to ix do
                returning := IntSet.union !returning (IntSet.inter id_set (ids category).(iy-1));
                newcomers := IntSet.diff !newcomers (ids category).(iy-1);
            done;
            IntSet.elements !newcomers, IntSet.elements !returning) (ids category);
    ;;

    let new_and_returning category = Array.map (fun (newcomers, returning) -> List.length newcomers, List.length returning) (data category);;
end

module Winners = struct
    let count_by_weeks db n =
        Sqlexpr.select db
            [%sql "SELECT @d{COUNT(individual_id)} FROM
                (SELECT COUNT(race_id) races, individual_id, category FROM race_result GROUP BY individual_id, category HAVING races >= %d) r
                GROUP BY category ORDER BY category"] n

    let count db =
        Sqlexpr.select db
            [%sql "SELECT @d{COUNT(individual_id)} FROM
                (SELECT COUNT(race_id) races, individual_id, category FROM race_result GROUP BY individual_id, category HAVING races >= 4) r
                GROUP BY category ORDER BY category"]
    
    let winner_ids db =
        count db |> List.map Random.int |> List.map succ |> Array.of_list

    let go db =
        let ids = winner_ids db in
        Sqlexpr.select db
            [%sql "SELECT @s{i.individual_name}, @s{rows.category} FROM
                (SELECT ROW_NUMBER () OVER (PARTITION BY r.category ORDER BY r.individual_id) row_num, r.individual_id, r.category FROM
                    (SELECT COUNT(race_id) races, individual_id, category FROM race_result GROUP BY individual_id, category HAVING races >= 4) r) rows
                INNER JOIN individual i ON rows.individual_id = i.individual_id
                WHERE
                    (rows.row_num = %d AND rows.category = 'A')
                OR
                    (rows.row_num = %d AND rows.category = 'B')
                OR
                    (rows.row_num = %d AND rows.category = 'C')
                OR
                    (rows.row_num = %d AND rows.category = 'D')"]
            ids.(0) ids.(1) ids.(2) ids.(3)
end
