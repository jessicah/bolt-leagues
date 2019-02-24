#use "topfind";;
#thread;;
#require "ppx_sqlexpr";;
#require "sqlexpr";;

type team = {
	tid: int;
	tname: string;
	tc: string;
	tbc: string;
	tbd: string;
}

module Sqlexpr = Sqlexpr_sqlite.Make(Sqlexpr_concurrency.Id);;

let init_base_db db =
	Sqlexpr.execute db
		[%sqlinit "CREATE TABLE IF NOT EXISTS team(
			tid INTEGER PRIMARY KEY,
			name TEXT NOT NULL,
			tc CHAR(6) NOT NULL,
			tbc CHAR(6) NOT NULL,
			tbd CHAR(6) NOT NULL);"];
	Sqlexpr.execute db
		[%sqlinit "CREATE TABLE IF NOT EXISTS individual(
			zwid INTEGER PRIMARY KEY,
			name TEXT NOT NULL,
			flag VARCHAR(6),
			category CHAR(1) NOT NULL);"];
	Sqlexpr.execute db
		[%sqlinit "CREATE TABLE IF NOT EXISTS ipoints(
			zwid INTEGER PRIMARY KEY,
			tid INTEGER,
			events INTEGER NOT NULL,
			points INTEGER NOT NULL,
			podiums TEXT NOT NULL,
			FOREIGN KEY(zwid) REFERENCES individual(zwid),
			FOREIGN KEY(tid) REFERENCES team(tid));"];
	Sqlexpr.execute db
		[%sqlinit "CREATE TABLE IF NOT EXISTS tpoints(
			tid INTEGER NOT NULL,
			category VARCHAR(1) NOT NULL,
			events INTEGER NOT NULL,
			points INTEGER NOT NULL,
			FOREIGN KEY(tid) REFERENCES team(tid));"];
	Sqlexpr.execute db
		[%sqlinit "CREATE UNIQUE INDEX tpoints_ix ON tpoints(tid, category);"]

module Individual = struct
	let insert db zwid category events points name ?flag ?team ?podiums () =
		let podiums = match podiums with None -> "" | Some value -> value in
		let tid, tname, tc, tbc, tbd = match team with
			| None -> None, None, None, None, None
			| Some team -> Some team.tid, Some team.tname, Some team.tc, Some team.tbc, Some team.tbd
		in
		begin match team with
		| None -> ()
		| Some t ->
			Sqlexpr.insert db
				[%sqlc "INSERT INTO team(tid, name, tc, tbc, tbd)
						VALUES(%d, %s, %s, %s, %s)
						ON CONFLICT(tid) DO UPDATE SET
							name=excluded.name,
							tc=excluded.tc,
							tbc=excluded.tbc,
							tbd=excluded.tbd;"]
				t.tid t.tname t.tc t.tbc t.tbd |> ignore;
		end;
		Sqlexpr.insert db
			[%sqlc "INSERT INTO individual(zwid, tid, name, flag, category)
					VALUES(%d, %d?, %s, %s?, %s)
					ON CONFLICT(zwid) DO UPDATE SET
						name=excluded.name,
						flag=ifnull(excluded.flag,individual.flag),
						category=max(individual.category,excluded.category);"]
			zwid tid name flag category |> ignore;
		Sqlexpr.insert db
			[%sqlc "INSERT INTO ipoints(zwid, tid, events, points, podiums)
				VALUES(%d, %d, %d, %d, %s)
				ON CONFLICT(zwid) DO UPDATE SET
				  tid=ifnull(excluded.tid, ipoints.tid),
					events=ipoints.events+excluded.events,
					points=ipoints.points+excluded.points,
					podiums=trim(ipoints.podiums + ' ' + excluded.podiums);"]
			zwid events tid points podiums |> ignore
	
	let update_podiums db zwid podiums =
		Sqlexpr.execute db
			[%sqlc "UPDATE ipoints SET podiums=trim("]
end

module Team = struct
	let insert db team category events points =
		Sqlexpr.insert db
			[%sqlc "INSERT INTO team(tid, name, tc, tbc, tbd)
					VALUES(%d, %s, %s, %s, %s)
					ON CONFLICT(tid) DO UPDATE SET
						name=excluded.name,
						tc=excluded.tc,
						tbc=excluded.tbc,
						tbd=excluded.tbd;"]
			team.tid team.tname team.tc team.tbc team.tbd |> ignore;
		Sqlexpr.insert db
			[%sqlc "INSERT INTO tpoints(tid, category, events, points)
				VALUES(%d, %s, %d, %d)
				ON CONFLICT(tid, category) DO UPDATE SET
					events=tpoints.events+excluded.events,
					points=tpoints.points+excluded.points;"]
			team.tid category events points
end

let init_db db =
	init_base_db db

let load_team_csv filename db =
	let rows = Csv.load filename in

    List.iter (function
		| category :: events :: points :: tid :: tname :: tc :: tbc :: tbd :: [] ->
			let tid = int_of_string tid in
			let events = int_of_string events in
			let points = int_of_string points in
			let team = { tid; tname; tc; tbc; tbd } in
			Team.insert db team category events points |> ignore
		| _ -> failwith "invalid CSV")
		(List.tl rows)

let load_indv_csv_no_podiums filename db =
	let rows = Csv.load filename in

	List.iter (function
		| category :: events :: points :: tid :: _ :: _ :: _ :: name :: flag :: zwid ::  [] ->
			let tid = int_of_string tid
			and events = int_of_string events
			and points = int_of_string points
			and zwid = int_of_string zwid in
			let team = Sqlexpr.select_one_f_maybe db
				(fun (tid,tname,tc,tbc,tbd) -> { tid; tname; tc; tbc; tbd })
				[%sqlc "SELECT @d{tid}, @s{name}, @s{tc}, @s{tbc}, @s{tbd} FROM team WHERE tid = %d"] tid
			in
			match team with
			| None -> Individual.insert db zwid category events points name ~flag () |> ignore
			| Some team ->
				Individual.insert db zwid category events points name ~flag ~team () |> ignore
		| _ -> failwith "invalid CSV")
		(List.tl rows)

let load_indv_csv filename db =
	let rows = Csv.load filename in

	List.iter (function
		| category :: events :: points :: tid :: _ :: _ :: _ :: name :: flag :: zwid :: podiums :: [] ->
			let tid = int_of_string tid
			and events = int_of_string events
			and points = int_of_string points
			and zwid = int_of_string zwid in
			let team = Sqlexpr.select_one_f_maybe db
				(fun (tid,tname,tc,tbc,tbd) -> { tid; tname; tc; tbc; tbd })
				[%sqlc "SELECT @d{tid}, @s{name}, @s{tc}, @s{tbc}, @s{tbd} FROM team WHERE tid = %d"] tid
			in
			match team with
			| None -> Individual.insert db zwid category events points name ~flag ~podiums () |> ignore
			| Some team ->
				Individual.insert db zwid category events points name ~flag ~team ~podiums () |> ignore
		| _ -> failwith "invalid CSV")
		(List.tl rows)

let load_indv_csv_no_tid filename db =
	let rows = Csv.load filename in

	Printf.printf "have %d rows to process...\n" (List.length rows);

	List.iter (function
		| category :: events :: points :: tname :: _ :: _ :: _ :: name :: flag :: zwid :: podiums :: [] ->
			let events = int_of_string events
			and points = int_of_string points
			and zwid = int_of_string zwid in
			let team = Sqlexpr.select_one_f_maybe db
				(fun (tid,tname,tc,tbc,tbd) -> { tid; tname; tc; tbc; tbd })
				[%sqlc "SELECT @d{tid}, @s{name}, @s{tc}, @s{tbc}, @s{tbd} FROM team WHERE name LIKE %s"] tname
			in
			match team with
			| None -> Individual.insert db zwid category events points name ~flag ~podiums () |> ignore
			| Some team ->
				Individual.insert db zwid category events points name ~flag ~team ~podiums () |> ignore
		| _ -> failwith "invalid CSV")
		(List.tl rows)

let load_podiums filename db =
	let rows = Csv.load filename in

	List.iter (function
		| zwid :: podiums :: [] ->
			let zwid = int_of_string zwid in
			Individual.insert db zwid category events points name ~flag ~podiums () |> ignore
		| _ -> failwith "invalid CSV")
		(List.tl rows)

let indv_to_csv db filename =
	let default d = function None -> d | Some x -> x in
	let maybe f d = function None -> d | Some x -> f x in
	(* category, events, points, tname, tid, tc, tbc, tbd, name, flag, zwid, podiums *)
	let rows = Sqlexpr.select db
		[%sqlc "SELECT @s{iv.category}, @d{i.events}, @d{i.points}, @s?{t.name}, @d?{t.tid}, @s?{t.tc}, @s?{t.tbc}, @s?{t.tbd}, @s{iv.name}, @s?{iv.flag}, @d{i.zwid}, @s{i.podiums}
				FROM ipoints i INNER JOIN individual iv ON i.zwid = iv.zwid LEFT JOIN team t on iv.tid = t.tid ORDER BY iv.category, i.points DESC"]
	in
	let t = List.map (fun (cat, events, points, tname, tid, tc, tbc, tbd, name, flag, zwid, podiums) ->
		[
			cat;
			string_of_int events;
			string_of_int points;
			default "" tname;
			maybe string_of_int "" tid;
			default "" tc;
			default "" tbc;
			default "" tbd;
			name;
			default "" flag;
			string_of_int zwid;
			podiums;
		]) rows
	in
	let t = ["category";"events";"points";"tname";"tid";"tc";"tbc";"tbd";"name";"flag";"zwid";"podiums"] :: t in
	Csv.save filename t

let teams_to_csv db filename =
	(* category, events, points, tid, tname, tc, tbc, tbd *)
	let rows = Sqlexpr.select db
		[%sqlc "SELECT @s{tp.category}, @d{tp.events}, @d{tp.points}, @d{tp.tid}, @s{t.name}, @s{t.tc}, @s{t.tbc}, @s{t.tbc}
				FROM tpoints tp INNER JOIN team t ON tp.tid = t.tid ORDER BY tp.category, tp.points DESC"]
	in
	let t = List.map (fun (cat, events, points, tid, tname, tc, tbc, tbd) ->
		[
			cat;
			string_of_int events;
			string_of_int points;
			string_of_int tid;
			tname;
			tc;
			tbc;
			tbd;
		]) rows
	in
	let t = ["category";"events";"points";"tid";"tname";"tc";"tbc";"tbd"] :: t in
	Csv.save filename t
