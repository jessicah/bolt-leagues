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

type individual = {
	iid: int;
	iname: string;
	iflag: string option;
}

type one_result = {
	rid: int;
	reid: int;
	rtid: int option;
	rposition: int;
	rcategory: string;
}

module Sqlexpr = Sqlexpr_sqlite.Make(Sqlexpr_concurrency.Id);;

let init_base_db db =
	Sqlexpr.execute db
		[%sqlinit "CREATE TABLE IF NOT EXISTS team(
			team_id INTEGER PRIMARY KEY,
			team_name TEXT NOT NULL,
			team_colour CHAR(6) NOT NULL,
			team_background_colour CHAR(6) NOT NULL,
			team_border_colour CHAR(6) NOT NULL);"];
	Sqlexpr.execute db
		[%sqlinit "CREATE TABLE IF NOT EXISTS individual(
			individual_id INTEGER PRIMARY KEY,
			individual_name TEXT NOT NULL,
			individual_flag VARCHAR(6));"];
	Sqlexpr.execute db
		[%sqlinit "CREATE TABLE IF NOT EXISTS race_result(
			individual_id INTEGER NOT NULL,
			race_id INTEGER NOT NULL,
			team_id INTEGER,
			position INTEGER NOT NULL,
			category CHAR(1) NOT NULL,
			FOREIGN KEY(individual_id) REFERENCES individual(individual_id),
			FOREIGN KEY(team_id) REFERENCES team(team_id));"];
	(*Sqlexpr.execute db
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
			FOREIGN KEY(tid) REFERENCES team(tid));"];*)	
	(*Sqlexpr.execute db
		[%sqlinit "CREATE UNIQUE INDEX tpoints_ix ON tpoints(tid, category);"]*)

module Team = struct
	let insert db team =
		Sqlexpr.insert db
			[%sqlc "INSERT INTO team(team_id, team_name, team_colour, team_background_colour, team_border_colour)
					VALUES(%d, %s, %s, %s, %s)
					ON CONFLICT(team_id) DO UPDATE SET
						team_name = excluded.team_name,
						team_colour = excluded.team_colour,
						team_background_colour = excluded.team_background_colour,
						team_border_colour = excluded.team_border_colour;"]
			team.tid team.tname team.tc team.tbc team.tbd |> ignore
	
	let get db id =
		Sqlexpr.select_one_f_maybe db
				(fun (tid,tname,tc,tbc,tbd) -> { tid; tname; tc; tbc; tbd })
				[%sqlc "SELECT @d{team_id}, @s{team_name}, @s{team_colour}, @s{team_background_colour}, @s{team_border_colour} FROM team WHERE team_id = %d"] id

	(*let add_points =
		Sqlexpr.insert db
			[%sqlc "INSERT INTO tpoints(tid, category, events, points)
				VALUES(%d, %s, %d, %d)
				ON CONFLICT(tid, category) DO UPDATE SET
					events=tpoints.events+excluded.events,
					points=tpoints.points+excluded.points;"]
			team.tid category events points*)
end

module Individual = struct
	let insert db zwid name flag =
		Sqlexpr.insert db
			[%sqlc "INSERT INTO individual(individual_id, individual_name, individual_flag)
					VALUES(%d, %s, %s?)
					ON CONFLICT(individual_id) DO UPDATE SET
						individual_name = excluded.individual_name,
						individual_flag = IFNULL(excluded.individual_flag, individual.individual_flag);"]
			zwid name flag |> ignore

	let add_result db zwid race category position name flag team =
		let tid = match team with
			| None -> None
			| Some team ->
				Team.insert db team;
				Some team.tid
		in
		insert db zwid name flag;
		Sqlexpr.insert db
			[%sqlc "INSERT INTO race_result(individual_id, race_id, team_id, position, category)
				VALUES(%d, %d, %d?, %d, %s);"]
			zwid race tid position category |> ignore
	
	let get db id =
		Sqlexpr.select_one_f_maybe db
			(fun (iid, iname, iflag) -> { iid; iname; iflag })
			[%sqlc "SELECT @d{individual_id}, @s{individual_name}, @s?{individual_flag} FROM individual WHERE individual_id = %d"] id
	
	let result db id event =
		Sqlexpr.select_one_f_maybe db
			(fun (rid, reid, rtid, rposition, rcategory) -> { rid; reid; rtid; rposition; rcategory })
			[%sqlc "SELECT @d{individual_id}, @d{race_id}, @d?{team_id}, @d{position}, @s{category} FROM race_result WHERE individual_id = %d AND race_id = %d"]
			id event
end

module Database = struct

	let init_db db =
		init_base_db db

	let load_team_csv filename db =
		let rows = Csv.load filename in

			List.iter (function
			| _ :: _ :: _ :: tid :: tname :: tc :: tbc :: tbd :: [] ->
				let tid = int_of_string tid in
				let team = { tid; tname; tc; tbc; tbd } in
				Team.insert db team |> ignore
			| _ -> failwith "invalid CSV")
			(List.tl rows)

	let load_indv_csv filename db =
		let rows = Csv.load filename in

		List.iter (function
			| _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: name :: flag :: zwid :: [] ->
				let zwid = int_of_string zwid in
				let flag = if String.length flag = 0 then None else Some flag in
				Individual.insert db zwid name flag |> ignore
			| _ -> failwith "invalid CSV")
			(List.tl rows)

	(*let load_podiums filename db =
		let rows = Csv.load filename in

		List.iter (function
			| zwid :: podiums :: [] ->
				let zwid = int_of_string zwid in
				Individual.insert db zwid category events points name ~flag ~podiums () |> ignore
			| _ -> failwith "invalid CSV")
			(List.tl rows)*)

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

	let teams_to_csv db =
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
		["category";"events";"points";"tid";"tname";"tc";"tbc";"tbd"] :: t

	let dump_teams db =
		let rows = Sqlexpr.select db
			[%sqlc "SELECT @s{team_id}, @s{team_name}, @s{team_colour}, @s{team_background_colour}, @s{team_border_colour}
				FROM team ORDER BY team_name"]
		|> List.map (fun (id, name, tc, tbc, tbd) -> [id; name; tc; tbc; tbd]) in
		["team_id"; "name"; "tc"; "tbc"; "tbd"] :: rows

	let dump_individuals db =
		let rows = Sqlexpr.select db
			[%sqlc "SELECT @s{individual_id}, @s{individual_name}, @s?{individual_flag}
				FROM individual ORDER BY individual_name"]
		|> List.map (fun (id, name, flag) -> [id; name; match flag with None -> "" | Some flag -> flag]) in
		["individual_id"; "name"; "flag"] :: rows

	let individuals = [
		"round1/current_ind.csv";
		"round2/current_ind.csv";
		"round3/current_ind.csv";
		"round4/current_ind.csv";
		"round5/current_ind.csv";
		"round6/current_ind.csv";
	]

	let teams = [
		"round1/current_team.csv";
		"round2/current_team.csv";
		"round3/current_team.csv";
		"round4/current_team.csv";
		"round5/current_team.csv";
		"round6/current_team.csv";
	]

	let podiums = [
		"round1/current_podiums.csv";
		"round2/current_podiums.csv";
		"round3/current_podiums.csv";
		"round4/current_podiums.csv";
		"round5/current_podiums.csv";
		"round6/current_podiums.csv";
	]

	let load_teams db = List.iter (fun path -> load_team_csv path db) teams
	let load_individuals db = List.iter (fun path -> load_indv_csv path db) individuals

end
