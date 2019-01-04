#!/usr/bin/env ocaml

#load "unix.cma"

let max_ftp =
	let ic = Unix.open_process_in
		"curl 'https://www.zwiftpower.com/api3.php?do=profile_results&z=653395&type=all' | jq '[.data[] | {wkg_ftp: .wkg_ftp[0] | tonumber, date: .event_date}] | sort_by(.date) | reverse | .[0:30] | [.[].wkg_ftp] | max'"
	in
	let line = String.trim(input_line ic) in
	Printf.printf "line: %S\n" line;
	let result = Scanf.sscanf line " %f" (fun x -> x) in
	close_in ic;
	result

let () =
	if max_ftp >= 4.0 then
		print_endline "Category A"
	else if max_ftp >= 3.2 then
		print_endline "Category B"
	else if max_ftp >= 2.5 then
		print_endline "Category C"
	else
		print_endline "Category D"

