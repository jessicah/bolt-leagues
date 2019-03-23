(* Separate utility functions out, so the main code is easier to reason with *)

let verbose = ref false

let contains_substring pat str =
    let rec search i j =
        if j >= String.length pat then true
        else if i + j >= String.length str then false
        else if str.[i + j] = pat.[j] then search i (j+1)
        else search (i+1) 0
    in search 0 0
;;

let post url data =
    if !verbose then Printf.printf "Posting to %s...\n%!" url;
    let buf = Buffer.create 80 in
    let c = Curl.init () in
    Curl.set_verbose c false;
    Curl.set_writefunction c (fun s -> Buffer.add_string buf s; String.length s);
    Curl.set_url c url;
    Curl.set_cookiejar c "temp/zwiftpower.cookies";
    Curl.set_cookiefile c "temp/zwiftpower.cookies";
    Curl.set_httpheader c ["Content-Type: application/x-www-form-urlencoded"];
    Curl.set_postfields c data;
    Curl.set_postfieldsize c (String.length data);
    Curl.perform c;
    let rc = Curl.get_responsecode c in
    Curl.cleanup c;
    rc, (Buffer.contents buf)
;;

let get url oc =
    if !verbose then Printf.printf "Fetching %s...\n%!" url;
    let c = Curl.init () in
    Curl.set_writefunction c (fun s -> output_string oc s; String.length s);
    Curl.set_url c url;
    Curl.set_cookiejar c "temp/zwiftpower.cookies";
    Curl.set_cookiefile c "temp/zwiftpower.cookies";
    Curl.perform c;
    Curl.cleanup c;
    close_out oc
;;

(*let edit_riders event_id riders =
    let format rider =
        Printf.sprintf "%s, %s, %d, %s, %d, %s, %d, %s"
            (Category.unwrap rider.e_category) rider.e_flag rider.e_power_type rider.e_name
            rider.e_zwid rider.e_uid rider.e_penalty rider.e_notes;
    in
    let data = String.concat "\n" (List.map format riders) in
    post
        "https://www.zwiftpower.com/ucp.php?mode=login"
        (Printf.sprintf "username=jessica.l.hamilton@gmail.com&password=%s&autologin=1&redirect=./index.php?&login="
            (Unix.getenv "ZP_PASSWORD"));
    post
        (Printf.sprintf "https://www.zwiftpower.com/zz.php?do=edit_results&act=save&zwift_event_id=%d" event_id)
        ("edit_results=" ^ data);;*)

let login_zp () =
    let password = try
            Unix.getenv "ZP_PASSWORD"
        with Not_found ->
            print_string "enter password: ";
            let tio = Unix.tcgetattr Unix.stdout in
            Unix.tcsetattr Unix.stdout Unix.TCSANOW { tio with Unix.c_echo = false };
            let pw = read_line() in
            Unix.tcsetattr Unix.stdout Unix.TCSANOW tio;
            print_newline ();
            Unix.putenv "ZP_PASSWORD" pw;
            pw
    in
    post "https://www.zwiftpower.com/ucp.php?mode=login"
        (Printf.sprintf "username=jessica.l.hamilton@gmail.com&password=%s&autologin=1&redirect=./index.php?&login="
            password) |> ignore
;;

let submit_results event_id data =
    login_zp ();
    post
        (Printf.sprintf "https://www.zwiftpower.com/zz.php?do=edit_results&act=save&zwift_event_id=%d" event_id)
        ("edit_results=" ^ data)
;;

let read_file name =
	let ic = open_in name in
	let length = in_channel_length ic in
	really_input_string ic length
;;

let read_all ic =
    let buffer = Buffer.create 1024 in
    let rec loop () =
        let bytes = Bytes.create 1024 in
        let num = input ic bytes 0 1024 in
        if num > 0 then begin
            Buffer.add_bytes buffer (Bytes.sub bytes 0 num);
            loop ()
        end
    in loop ();
    Buffer.contents buffer
;;

let unlink file =
    try
        Unix.unlink file
    with
    | Unix.Unix_error(Unix.ENOENT, "unlink", _) -> ()
    | e -> print_endline (Printexc.to_string e)
;;

let rec fetch_placings url n =
    if n = 0 then failwith "retry exceeded";
    begin try
        unlink "temp/jq.fifo";
        unlink "temp/atd.fifo";
        Unix.mkfifo "temp/jq.fifo" 0o644;
        Unix.mkfifo "temp/atd.fifo" 0o644;
        let out_fd = Unix.openfile "temp/atd.fifo" [Unix.O_RDWR; Unix.O_NONBLOCK] 0 in
        Unix.create_process "jq" [|"--unbuffered"; "-f"; "postprocess.jq"; "temp/jq.fifo"|] Unix.stdin out_fd Unix.stdout |> ignore;
        get url (open_out "temp/jq.fifo");
        Unix.close out_fd;
        let ic = open_in "temp/atd.fifo" in
        let results = Results_j.placings_of_string (read_all ic) in
        close_in ic;
        unlink "temp/atd.fifo";
        unlink "temp/jq.fifo";
        results
    with exn ->
        print_endline (Printexc.to_string exn);
        fetch_placings url (n-1)
    end
;;

let fetch_placings url = fetch_placings url 10
;;

let event_results_zwift_cache = Hashtbl.create 100;;

let rec fetch_zwifters url n =
    if (contains_substring "=event_results_zwift" url) && (Hashtbl.mem event_results_zwift_cache url) then begin
        if !verbose then Printf.printf "\x1B[92mFetched %s (from cache)...\n\x1B[39m%!" url;
        Hashtbl.find event_results_zwift_cache url
    end else begin
        if n = 0 then failwith "retry exceeded";
        begin try
            unlink "temp/jq.fifo";
            unlink "tempatd.fifo";
            Unix.mkfifo "temp/jq.fifo" 0o644;
            Unix.mkfifo "temp/atd.fifo" 0o644;
            let out_fd = Unix.openfile "temp/atd.fifo" [Unix.O_RDWR; Unix.O_NONBLOCK] 0 in
            Unix.create_process "jq" [|"--unbuffered"; "-f"; "postprocess_zwift.jq"; "temp/jq.fifo"|] Unix.stdin out_fd Unix.stdout |> ignore;
            get url (open_out "temp/jq.fifo");
            Unix.close out_fd;
            let ic = open_in "temp/atd.fifo" in
            let results = Results_j.zwifters_of_string (read_all ic) in
            close_in ic;
            unlink "temp/atd.fifo";
            unlink "temp/jq.fifo";
            Hashtbl.add event_results_zwift_cache url results;
            results
        with exn ->
            fetch_zwifters url (n-1)
        end
    end
;;

let fetch_zwifters url = fetch_zwifters url 10
;;

let flatten_placings placings =
    List.concat (Array.to_list placings.Results_j.placings)
;;

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
    in loop ([], [])
;;

let replace_escapes s =
    Netencoding.Html.decode `Enc_utf8 `Enc_utf8 () s
    |> Netencoding.Url.encode ~plus:false
;;

let longest_common_substring left right =
	let left_len = String.length left in
	let right_len = String.length right in
	let num = Array.make_matrix left_len right_len 0 in
	let maxlen = ref 0 in
	let last_subs_begin = ref 0 in
	let buffer = Buffer.create (max left_len right_len) in

	for i = 0 to left_len - 1 do
		for j = 0 to right_len - 1 do
			if left.[i] <> right.[j] then
				num.(i).(j) <- 0
			else begin
				if i = 0 || j = 0 then
					num.(i).(j) <- 1
				else
					num.(i).(j) <- 1 + num.(i-1).(j-1);

				if num.(i).(j) > !maxlen then begin
					maxlen := num.(i).(j);
					let this_subs_begin = i - num.(i).(j) + 1 in
					if !last_subs_begin = this_subs_begin then
						Buffer.add_char buffer left.[i]
					else begin
						last_subs_begin := this_subs_begin;
						Buffer.reset buffer;
						Buffer.add_substring buffer
							left !last_subs_begin (i + 1 - !last_subs_begin);
					end
				end
			end
		done;
	done;
	Buffer.contents buffer
;;
