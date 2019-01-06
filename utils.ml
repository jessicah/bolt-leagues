(* Separate utility functions out, so the main code is easier to reason with *)

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
    rc, (Buffer.contents buf)
;;

let get url oc =
    let c = Curl.init () in
    Curl.set_writefunction c (fun s -> output_string oc s; String.length s);
    Curl.set_url c url;
    Curl.set_cookiejar c "zwiftpower.cookies";
    Curl.set_cookiefile c "zwiftpower.cookies";
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

let submit_results event_id data =
    post "https://www.zwiftpower.com/ucp.php?mode=login"
        (Printf.sprintf "username=jessica.l.hamilton@gmail.com&password=%s&autologin=1&redirect=./index.php?&login="
            (Unix.getenv "ZP_PASSWORD"));
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

let fetch_placings url =
    unlink "jq.fifo";
    unlink "atd.fifo";
    Unix.mkfifo "jq.fifo" 0o644;
    Unix.mkfifo "atd.fifo" 0o644;
    let out_fd = Unix.openfile "atd.fifo" [Unix.O_RDWR; Unix.O_NONBLOCK] 0 in
    Unix.create_process "jq" [|"--unbuffered"; "-f"; "postprocess.jq"; "jq.fifo"|] Unix.stdin out_fd Unix.stdout;
    get url (open_out "jq.fifo");
    Unix.close out_fd;
    let ic = open_in "atd.fifo" in
    let results = Results_j.placings_of_string (read_all ic) in
    close_in ic;
    unlink "atd.fifo";
    unlink "jq.fifo";
    results
;;

let contains_substring pat str =
    let rec search i j =
        if j >= String.length pat then true
        else if i + j >= String.length str then false
        else if str.[i + j] = pat.[j] then search i (j+1)
        else search (i+1) 0
    in search 0 0
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
