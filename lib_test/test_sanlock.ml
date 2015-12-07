open OUnit
open Sanlock

(* Helper functions *)

(*BISECT-IGNORE-BEGIN*)
let finally f g =
  let res = try f () with e -> g (); raise e in g (); res

let with_open_file path f =
  let fd = Unix.(openfile path [O_CREAT; O_TRUNC; O_RDWR] 0o600) in
  finally (fun () -> f fd) (fun () -> Unix.close fd)

let with_temp_file f =
  let path = Filename.temp_file "test_sanlock" ".lock" in
  finally (fun () -> with_open_file path (f path)) (fun () -> Unix.unlink path)

let is_valid_fd fd =
  Unix.(try fstat fd |> ignore; true with Unix_error (EBADF, _, _) -> false)

let sleep s = Unix.select [] [] [] s |> ignore
(*BISECT-IGNORE-END*)

(* Test cases *)

let check_sanlock_daemon () =
  let pid_file = "/var/run/sanlock/sanlock.pid" in
  if not (Sys.file_exists pid_file) then begin
    print_endline "error: sanlock daemon isn't running.";
    exit 1;
  end;
  let pid_ic = open_in pid_file in
  let pid = input_line pid_ic in
  close_in pid_ic;
  if not (Sys.file_exists ("/proc/" ^ pid)) then begin
    print_endline "error: sanlock daemon isn't running.";
    exit 1;
  end;
  let sock_path = "/var/run/sanlock/sanlock.sock" in
  try Unix.(access sock_path [ W_OK ])
  with _ ->
    print_endline "error: No access to sanlock socket. Restart daemon with -U $USER";
    exit 2

let test_register =
  "Test we can register() with the sanlock daemon" >:: fun () ->
  let handle = register () in
  if not (is_valid_fd (Obj.magic handle))
  then assert_string "register returned bad fd"

let test_get_alignment =
  "Test that get_alignment() gives sensible alignment values" >:: fun () ->
  with_temp_file (fun path _ ->
    let mib = Int64.mul 1024L 1024L in
    assert_equal ~msg:"Expected 1MB alignment" mib (get_alignment path)
  )

let test_init_lockspace =
  "Test we can initialise a lockspace" >:: fun () ->
  with_temp_file (fun path _ ->
    init_lockspace "lockspace1" path |> ignore
  )

let test_add_rem_lockspace =
  "Test we can join and leave a lockspace" >:: fun () ->
  with_temp_file (fun path _ ->
    let ls = init_lockspace "lockspace1" path in
    let host_id = 1 in
    let m = add_lockspace ls host_id in
    rem_lockspace m
  )

let test_init_resource =
  "Test we can initialise a resource" >:: fun () ->
  with_temp_file (fun path _ ->
    let ls = init_lockspace "lockspace1" path in
    let offset = get_alignment path in
    init_resource ls [(path, offset)] "resource1" |> ignore
  )

let test_acquire_release =
  "Test acquiring a resource blocks other attempts to acquire it" >:: fun () ->
  with_temp_file (fun path _ ->
    let ls = init_lockspace "lockspace1" path in
    let offset = get_alignment path in
    let res1 = init_resource ls [(path, Int64.mul 1L offset)] "resource1" in
    let res2 = init_resource ls [(path, Int64.mul 2L offset)] "resource2" in
    let m = add_lockspace ls 1 in
    let h = register () in
    acquire h res1;
    acquire h res2;
    assert_raises (Sanlk_error(Unix.(error_message EEXIST))) (fun () ->
      acquire h res1
    );
    release h res1;
    acquire h res1;
    release h res1;
    release h res2;
    rem_lockspace m;
  )

let test_signal_on_rem_lockspace =
  "Test we get a signal on rem_lockspace if we're holding a lock" >:: fun () ->
  with_temp_file (fun path _ ->
    let ls = init_lockspace "lockspace1" path in
    let offset = get_alignment path in
    let res = init_resource ls [(path, Int64.mul 1L offset)] "resource1" in
    let m = add_lockspace ls 1 in
    let h = register () in
    acquire h res;
    restrict h `Sigkill;
    try
      Sys.(set_signal sigterm (Signal_handle (fun _ -> failwith "ok")));
      rem_lockspace m;
    with Failure("ok") -> ()
  )

let _ =
  check_sanlock_daemon ();
  let suite = "sanlock" >::: [
    test_register;
    test_get_alignment;
    test_init_lockspace;
    test_init_resource;
    test_add_rem_lockspace;
    test_acquire_release;
    test_signal_on_rem_lockspace;
  ] in
  OUnit2.run_test_tt_main @@ ounit2_of_ounit1 suite
