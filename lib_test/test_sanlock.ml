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
    print_endline "error: No access to sanlock socket. Restart with -U <user>";
    exit 2

let with_sanlock_disk f =
  with_temp_file (fun path _ ->
    let disk = {
      Disk.path;
      offset = UInt64.of_int 0;
      pad1 = UInt32.of_int 0;
      pad2 = UInt32.of_int 0;
    } in
    f disk
  )

let test_register =
  "Test we can register() with the sanlock daemon" >:: fun () ->
  let fd = register () in
  if not (is_valid_fd fd) then assert_string "register returned bad fd";
  let fd' = register () in
  if not (is_valid_fd fd') then assert_string "second register returned bad fd"

let test_align =
  "Test that align() gives sensible alignment values" >:: fun () ->
  with_sanlock_disk (fun disk ->
    assert_equal ~msg:"Expected 1MB alignment" (1024*1024) (align disk)
  )

let test_init_lockspace =
  "Test we can initialise a lockspace" >:: fun () ->
  with_sanlock_disk (fun disk ->
    let offset = align disk |> UInt64.of_int in
    init_lockspace "lockspace1" { disk with Disk.offset }
  )

let _ =
  check_sanlock_daemon ();
  let suite = "sanlock" >::: [
    test_register;
    test_align;
    test_init_lockspace;
  ] in
  OUnit2.run_test_tt_main @@ ounit2_of_ounit1 suite
