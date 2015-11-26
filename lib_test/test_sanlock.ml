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

let dummy_test =
  "This test is a placeholder" >:: fun () -> ()

let _ =
  check_sanlock_daemon ();
  let suite = "sanlock" >::: [
      dummy_test;
  ] in
  OUnit2.run_test_tt_main @@ ounit2_of_ounit1 suite
