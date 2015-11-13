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

let dummy_test =
  "This test is a placeholder" >:: fun () -> ()

let _ =
  let suite = "sanlock" >::: [
      dummy_test;
  ] in
  OUnit2.run_test_tt_main @@ ounit2_of_ounit1 suite
