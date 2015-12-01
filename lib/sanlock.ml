open Ctypes

module B = Ffi_bindings.Bindings(Ffi_generated)
module T = Ffi_bindings.Types(Ffi_generated_types)

module UInt32 = Ffi_bindings.UInt32
module UInt64 = Ffi_bindings.UInt64

module Disk = B.Sanlk_disk

let crush_flags =
  UInt32.(Infix.(
    List.fold_left (fun acc flag -> acc lor flag) zero))

let int_of_fd : Unix.file_descr -> int = Obj.magic
let fd_of_int : int -> Unix.file_descr = Obj.magic

exception Sanlk_error of string
let check_rv rv =
  if rv < 0 then
    try raise (Sanlk_error (List.assoc rv T.Return_value.result_map))
    with Not_found ->
      raise (Sanlk_error Unix.(error_message (EUNKNOWNERR (abs rv))))

let init_lockspace ?(max_hosts=0) ?(num_hosts=0) name host_id_disk =
  let ls = {
    B.Sanlk_lockspace.name;
    host_id = UInt64.zero;
    flags = UInt32.zero;
    host_id_disk;
  } in
  B.sanlock_init_lockspace ls null max_hosts num_hosts |> check_rv

let add_lockspace ?(async=false) lockspace =
  let add_flags = if async then T.Add_flag.([ add_async ]) else [] in
  let flags = crush_flags add_flags in
  B.sanlock_add_lockspace lockspace flags |> check_rv

let rem_lockspace ?(async=false) ?(unused=false) lockspace =
  let add_flags = if async then T.Remove_flag.([ rem_async ]) else [] in
  let flags = crush_flags add_flags in
  B.sanlock_rem_lockspace lockspace flags |> check_rv

let align disk =
  let alignment = B.sanlock_align disk in
  check_rv alignment;
  alignment

let register () =
  let sock_fd = B.sanlock_register () in
  check_rv sock_fd;
  fd_of_int sock_fd

let restrict ?(restrict) sock =
  let restrict_flags = match restrict with
  | Some `All -> [ T.Restrict_flag.restrict_all ]
  | Some `Sigkill -> [ T.Restrict_flag.restrict_sigkill ]
  | Some `Sigterm -> [ T.Restrict_flag.restrict_sigterm ]
  | None -> [] in
  let flags = crush_flags restrict_flags in
  B.sanlock_restrict sock flags |> check_rv

let init_resource ?(max_hosts=0) ?(num_hosts=0) name lockspace disks =
  let res = {
    B.Sanlk_resource.lockspace_name = lockspace;
    name;
    lver = UInt64.zero;
    data64 = UInt64.zero;
    data32 = UInt32.zero;
    unused = UInt32.zero;
    flags = UInt32.zero;
    num_disks = List.length disks |> UInt32.of_int;
    disks;
  } in
  B.sanlock_init_resource null res max_hosts num_hosts |> check_rv

let acquire sock resources options =
  let pid = 0 in  (* we're using sock > -1 as obtained from register() *)
  let flags = crush_flags [] in   (* No flags for acuqire despite prototype *)
  let res_arr = CArray.of_list B.Sanlk_resource.t resources in
  let res_count = CArray.length res_arr in
  let res_args = CArray.start res_arr in
  B.sanlock_acquire sock pid flags res_count res_args options

let release ?(all=false) sock resources =
  let pid = 0 in  (* we're using sock > -1 as obtained from register() *)
  let release_flags = if all then T.Release_flag.([ rel_all ]) else [] in
  let flags = crush_flags release_flags in
  let res_arr = CArray.of_list B.Sanlk_resource.t resources in
  let res_count = CArray.length res_arr in
  let res_args = CArray.start res_arr in
  B.sanlock_release sock pid flags res_count res_args
