open Ctypes

module B = Ffi_bindings.Bindings(Ffi_generated)
module T = Ffi_bindings.Types(Ffi_generated_types)

module UInt32 = Ffi_bindings.UInt32
module UInt64 = Ffi_bindings.UInt64

type lockspace = B.Sanlk_lockspace.t
type lockspace_membership = B.Sanlk_lockspace.t
type resource = B.Sanlk_resource.t
type handle = Unix.file_descr

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

let make_disk ?(offset=0L) path =
  { B.Sanlk_disk.path;
    offset = UInt64.of_int64 offset;
    pad1 = UInt32.of_int 0;
    pad2 = UInt32.of_int 0;
  }

let no_options =
  { B.Sanlk_options.name = "";
    flags = crush_flags [];
    len = UInt32.zero;
    str = [];
  }

let init_lockspace ?(offset=0L) ?(max_hosts=0) ?(num_hosts=0) name path =
  let host_id_disk = make_disk ~offset path in
  let ls = {
    B.Sanlk_lockspace.name;
    host_id = UInt64.zero;
    flags = UInt32.zero;
    host_id_disk;
  } in
  B.sanlock_init_lockspace ls null max_hosts num_hosts |> check_rv;
  ls

let add_lockspace ?(async=false) lockspace host_id =
  let ls = { lockspace with B.Sanlk_lockspace.host_id = UInt64.of_int host_id } in
  let add_flags = if async then T.Add_flag.([ add_async ]) else [] in
  let flags = crush_flags add_flags in
  B.sanlock_add_lockspace ls flags |> check_rv;
  ls

let rem_lockspace ?(async=false) ?(unused=false) lockspace =
  let add_flags = if async then T.Remove_flag.([ rem_async ]) else [] in
  let flags = crush_flags add_flags in
  B.sanlock_rem_lockspace lockspace flags |> check_rv

let get_alignment path =
  let alignment = B.sanlock_align (make_disk path) in
  check_rv alignment;
  Int64.of_int alignment

let register () =
  let sock_fd = B.sanlock_register () in
  check_rv sock_fd;
  fd_of_int sock_fd

let restrict handle restrict =
  let sock = int_of_fd handle in
  let restrict_flags = match restrict with
  | `All -> [ T.Restrict_flag.restrict_all ]
  | `Sigkill -> [ T.Restrict_flag.restrict_sigkill ]
  | `Sigterm -> [ T.Restrict_flag.restrict_sigterm ] in
  let flags = crush_flags restrict_flags in
  B.sanlock_restrict sock flags |> check_rv

let init_resource ?(max_hosts=0) ?(num_hosts=0) lockspace disk_offsets name =
  let disks =
    List.map (fun (path, offset) -> make_disk ~offset path) disk_offsets in
  let res = {
    B.Sanlk_resource.lockspace_name = lockspace.B.Sanlk_lockspace.name;
    name;
    lver = UInt64.zero;
    data64 = UInt64.zero;
    data32 = UInt32.zero;
    unused = UInt32.zero;
    flags = UInt32.zero;
    num_disks = List.length disks |> UInt32.of_int;
    disks;
  } in
  B.sanlock_init_resource null res max_hosts num_hosts |> check_rv;
  res

let acquire ?(shared=false) handle resource =
  let acquire_flags = if shared then T.Acquire_flag.([ res_shared ]) else [] in
  let flags = crush_flags acquire_flags in
  (* We're using sock > -1 as obtained from register() so set pid = 0 *)
  let pid = 0 in
  let sock = int_of_fd handle in
  (* Simplify the API by only handling one at a time *)
  let res_count = 1 in
  let res_args = CArray.(of_list B.Sanlk_resource.t [resource] |> start) in
  B.sanlock_acquire sock pid flags res_count res_args no_options |> check_rv

let release handle resource =
  (* There's a flag to release all resources but we'll handle one at a time *)
  let flags = crush_flags [] in
  (* We're using sock > -1 as obtained from register() so set pid = 0 *)
  let pid = 0 in
  let sock = int_of_fd handle in
  (* Simplify the API by only handling one at a time *)
  let res_count = 1 in
  let res_args = CArray.(of_list B.Sanlk_resource.t [resource] |> start) in
  B.sanlock_release sock pid flags res_count res_args |> check_rv
