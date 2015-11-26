module B = Ffi_bindings.Bindings(Ffi_generated)
module T = Ffi_bindings.Types(Ffi_generated_types)

let crush_flags =
  Ffi_bindings.UInt32.(Infix.(
    List.fold_left (fun acc flag -> acc lor flag) zero))

exception Sanlk_error of int
let check_rv rv =
  if rv <> T.Return_value.ok then raise (Sanlk_error rv)

let add_lockspace ?(async=false) lockspace =
  let add_flags = if async then T.Add_flag.([ add_async ]) else [] in
  let flags = crush_flags add_flags in
  B.sanlock_add_lockspace lockspace flags |> check_rv

let rem_lockspace ?(async=false) ?(unused=false) lockspace =
  let add_flags = if async then T.Rem_flag.([ rem_async ]) else [] in
  let flags = crush_flags add_flags in
  B.sanlock_rem_lockspace lockspace flags |> check_rv
