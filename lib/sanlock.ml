module B = Ffi_bindings.Foreign_bindings
module S = Ffi_bindings.Bindings(Ffi_generated)
module T = Ffi_bindings.Types(Ffi_generated_types)

let crush_flags =
  List.fold_left (fun acc flag -> acc lor flag) 0
