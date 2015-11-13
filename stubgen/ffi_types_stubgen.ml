let () =
  print_endline "#include <stdint.h>";
  print_endline "#include <sanlock.h>";
  print_endline "#include <sanlock_admin.h>";
  print_endline "#include <sanlock_resource.h>";
  Cstubs.Types.write_c Format.std_formatter (module Ffi_bindings.Types)
