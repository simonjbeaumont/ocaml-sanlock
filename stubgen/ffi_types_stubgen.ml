let () =
  print_endline "#include <stdint.h>";
  print_endline "#include <sanlock.h>";
  Cstubs.Types.write_c Format.std_formatter (module Ffi_bindings.Types)
