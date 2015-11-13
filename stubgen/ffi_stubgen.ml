let _ =
  let prefix = "libsanlock_stub" in
  let generate_ml, generate_c = ref false, ref false in
  Arg.(parse [ ("-ml", Set generate_ml, "Generate ML");
                ("-c", Set generate_c, "Generate C") ])
    (fun _ -> failwith "unexpected anonymous argument")
    "stubgen [-ml|-c]";
  match !generate_ml, !generate_c with
  | false, false
  | true, true ->
    failwith "Exactly one of -ml and -c must be specified"
  | true, false ->
    Cstubs.write_ml Format.std_formatter ~prefix (module Ffi_bindings.Bindings)
  | false, true ->
    print_endline "#include <stdint.h>";
    print_endline "#include <sanlock.h>";
    print_endline "#include <sanlock_admin.h>";
    print_endline "#include <sanlock_resource.h>";
    Cstubs.write_c Format.std_formatter ~prefix (module Ffi_bindings.Bindings)
