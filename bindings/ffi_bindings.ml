open Ctypes
open PosixTypes

module Types (F: Cstubs.Types.TYPE) = struct
  open F

  let name_len = constant "SANLK_NAME_LEN" int
  let path_len = constant "SANLK_PATH_LEN" int
end

module Bindings (F : Cstubs.FOREIGN) = struct
  open F

  module Sanlk_disk = struct
    type sanlk_disk
    let sanlk_disk : sanlk_disk structure typ = structure "sanlk_disk"
    let (-:) ty label = field sanlk_disk label ty
    let path = string_opt -: "path"
    let offset = uint64_t -: "offset"
    let pad1 = uint32_t -: "pad1"
    let pad2 = uint32_t -: "pad2"
    let () = seal sanlk_disk

    type t = sanlk_disk structure ptr
    let t = ptr sanlk_disk
  end

  module Sanlk_resource = struct
    type sanlk_resource
    let sanlk_resource : sanlk_resource structure typ = structure "sanlk_resource"
    let (-:) ty label = field sanlk_resource label ty
    (* TODO: It would be nice to use a char array of length SANLK_NAME_LEN above *)
    let lockspace_name = string_opt -: "lockspace_name" (* terminating \0 not required *)
    let name = string_opt -: "name"                     (* terminating \0 not required *)
    let lver = uint64_t -: "lver"     (* use with SANLK_RES_LVER *)
    let data64 = uint32_t -: "data64" (* per-resource command-specific data *)
    let data32 = uint32_t -: "data32" (* per-resource command-specific data *)
    let unused = uint32_t -: "unused"
    let flags = uint32_t -: "flags"   (* SANLK_RES_ *)
    let num_disks = uint32_t -: "num_disks"
    (* followed by num_disks sanlk_disk structs *)
    let disks = Sanlk_disk.t -: "disks"
    let () = seal sanlk_resource

    type t = sanlk_resource structure ptr
    let t = ptr sanlk_resource
  end

  module Sanlk_options = struct
    type sanlk_options
    let sanlk_options : sanlk_options structure typ = structure "sanlk_options"
    let (-:) ty label = field sanlk_options label ty
    let name = string_opt -: "name"
    let flags = uint32_t -: "flags"
    let len = uint32_t -: "len"
    (* followed by len bytes (migration input will use this) *)
    let str = ptr char -: "str"
    let () = seal sanlk_options

    type t = sanlk_options structure ptr
    let t = ptr sanlk_options
  end

  module Sanlk_lockspace = struct
    type sanlk_lockspace
    let sanlk_lockspace : sanlk_lockspace structure typ = structure "sanlk_lockspace"
    let (-:) ty label = field sanlk_lockspace label ty
    let name = string_opt -: "name"
    let host_id = uint64_t -: "host_id"
    let flags = uint32_t -: "flags"
    let host_id_disk = Sanlk_disk.sanlk_disk -: "sanlk_disk"
    let () = seal sanlk_lockspace

    type t = sanlk_lockspace structure ptr
    let t = ptr sanlk_lockspace
  end

  module Sanlk_host = struct
    type sanlk_host
    let sanlk_host : sanlk_host structure typ = structure "sanlk_host"
    let (-:) ty label = field sanlk_host label ty
    let host_id = uint64_t -: "host_id"
    let generation = uint64_t -: "generation"
    let timestamp = uint64_t -: "timestamp"
    let io_timeout = uint32_t -: "io_timeout"
    let flags = uint32_t -: "flags"
    let () = seal sanlk_host

    type t = sanlk_host structure ptr
    let t = ptr sanlk_host
  end

  let sanlock_add_lockspace_timeout = foreign "sanlock_add_lockspace_timeout"
    (Sanlk_lockspace.t @-> uint32_t @-> uint32_t @-> returning int)

  let sanlock_rem_lockspace = foreign "sanlock_rem_lockspace"
    (Sanlk_lockspace.t @-> uint32_t @-> returning int)

  let sanlock_get_lockspaces = foreign "sanlock_get_lockspaces"
    (ptr Sanlk_lockspace.t @-> ptr int @-> uint32_t @-> returning int)

  let sanlock_get_hosts = foreign "sanlock_get_hosts"
    (string @-> uint64_t @-> ptr Sanlk_host.t @-> uint32_t @-> returning int)

  let sanlock_set_config = foreign "sanlock_set_config"
    (string @-> uint32_t @-> uint32_t @-> ptr void @-> returning int)

  let sanlock_align = foreign "sanlock_align"
    (Sanlk_disk.t @-> returning int)

  let sanlock_write_lockspace = foreign "sanlock_write_lockspace"
    (Sanlk_lockspace.t @-> int @-> uint32_t @-> uint32_t @-> returning int)

  let sanlock_read_lockspace = foreign "sanlock_read_lockspace"
    (Sanlk_lockspace.t @-> uint32_t @-> uint32_t @-> returning int)

  let sanlock_write_resource = foreign "sanlock_write_resource"
    (Sanlk_resource.t @-> int @-> uint32_t @-> uint32_t @-> returning int)

  let sanlock_read_resource = foreign "sanlock_read_resource"
    (Sanlk_resource.t @-> uint32_t @-> returning int)

  let sanlock_version = foreign "sanlock_version"
    (uint32_t @-> ptr uint32_t @-> ptr uint32_t @-> returning int)

  let sanlock_register = foreign "sanlock_register"
    (void @-> returning int)

  let sanlock_restrict = foreign "sanlock_restrict"
    (int @-> uint32_t @-> returning int)

  let sanlock_killpath = foreign "sanlock_killpath"
    (int @-> uint32_t @-> string @-> string @-> returning int)

  let sanlock_acquire = foreign "sanlock_acquire"
    (int @-> int @-> uint32_t @-> int @-> ptr (ptr Sanlk_resource.t) @-> Sanlk_options.t @-> returning int)

  let sanlock_release = foreign "sanlock_release"
    (int @-> int @-> uint32_t @-> int @-> ptr Sanlk_resource.t @-> returning int)

  let sanlock_convert = foreign "sanlock_convert"
    (int @-> int @-> uint32_t @-> Sanlk_resource.t @-> returning int)

  let sanlock_set_lvb = foreign "sanlock_set_lvb"
    (uint32_t @-> Sanlk_resource.t @-> string @-> int @-> returning int)

  let sanlock_get_lvb = foreign "sanlock_get_lvb"
    (uint32_t @-> Sanlk_resource.t @-> string @-> int @-> returning int)
end
