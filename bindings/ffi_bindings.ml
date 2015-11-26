open Ctypes
open PosixTypes

include Unsigned

module Types (F: Cstubs.Types.TYPE) = struct
  open F

  let name_len = constant "SANLK_NAME_LEN" int
  let path_len = constant "SANLK_PATH_LEN" int

  module Add_flag = struct
    let add_async = constant "SANLK_ADD_ASYNC" uint32_t
  end

  module Rem_flag = struct
    let rem_async = constant "SANLK_REM_ASYNC" uint32_t
    let rem_unused = constant "SANLK_REM_UNUSED" uint32_t
  end

  module Return_value = struct
    let ok = constant "SANLK_OK" int
    let none = constant "SANLK_NONE" int (* unused *)
    let error = constant "SANLK_ERROR" int
    let aio_timeout = constant "SANLK_AIO_TIMEOUT" int

    (* run_ballot *)
    let dblock_read = constant "SANLK_DBLOCK_READ" int
    let dblock_write = constant "SANLK_DBLOCK_WRITE" int
    let dblock_lver = constant "SANLK_DBLOCK_LVER" int
    let dblock_mbal = constant "SANLK_DBLOCK_MBAL" int
    let dblock_checksum = constant "SANLK_DBLOCK_CHECKSUM" int

    (* verify_leader, leader_read, leader_write (paxos or delta)
     * (when adding to list, check if it should be a corrupt_result()) *)
    let leader_read = constant "SANLK_LEADER_READ" int
    let leader_write = constant "SANLK_LEADER_WRITE" int
    let leader_diff = constant "SANLK_LEADER_DIFF" int
    let leader_magic = constant "SANLK_LEADER_MAGIC" int
    let leader_version = constant "SANLK_LEADER_VERSION" int
    let leader_sectorsize = constant "SANLK_LEADER_SECTORSIZE" int
    let leader_lockspace = constant "SANLK_LEADER_LOCKSPACE" int
    let leader_resource = constant "SANLK_LEADER_RESOURCE" int
    let leader_numhosts = constant "SANLK_LEADER_NUMHOSTS" int
    let leader_checksum = constant "SANLK_LEADER_CHECKSUM" int

    (* paxos_lease_acquire *)
    let acquire_lver = constant "SANLK_ACQUIRE_LVER" int
    let acquire_lockspace = constant "SANLK_ACQUIRE_LOCKSPACE" int
    let acquire_iddisk = constant "SANLK_ACQUIRE_IDDISK" int
    let acquire_idlive = constant "SANLK_ACQUIRE_IDLIVE" int
    let acquire_owned = constant "SANLK_ACQUIRE_OWNED" int
    let acquire_other = constant "SANLK_ACQUIRE_OTHER" int
    let acquire_shretry = constant "SANLK_ACQUIRE_SHRETRY" int

    (* paxos_lease_release *)
    let release_lver = constant "SANLK_RELEASE_LVER" int
    let release_owner = constant "SANLK_RELEASE_OWNER" int

    (* delta_lease_renew, delta_lease_acquire *)
    let renew_owner = constant "SANLK_RENEW_OWNER" int
    let renew_diff = constant "SANLK_RENEW_DIFF" int
    let hostid_busy = constant "SANLK_HOSTID_BUSY" int

    (* request_token *)
    let request_magic = constant "SANLK_REQUEST_MAGIC" int
    let request_version = constant "SANLK_REQUEST_VERSION" int
    let request_old = constant "SANLK_REQUEST_OLD" int
    let request_lver = constant "SANLK_REQUEST_LVER" int
  end
end

module Bindings (F : Cstubs.FOREIGN) = struct
  open F

  module Sanlk_disk = struct
    type t = {
      path : string option;
      offset : UInt64.t;
      pad1 : UInt32.t;
      pad2 : UInt32.t;
    }

    type internal
    let internal : internal structure typ = structure "sanlk_disk"
    let (-:) ty label = field internal label ty
    let path = string_opt -: "path"
    let offset = uint64_t -: "offset"
    let pad1 = uint32_t -: "pad1"
    let pad2 = uint32_t -: "pad2"
    let () = seal internal

    let of_internal_ptr p =
      { path = getf !@p path;
        offset = getf !@p offset;
        pad1 = getf !@p pad1;
        pad2 = getf !@p pad2;
      }

    let to_internal_ptr t =
      let internal = make internal in
      setf internal path t.path;
      setf internal offset t.offset;
      setf internal pad1 t.pad1;
      setf internal pad2 t.pad2;
      addr internal

    let t = view ~read:of_internal_ptr ~write:to_internal_ptr (ptr internal)
  end

  module Sanlk_resource = struct
    type t = {
      lockspace_name : string option;
      name : string option;
      lver : UInt64.t;
      data64 : UInt64.t;
      data32 : UInt32.t;
      unused : UInt32.t;
      flags : UInt32.t;
      num_disks : UInt32.t;
      disks : Sanlk_disk.t list;
    }

    type internal
    let internal : internal structure typ = structure "sanlk_resource"
    let (-:) ty label = field internal label ty
    (* TODO: It would be nice to use a char array of length SANLK_NAME_LEN above *)
    let lockspace_name = string_opt -: "lockspace_name" (* terminating \0 not required *)
    let name = string_opt -: "name"                     (* terminating \0 not required *)
    let lver = uint64_t -: "lver"     (* use with SANLK_RES_LVER *)
    let data64 = uint64_t -: "data64" (* per-resource command-specific data *)
    let data32 = uint32_t -: "data32" (* per-resource command-specific data *)
    let unused = uint32_t -: "unused"
    let flags = uint32_t -: "flags"   (* SANLK_RES_ *)
    let num_disks = uint32_t -: "num_disks"
    (* followed by num_disks sanlk_disk structs *)
    let disks = ptr Sanlk_disk.internal -: "disks"
    let () = seal internal

    let of_internal_ptr p =
      let disks =
        let disks_ptr = getf !@p disks in
        let disks_len = getf !@p num_disks |> UInt32.to_int in
        CArray.(from_ptr disks_ptr disks_len |> to_list)
        |> List.map addr
        |> List.map (Sanlk_disk.of_internal_ptr) in
      { lockspace_name = getf !@p lockspace_name;
        name = getf !@p name;
        lver = getf !@p lver;
        data64 = getf !@p data64;
        data32 = getf !@p data32;
        unused = getf !@p unused;
        flags = getf !@p flags;
        num_disks = getf !@p num_disks;
        disks;
      }

    let to_internal_ptr t =
      let internal = make internal in
      setf internal lockspace_name t.lockspace_name;
      setf internal name t.name;
      setf internal lver t.lver;
      setf internal data64 t.data64;
      setf internal data32 t.data32;
      setf internal unused t.unused;
      setf internal flags t.flags;
      setf internal num_disks t.num_disks;
      let disks_ptr =
        List.map Sanlk_disk.to_internal_ptr t.disks
        |> List.map (!@)
        |> CArray.of_list Sanlk_disk.internal
        |> CArray.start in
      setf internal disks disks_ptr;
      addr internal

    let t = view ~read:of_internal_ptr ~write:to_internal_ptr (ptr internal)
  end

  module Sanlk_options = struct
    type t = {
      name : string option;
      flags : UInt32.t;
      len : UInt32.t;
      str : char list;
    }

    type internal
    let internal : internal structure typ = structure "sanlk_options"
    let (-:) ty label = field internal label ty
    let name = string_opt -: "name"
    let flags = uint32_t -: "flags"
    let len = uint32_t -: "len"
    (* followed by len bytes (migration input will use this) *)
    let str = (array 0 char) -: "str"
    let () = seal internal

    let of_internal_ptr p =
      { name = getf !@p name;
        flags = getf !@p flags;
        len = getf !@p len;
        str = getf !@p str |> CArray.to_list;
      }

    let to_internal_ptr t =
      let internal = make internal in
      setf internal name t.name;
      setf internal flags t.flags;
      setf internal len t.len;
      setf internal str CArray.(of_list char t.str);
      addr internal

    let t = view ~read:of_internal_ptr ~write:to_internal_ptr (ptr internal)
  end

  module Sanlk_lockspace = struct
    type t = {
      name : string option;
      host_id : UInt64.t;
      flags : UInt32.t;
      host_id_disk : Sanlk_disk.t;
    }

    type internal
    let internal : internal structure typ = structure "sanlk_lockspace"
    let (-:) ty label = field internal label ty
    let name = string_opt -: "name"
    let host_id = uint64_t -: "host_id"
    let flags = uint32_t -: "flags"
    let host_id_disk = Sanlk_disk.internal -: "sanlk_disk"
    let () = seal internal

    let of_internal_ptr p =
      { name = getf !@p name;
        host_id = getf !@p host_id;
        flags = getf !@p flags;
        host_id_disk = getf !@p host_id_disk |> addr |> Sanlk_disk.of_internal_ptr;
      }

    let to_internal_ptr t =
      let internal = make internal in
      setf internal name t.name;
      setf internal host_id t.host_id;
      setf internal flags t.flags;
      setf internal host_id_disk (!@ (Sanlk_disk.to_internal_ptr t.host_id_disk));
      addr internal

    let t = view ~read:of_internal_ptr ~write:to_internal_ptr (ptr internal)
  end

  module Sanlk_host = struct
    type t = {
      host_id : UInt64.t;
      generation : UInt64.t;
      timestamp : UInt64.t;
      io_timeout : UInt32.t;
      flags : UInt32.t;
    }

    type internal
    let internal : internal structure typ = structure "sanlk_host"
    let (-:) ty label = field internal label ty
    let host_id = uint64_t -: "host_id"
    let generation = uint64_t -: "generation"
    let timestamp = uint64_t -: "timestamp"
    let io_timeout = uint32_t -: "io_timeout"
    let flags = uint32_t -: "flags"
    let () = seal internal

    let of_internal_ptr p =
      { host_id = getf !@p host_id;
        generation = getf !@p generation;
        timestamp = getf !@p timestamp;
        io_timeout = getf !@p io_timeout;
        flags = getf !@p flags;
      }

    let to_internal_ptr t =
      let internal = make internal in
      setf internal host_id t.host_id;
      setf internal generation t.generation;
      setf internal timestamp t.timestamp;
      setf internal io_timeout t.io_timeout;
      setf internal flags t.flags;
      addr internal

    let t = view ~read:of_internal_ptr ~write:to_internal_ptr (ptr internal)
  end

  (* add_lockspace returns:
   * 0: the lockspace has been added successfully
   * -EEXIST: the lockspace already exists
   * -EINPROGRESS: the lockspace is already in the process of being added
   * (the in-progress add may or may not succeed)
   * -EAGAIN: the lockspace is being removed  *)
  let sanlock_add_lockspace = foreign "sanlock_add_lockspace"
    (Sanlk_lockspace.t @-> uint32_t @-> returning int)

  (* rem_lockspace returns:
   * 0: the lockspace has been removed successfully
   * -EINPROGRESS: the lockspace is already in the process of being removed
   * -ENOENT: lockspace not found
   * -EBUSY: UNUSED was set and lockspace is being used
   *
   * The sanlock daemon will kill any pids using the lockspace when the
   * lockspace is removed (unless UNUSED is set).  *)
  let sanlock_rem_lockspace = foreign "sanlock_rem_lockspace"
    (Sanlk_lockspace.t @-> uint32_t @-> returning int)

  let sanlock_align = foreign "sanlock_align"
    (Sanlk_disk.t @-> returning int)

  let sanlock_register = foreign "sanlock_register"
    (void @-> returning int)

  let sanlock_restrict = foreign "sanlock_restrict"
    (int @-> uint32_t @-> returning int)

  let sanlock_acquire = foreign "sanlock_acquire"
    (int @-> int @-> uint32_t @-> int @-> (ptr Sanlk_resource.t) @-> Sanlk_options.t @-> returning int)

  let sanlock_release = foreign "sanlock_release"
    (int @-> int @-> uint32_t @-> int @-> (ptr Sanlk_resource.t) @-> returning int)

  (* The following functions are used by current lvm2 tree (v2_02_128) but are
   * not available in libsanlock v2.2 (the version shipped with Ubuntu 14.04 *)
  (****************************************************************************
  let sanlock_add_lockspace_timeout = foreign "sanlock_add_lockspace_timeout"
    (ptr Sanlk_lockspace.t @-> uint32_t @-> uint32_t @-> returning int)

  let sanlock_get_lockspaces = foreign "sanlock_get_lockspaces"
    (ptr Sanlk_lockspace.t @-> ptr int @-> uint32_t @-> returning int)

  let sanlock_get_hosts = foreign "sanlock_get_hosts"
    (string @-> uint64_t @-> ptr Sanlk_host.t @-> uint32_t @-> returning int)

  let sanlock_set_config = foreign "sanlock_set_config"
    (string @-> uint32_t @-> uint32_t @-> ptr void @-> returning int)

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

  let sanlock_killpath = foreign "sanlock_killpath"
    (int @-> uint32_t @-> string @-> string @-> returning int)

  let sanlock_convert = foreign "sanlock_convert"
    (int @-> int @-> uint32_t @-> Sanlk_resource.t @-> returning int)

  let sanlock_set_lvb = foreign "sanlock_set_lvb"
    (uint32_t @-> Sanlk_resource.t @-> string @-> int @-> returning int)

  let sanlock_get_lvb = foreign "sanlock_get_lvb"
    (uint32_t @-> Sanlk_resource.t @-> string @-> int @-> returning int)
  ****************************************************************************)
end
