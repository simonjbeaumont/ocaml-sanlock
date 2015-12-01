open Ctypes
open PosixTypes

module UInt32 = Unsigned.UInt32
module UInt64 = Unsigned.UInt64

module Types (F: Cstubs.Types.TYPE) = struct
  open F

  let name_len = constant "SANLK_NAME_LEN" int
  let path_len = constant "SANLK_PATH_LEN" int

  module Add_flag = struct
    let add_async = constant "SANLK_ADD_ASYNC" uint32_t
  end

  module Remove_flag = struct
    let rem_async = constant "SANLK_REM_ASYNC" uint32_t
    let rem_unused = constant "SANLK_REM_UNUSED" uint32_t
  end

  module Restrict_flag = struct
    let restrict_all = constant "SANLK_RESTRICT_ALL" uint32_t
    let restrict_sigkill = constant "SANLK_RESTRICT_SIGKILL" uint32_t
    let restrict_sigterm = constant "SANLK_RESTRICT_SIGTERM" uint32_t
  end

  module Release_flag = struct
    let rel_all = constant "SANLK_REL_ALL" uint32_t
  end

  module Return_value = struct
    let result_map = [
      constant "SANLK_OK" int, "SANLK_OK";
      constant "SANLK_NONE" int, "SANLK_NONE";  (* unused *)
      constant "SANLK_ERROR" int, "SANLK_ERROR";
      constant "SANLK_AIO_TIMEOUT" int, "SANLK_AIO_TIMEOUT";

      (* run_ballot *)
      constant "SANLK_DBLOCK_READ" int, "SANLK_DBLOCK_READ";
      constant "SANLK_DBLOCK_WRITE" int, "SANLK_DBLOCK_WRITE";
      constant "SANLK_DBLOCK_LVER" int, "SANLK_DBLOCK_LVER";
      constant "SANLK_DBLOCK_MBAL" int, "SANLK_DBLOCK_MBAL";
      constant "SANLK_DBLOCK_CHECKSUM" int, "SANLK_DBLOCK_CHECKSUM";

      (* verify_leader, leader_read, leader_write (paxos or delta)
       * (when adding to list, check if it should be a corrupt_result()) *)
      constant "SANLK_LEADER_READ" int, "SANLK_LEADER_READ";
      constant "SANLK_LEADER_WRITE" int, "SANLK_LEADER_WRITE";
      constant "SANLK_LEADER_DIFF" int, "SANLK_LEADER_DIFF";
      constant "SANLK_LEADER_MAGIC" int, "SANLK_LEADER_MAGIC";
      constant "SANLK_LEADER_VERSION" int, "SANLK_LEADER_VERSION";
      constant "SANLK_LEADER_SECTORSIZE" int, "SANLK_LEADER_SECTORSIZE";
      constant "SANLK_LEADER_LOCKSPACE" int, "SANLK_LEADER_LOCKSPACE";
      constant "SANLK_LEADER_RESOURCE" int, "SANLK_LEADER_RESOURCE";
      constant "SANLK_LEADER_NUMHOSTS" int, "SANLK_LEADER_NUMHOSTS";
      constant "SANLK_LEADER_CHECKSUM" int, "SANLK_LEADER_CHECKSUM";

      (* paxos_lease_acquire *)
      constant "SANLK_ACQUIRE_LVER" int, "SANLK_ACQUIRE_LVER";
      constant "SANLK_ACQUIRE_LOCKSPACE" int, "SANLK_ACQUIRE_LOCKSPACE";
      constant "SANLK_ACQUIRE_IDDISK" int, "SANLK_ACQUIRE_IDDISK";
      constant "SANLK_ACQUIRE_IDLIVE" int, "SANLK_ACQUIRE_IDLIVE";
      constant "SANLK_ACQUIRE_OWNED" int, "SANLK_ACQUIRE_OWNED";
      constant "SANLK_ACQUIRE_OTHER" int, "SANLK_ACQUIRE_OTHER";
      constant "SANLK_ACQUIRE_SHRETRY" int, "SANLK_ACQUIRE_SHRETRY";

      (* paxos_lease_release *)
      constant "SANLK_RELEASE_LVER" int, "SANLK_RELEASE_LVER";
      constant "SANLK_RELEASE_OWNER" int, "SANLK_RELEASE_OWNER";

      (* delta_lease_renew, delta_lease_acquire *)
      constant "SANLK_RENEW_OWNER" int, "SANLK_RENEW_OWNER";
      constant "SANLK_RENEW_DIFF" int, "SANLK_RENEW_DIFF";
      constant "SANLK_HOSTID_BUSY" int, "SANLK_HOSTID_BUSY";

      (* request_token *)
      constant "SANLK_REQUEST_MAGIC" int, "SANLK_REQUEST_MAGIC";
      constant "SANLK_REQUEST_VERSION" int, "SANLK_REQUEST_VERSION";
      constant "SANLK_REQUEST_OLD" int, "SANLK_REQUEST_OLD";
      constant "SANLK_REQUEST_LVER" int, "SANLK_REQUEST_LVER";
    ]
  end
end

module Bindings (F : Cstubs.FOREIGN) = struct
  open F

  module Views = struct

    let max_path_len = 1024 (* TODO: get from type gen above *)
    let max_name_len = 48 (* TODO: get from type gen above *)

    let string_of_char_array a =
      CArray.(start a |> string_from_ptr ~length:(length a))

    let char_array_of_string max_length null s =
      (* Note: this function currently transparently truncates... *)
      let trimmed = try String.sub s 0 max_length with _ -> s in
      let char_array = CArray.(make char ~initial:'\000' max_length) in
      String.iteri (fun i c -> CArray.set char_array i c) trimmed;
      if null then CArray.set char_array (max_length - 1) '\000';
      char_array

    let sanlk_path =
      view ~write:(char_array_of_string max_path_len true)
        ~read:string_of_char_array (array max_path_len char)

    let sanlk_name =
      view ~write:(char_array_of_string max_name_len false)
        ~read:string_of_char_array (array max_name_len char)
  end

  module Sanlk_disk = struct
    type t = {
      path : string;
      offset : UInt64.t;
      pad1 : UInt32.t;
      pad2 : UInt32.t;
    }

    type internal
    let internal : internal structure typ = structure "sanlk_disk"
    let (-:) ty label = field internal label ty
    let path = Views.sanlk_path -: "path" (* is guarenteed \0 terminated *)
    let offset = uint64_t -: "offset"
    let pad1 = uint32_t -: "pad1"
    let pad2 = uint32_t -: "pad2"
    let () = seal internal

    let of_internal i =
      { path = getf i path;
        offset = getf i offset;
        pad1 = getf i pad1;
        pad2 = getf i pad2;
      }

    let to_internal t =
      let internal = make internal in
      setf internal path t.path;
      setf internal offset t.offset;
      setf internal pad1 t.pad1;
      setf internal pad2 t.pad2;
      internal

    let of_internal_ptr p = of_internal !@p
    let to_internal_ptr t = to_internal t |> addr

    let t = view ~read:of_internal_ptr ~write:to_internal_ptr (ptr internal)
  end

  module Sanlk_resource = struct
    type t = {
      lockspace_name : string;
      name : string;
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
    let lockspace_name = Views.sanlk_name -: "lockspace_name" (* terminating \0 not required *)
    let name = Views.sanlk_name -: "name"                     (* terminating \0 not required *)
    let lver = uint64_t -: "lver"     (* use with SANLK_RES_LVER *)
    let data64 = uint64_t -: "data64" (* per-resource command-specific data *)
    let data32 = uint32_t -: "data32" (* per-resource command-specific data *)
    let unused = uint32_t -: "unused"
    let flags = uint32_t -: "flags"   (* SANLK_RES_ *)
    let num_disks = uint32_t -: "num_disks"
    (* followed by num_disks sanlk_disk structs (flexible array member) *)
    let disks = array 0 Sanlk_disk.internal -: "disks" (* see view functions *)
    let () = seal internal

    let of_internal_ptr p =
      let disks_len = getf !@p num_disks |> UInt32.to_int in
      let disks_list =
        let arr_start = getf !@p disks |> CArray.start in
        CArray.from_ptr arr_start disks_len |> CArray.to_list
        |> List.map (Sanlk_disk.of_internal) in
      { lockspace_name = getf !@p lockspace_name;
        name = getf !@p name;
        lver = getf !@p lver;
        data64 = getf !@p data64;
        data32 = getf !@p data32;
        unused = getf !@p unused;
        flags = getf !@p flags;
        num_disks = getf !@p num_disks;
        disks = disks_list;
      }

    let to_internal_ptr t =
      let size = (sizeof internal + sizeof Sanlk_disk.internal * List.length t.disks) in
      let internal =
        allocate_n (abstract ~name:"" ~size ~alignment:1) 1
        |> to_voidp |> from_voidp internal |> (!@) in
      setf internal lockspace_name t.lockspace_name;
      setf internal name t.name;
      setf internal lver t.lver;
      setf internal data64 t.data64;
      setf internal data32 t.data32;
      setf internal unused t.unused;
      setf internal flags t.flags;
      setf internal num_disks t.num_disks;
      let disks_arr = getf internal disks in
      List.map Sanlk_disk.to_internal t.disks
      |> List.iteri (CArray.unsafe_set disks_arr);
      addr internal

    let t = view ~read:of_internal_ptr ~write:to_internal_ptr (ptr internal)
  end

  module Sanlk_options = struct
    type t = {
      name : string;
      flags : UInt32.t;
      len : UInt32.t;
      str : char list;
    }

    type internal
    let internal : internal structure typ = structure "sanlk_options"
    let (-:) ty label = field internal label ty
    let name = Views.sanlk_name -: "name"
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
      name : string;
      host_id : UInt64.t;
      flags : UInt32.t;
      host_id_disk : Sanlk_disk.t;
    }

    type internal
    let internal : internal structure typ = structure "sanlk_lockspace"
    let (-:) ty label = field internal label ty
    let name = Views.sanlk_name -: "name"
    let host_id = uint64_t -: "host_id"
    let flags = uint32_t -: "flags"
    let host_id_disk = Sanlk_disk.internal -: "host_id_disk"
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

  (* sanlock_init(struct sanlk_lockspace *ls, struct sanlk_resource *res,
                  int max_hosts, int num_hosts);
   * Ask sanlock daemon to initialize disk space.
   * Use max_hosts = 0 for default value.
   * Use num_hosts = 0 for default value.
   * Provide either lockspace or resource, not both  *)
  let sanlock_init_lockspace = foreign "sanlock_init"
    (Sanlk_lockspace.t @-> ptr void @-> int @-> int @-> returning int)

  let sanlock_init_resource = foreign "sanlock_init"
    (ptr void @-> Sanlk_resource.t @-> int @-> int @-> returning int)

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

  (* Returns the alignment in bytes required by sanlock_init()
   * (1MB for disks with 512 sectors, 8MB for disks with 4096 sectors)  *)
  let sanlock_align = foreign "sanlock_align"
    (Sanlk_disk.t @-> returning int)

  (* sock > -1, pid is ignored:
   * process creates registered connection and acquires/releases leases on
   * that connection for itself
   *
   * sock == -1, pid is used:
   * process asks daemon to acquire/release leases for another separately
   * registered pid  *)

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
