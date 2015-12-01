type lockspace
type resource
type handle

exception Sanlk_error of string

val init_lockspace : ?offset:int64 -> ?max_hosts:int -> ?num_hosts:int -> string -> string -> lockspace
val add_lockspace : ?async:bool -> lockspace -> int -> unit
val rem_lockspace : ?async:bool -> ?unused:bool -> lockspace -> int -> unit
val get_alignment : string -> int64
val register : unit -> handle
val init_resource : ?max_hosts:int -> ?num_hosts:int -> lockspace -> (string * int64) list -> string -> resource
