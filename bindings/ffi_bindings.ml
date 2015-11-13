open Ctypes
open PosixTypes

module Types (F: Cstubs.Types.TYPE) = struct
  open F
end

module Bindings (F : Cstubs.FOREIGN) = struct
  open F
end

module Foreign_bindings = struct
  open Foreign
end
