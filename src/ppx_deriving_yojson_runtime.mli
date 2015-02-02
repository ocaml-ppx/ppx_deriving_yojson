val ( >>= ) :
  [ `Error of 'a | `Ok of 'b ] -> ('b -> ([> `Error of 'a ] as 'c)) -> 'c
val ( >|= ) :
  [ `Error of 'a | `Ok of 'b ] ->
  ('b -> 'c) -> [ `Error of 'a | `Ok of 'c ]
val map_bind :
  ('a -> [ `Error of 'b | `Ok of 'c ]) ->
  'c list -> 'a list -> [ `Error of 'b | `Ok of 'c list ]

module List : (module type of List)
module String : (module type of String)
module Bytes : (module type of Bytes)
module Int32 : (module type of Int32)
module Int64 : (module type of Int64)
module Nativeint : (module type of Nativeint)
module Array : (module type of Array)
