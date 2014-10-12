val ( >>= ) :
  [ `Error of 'a | `Ok of 'b ] -> ('b -> ([> `Error of 'a ] as 'c)) -> 'c
val ( >|= ) :
  [ `Error of 'a | `Ok of 'b ] ->
  ('b -> 'c) -> [ `Error of 'a | `Ok of 'c ]
val map_bind :
  ('a -> [ `Error of 'b | `Ok of 'c ]) ->
  'c list -> 'a list -> [ `Error of 'b | `Ok of 'c list ]
