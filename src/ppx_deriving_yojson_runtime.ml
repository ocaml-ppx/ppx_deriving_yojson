let (>>=) x f =
  match x with `Ok x -> f x | (`Error _) as x -> x

let (>|=) x f =
  x >>= fun x -> `Ok (f x)

let rec map_bind f acc xs =
  match xs with
  | x :: xs -> f x >>= fun x -> map_bind f (x :: acc) xs
  | [] -> `Ok (List.rev acc)


module List = List
module String = String
module Bytes = Bytes
module Int32 = Int32
module Int64 = Int64
module Nativeint = Nativeint
module Array = Array
