let (>>=) x f =
  match x with `Ok x -> f x | (`Error _) as x -> x

let (>|=) x f =
  x >>= fun x -> `Ok (f x)

let rec map_bind f acc xs =
  match xs with
  | x :: xs -> f x >>= fun x -> map_bind f (x :: acc) xs
  | [] -> `Ok (List.rev acc)
