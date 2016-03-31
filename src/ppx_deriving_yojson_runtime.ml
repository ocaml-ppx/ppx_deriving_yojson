include Ppx_deriving_runtime

let (>>=) x f =
  match x with Result.Ok x -> f x | (Result.Error _) as x -> x

let (>|=) x f =
  x >>= fun x -> Result.Ok (f x)

let rec map_bind f acc xs =
  match xs with
  | x :: xs -> f x >>= fun x -> map_bind f (x :: acc) xs
  | [] -> Result.Ok (List.rev acc)

type 'a error_or = ('a, string) Result.result
