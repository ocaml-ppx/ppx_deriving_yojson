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

let show_error_or f = function
  | Result.Error s -> "Error: " ^ s
  | Result.Ok x -> f x

let pp_error_or pp_x fmt = function
  | Result.Ok x -> pp_x fmt x
  | Result.Error e ->
    Format.pp_print_string fmt "Error ";
    Format.pp_print_string fmt e
