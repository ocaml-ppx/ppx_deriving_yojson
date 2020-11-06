include module type of Ppx_deriving_runtime

type 'a error_or = ('a, string) Result.result

val ( >>= ) : 'a error_or -> ('a -> 'b error_or) -> 'b error_or
val ( >|= ) : 'a error_or -> ('a -> 'b) -> 'b error_or
val map_bind : ('a -> 'b error_or) -> 'b list -> 'a list -> 'b list error_or

(** [safe_map f l] returns the same value as [List.map f l], but
    computes it tail-recursively so that large list lengths don't
    cause a stack overflow *)
val safe_map : ('a -> 'b) -> 'a list -> 'b list
