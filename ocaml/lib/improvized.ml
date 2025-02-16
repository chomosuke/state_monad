open Core

module T = struct
  type ('a, 's) t =
    | Nested :
        { f : 'b -> ('a, 's) t
        ; prev : ('b, 's) t
        }
        -> ('a, 's) t
    | Pure of 'a
    | Get : ('s, 's) t
    | Set : 's -> (unit, 's) t

  let bind m ~f = Nested { f; prev = m }
  let return x = Pure x
  let map = `Define_using_bind
end

include T
include Monad.Make2 (T)

let get = Get
let set s = Set s

let rec run : type a s. (a, s) t -> s -> a * s =
  fun m s ->
  match m with
  | Nested { f; prev } ->
    let x, s = run prev s in
    let m = f x in
    run m s
  | Pure x -> x, s
  | Get -> s, s
  | Set x -> (), x
;;
