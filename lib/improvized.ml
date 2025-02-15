open Core

module T = struct
  type 'a t =
    | Nested :
        { f : 'b -> 'a t
        ; prev : 'b t
        }
        -> 'a t
    | Pure of 'a
    | Get : int t
    | Set : int -> unit t

  let bind x ~f = Nested { f; prev = x }
  let return x = Pure x
  let map = `Define_using_bind
end

include T
include Monad.Make (T)

let get = Get
let set s = Set s

let rec run : type a. a t -> int -> a * int =
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

let%test "test run" =
  let open Let_syntax in
  let add_one =
    let%bind current = get in
    let%bind () = set (current + 1) in
    return ()
  in
  let add_three =
    let%bind () = add_one in
    let%bind () = add_one in
    let%bind () = add_one in
    return ()
  in
  let (), s = run add_three 1 in
  s = 4
;;
