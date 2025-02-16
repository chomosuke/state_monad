open Core

module T = struct
  type ('a, 's) t = 's -> 'a * 's

  let bind m ~f =
    fun s ->
    let x, s = m s in
    let m = f x in
    m s
  ;;

  let return x = fun s -> x, s
  let map = `Define_using_bind
end

include T
include Monad.Make2 (T)

let get = fun s -> s, s
let set x = fun _ -> (), x
let run = Fn.id
