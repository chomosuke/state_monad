open Core

module type Int_state_monad = sig
  include Monad.S

  val get : int t
  val set : int -> unit t
  val run : 'a t -> int -> 'a * int
end
