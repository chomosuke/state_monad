open Core

module type Int_state_monad = sig
  include Monad.S2

  val get : ('s, 's) t
  val set : 's -> (unit, 's) t
  val run : ('a, 's) t -> 's -> 'a * 's
end
