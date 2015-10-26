(* Algebraic definitions of the Monoid, Group and Ring modules.
   Parametrized over a type t. *)
module type Monoid = sig
  type t
  val add : t -> t -> t
  val i : t
end

module type Group = sig
  type t
  val add : t -> t -> t
  val inv : t -> t
  val i : t
end

module type Ring = sig
  type t
  val add : t -> t -> t
  val mul : t -> t -> t
  val i : t
end


(* Boolean monoids *)
module All : Monoid with type t = bool = 
struct
  type t = bool
  let add = (&&)
  let i = true 
end

module Any : Monoid with type t = bool = 
struct
  type t = bool
  let add = (||)
  let i = false
end

(* Integer groups *)
module Sum : Group with type t = int =
struct
  type t = int
  let add = (+)
  let inv = (fun x -> (-x))
  let i = 0
end

module Z4 : Group with type t = int =
struct
  type t = int
  let add = (fun x y -> (x+y) mod 4)
  let inv = (fun x -> (-x) mod 4)
  let i = 0
end


(* Ring examples *)
module Trivial : Ring with type t = unit =
struct
  type t = unit
  let add = (fun x y -> ())
  let mul = (fun x y -> ())
  let i = ()
end

module Integral : Ring with type t = int =
struct
  type t = int
  let add = (+)
  let mul = ( * )
  let i = 0
end


(* Provide functions to check the algebraic structure *)

