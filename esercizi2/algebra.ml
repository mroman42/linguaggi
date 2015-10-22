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

(* Integer monoids *)
module Sum : Monoid with type t = int =
struct
  type t = int
  let add = (+)
  let i = 0
end
