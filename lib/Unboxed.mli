(** Packed arrays *)

(**{1 Packed arrays} 

   This module provides support for packed ('unboxed') arrays of
   values. *)

(**{2 Width descriptions} *)

type zero    = Zero
type 'n succ = Succ of 'n

type _ is_a_natural =
  | Zero :                       zero is_a_natural
  | Succ : 'n is_a_natural -> 'n succ is_a_natural

type (_, _) width =
  | Width0 : (unit, 'n) width
  | Width1 : ('a, 'n succ) width
  | Width2 : ('a * 'b, 'n succ succ) width
  | Width3 : ('a * 'b * 'c, 'n succ succ succ) width
  | Width4 : ('a * 'b * 'c * 'd, 'n succ succ succ succ) width
  | Width5 : ('a * 'b * 'c * 'd * 'e, 'n succ succ succ succ succ) width
  | Width6 : ('a * 'b * 'c * 'd * 'e * 'f, 'n succ succ succ succ succ succ) width
  | Width7 : ('a * 'b * 'c * 'd * 'e * 'f * 'g, 'n succ succ succ succ succ succ succ) width

(**{2 Packed arrays of monomorphic variant types} *)

module MonomorphicVariantArray : sig

  module type Element_Descriptor =
    sig
      type 'a constructor
      type size
      val size : size is_a_natural
      val width_of : 'a constructor -> ('a, size) width
      type t = Data : 'a constructor * 'a -> t
    end

  module type S =
    sig
      module Elt : Element_Descriptor

      type t
      type elt = Elt.t
      val empty  : t
      val create : int -> elt -> t
      val init   : int -> (int -> elt) -> t
      val get    : t -> int -> elt
      val set    : t -> int -> elt -> unit
      val length : t -> int
      val copy   : t -> t
    end

  module Make (Elt : Element_Descriptor)
         : S with module Elt = Elt
end


module MonomorphicVariantDynArray : sig

  module type Element_Descriptor =
    sig
      type 'a constructor
      type size
      val size : size is_a_natural
      val width_of : 'a constructor -> ('a, size) width
      type t = Data : 'a constructor * 'a -> t
    end

  module type S =
    sig
      module Elt : Element_Descriptor

      type t
      type elt = Elt.t
      val create : unit -> t
      val add : t -> elt -> unit
      val set : t -> int -> elt -> unit
      val get : t -> int -> elt
      val length : t -> int
      val to_array : t -> MonomorphicVariantArray.Make(Elt).t
      val copy : t -> t
    end

  module Make (Elt : Element_Descriptor)
         : S with module Elt = Elt
end

(**{2 Packed arrays of polymorphic variant types} *)

module PolymorphicVariantArray : sig
  module type Element_Descriptor =
    sig
      type ('a,'d) constructor
      type size
      val size : size is_a_natural
      val width_of : ('a,'d) constructor -> ('d,size) width
      type 'a t = Data : ('a,'d) constructor * 'd -> 'a t
    end

  module type S =
    sig
      module Elt : Element_Descriptor

      type 'a t
      type 'a elt = 'a Elt.t
      val empty  : 'a t
      val create : int -> 'a elt -> 'a t
      val init   : int -> (int -> 'a elt) -> 'a t
      val get    : 'a t -> int -> 'a elt
      val set    : 'a t -> int -> 'a elt -> unit
      val length : 'a t -> int
      val copy   : 'a t -> 'a t
    end

  module Make (Elt : Element_Descriptor)
         : S with module Elt = Elt
end

(**{2 Arrays of unboxed tuples with missing elements} *)

module PartialTupleArray : sig
  module type Element_Descriptor =
    sig
      type t
      type size
      val width : (t,size) width
    end

  module type S =
    sig
      module Elt : Element_Descriptor

      type t
      type elt = Elt.t

      val create : int -> t
      val get    : t -> int -> elt option
      val set    : t -> int -> elt -> unit
      val clear  : t -> int -> unit
    end

  module Make (Elt : Element_Descriptor)
         : S with module Elt = Elt
end
