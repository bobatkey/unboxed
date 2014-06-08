(**********************************************************************)
type zero    = Zero
type 'n succ = Succ of 'n

type _ is_a_natural =
  | Zero :                       zero is_a_natural
  | Succ : 'n is_a_natural -> 'n succ is_a_natural

let rec int_of_natural : 'n. 'n is_a_natural -> int =
  fun (type n) (n : n is_a_natural) -> match n with
    | Zero   -> 0
    | Succ n -> 1 + int_of_natural n

type (_, _) width =
  | Width0 : (unit, 'n) width
  | Width1 : ('a, 'n succ) width
  | Width2 : ('a * 'b, 'n succ succ) width
  | Width3 : ('a * 'b * 'c, 'n succ succ succ) width
  | Width4 : ('a * 'b * 'c * 'd, 'n succ succ succ succ) width
  | Width5 : ('a * 'b * 'c * 'd * 'e, 'n succ succ succ succ succ) width
  | Width6 : ('a * 'b * 'c * 'd * 'e * 'f, 'n succ succ succ succ succ succ) width
  | Width7 : ('a * 'b * 'c * 'd * 'e * 'f * 'g, 'n succ succ succ succ succ succ succ) width

let int_of_width (type a) (type s) : (a, s) width -> int = function
  | Width0 -> 0
  | Width1 -> 1
  | Width2 -> 2
  | Width3 -> 3
  | Width4 -> 4
  | Width5 -> 5
  | Width6 -> 6
  | Width7 -> 7

let primitive_get (type a) (type size) array offset : (a,size) width -> a =
  function
  | Width0 -> ()
  | Width1 -> (Obj.obj array.(offset))
  | Width2 -> (Obj.obj array.(offset),
               Obj.obj array.(offset+1))
  | Width3 -> (Obj.obj array.(offset),
               Obj.obj array.(offset+1),
               Obj.obj array.(offset+2))
  | Width4 -> (Obj.obj array.(offset),
               Obj.obj array.(offset+1),
               Obj.obj array.(offset+2),
               Obj.obj array.(offset+3))
  | Width5 -> (Obj.obj array.(offset),
               Obj.obj array.(offset+1),
               Obj.obj array.(offset+2),
               Obj.obj array.(offset+3),
               Obj.obj array.(offset+4))
  | Width6 -> (Obj.obj array.(offset),
               Obj.obj array.(offset+1),
               Obj.obj array.(offset+2),
               Obj.obj array.(offset+3),
               Obj.obj array.(offset+4),
               Obj.obj array.(offset+5))
  | Width7 -> (Obj.obj array.(offset),
               Obj.obj array.(offset+1),
               Obj.obj array.(offset+2),
               Obj.obj array.(offset+3),
               Obj.obj array.(offset+4),
               Obj.obj array.(offset+5),
               Obj.obj array.(offset+6))

let primitive_set (type a) (type size) array offset (value : a) (width : (a,size) width) : unit =
  match width, value with
  | Width0, ()    ->
     ()
  | Width1, a     ->
     array.(offset) <- Obj.repr a
  | Width2, (a,b) ->
     array.(offset) <- Obj.repr a;
     array.(offset+1) <- Obj.repr b
  | Width3, (a,b,c) ->
     array.(offset) <- Obj.repr a;
     array.(offset+1) <- Obj.repr b;
     array.(offset+2) <- Obj.repr c
  | Width4, (a,b,c,d) ->
     array.(offset) <- Obj.repr a;
     array.(offset+1) <- Obj.repr b;
     array.(offset+2) <- Obj.repr c;
     array.(offset+3) <- Obj.repr d
  | Width5, (a,b,c,d,e) ->
     array.(offset) <- Obj.repr a;
     array.(offset+1) <- Obj.repr b;
     array.(offset+2) <- Obj.repr c;
     array.(offset+3) <- Obj.repr d;
     array.(offset+4) <- Obj.repr e
  | Width6, (a,b,c,d,e,f) ->
     array.(offset) <- Obj.repr a;
     array.(offset+1) <- Obj.repr b;
     array.(offset+2) <- Obj.repr c;
     array.(offset+3) <- Obj.repr d;
     array.(offset+4) <- Obj.repr e;
     array.(offset+5) <- Obj.repr f;
  | Width7, (a,b,c,d,e,f,g) ->
     array.(offset) <- Obj.repr a;
     array.(offset+1) <- Obj.repr b;
     array.(offset+2) <- Obj.repr c;
     array.(offset+3) <- Obj.repr d;
     array.(offset+4) <- Obj.repr e;
     array.(offset+5) <- Obj.repr f;
     array.(offset+6) <- Obj.repr g

let primitive_unset (type a) (type size) array offset (width : (a,size) width) : unit =
  match width with
  | Width0    ->
     ()
  | Width1     ->
     array.(offset) <- Obj.repr 0
  | Width2 ->
     array.(offset) <- Obj.repr 0;
     array.(offset+1) <- Obj.repr 0
  | Width3 ->
     array.(offset) <- Obj.repr 0;
     array.(offset+1) <- Obj.repr 0;
     array.(offset+2) <- Obj.repr 0
  | Width4 ->
     array.(offset) <- Obj.repr 0;
     array.(offset+1) <- Obj.repr 0;
     array.(offset+2) <- Obj.repr 0;
     array.(offset+3) <- Obj.repr 0
  | Width5 ->
     array.(offset) <- Obj.repr 0;
     array.(offset+1) <- Obj.repr 0;
     array.(offset+2) <- Obj.repr 0;
     array.(offset+3) <- Obj.repr 0;
     array.(offset+4) <- Obj.repr 0
  | Width6 ->
     array.(offset) <- Obj.repr 0;
     array.(offset+1) <- Obj.repr 0;
     array.(offset+2) <- Obj.repr 0;
     array.(offset+3) <- Obj.repr 0;
     array.(offset+4) <- Obj.repr 0;
     array.(offset+5) <- Obj.repr 0
  | Width7 ->
     array.(offset) <- Obj.repr 0;
     array.(offset+1) <- Obj.repr 0;
     array.(offset+2) <- Obj.repr 0;
     array.(offset+3) <- Obj.repr 0;
     array.(offset+4) <- Obj.repr 0;
     array.(offset+5) <- Obj.repr 0;
     array.(offset+6) <- Obj.repr 0

(**********************************************************************)
module MonomorphicVariantArray = struct
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
      val empty : t
      val create : int -> elt -> t
      val init : int -> (int -> elt) -> t
      val get : t -> int -> elt
      val set : t -> int -> elt -> unit
      val length : t -> int
      val copy : t -> t
    end

  module Make (D : Element_Descriptor) = struct
    module Elt = D
    open D

    let elem_size = 1 + int_of_natural size

    type t = Obj.t array

    type elt = D.t

    let empty = [| |]

    let set array i elt =
      let offset = i * elem_size in
      match elt with
      | Data (constructor, value) ->
         array.(offset) <- Obj.repr constructor;
         primitive_set array (offset+1) value (width_of constructor)

    let get array i =
      let offset = i * elem_size in
      let constructor = Obj.obj (Array.get array offset) in
      Data (constructor, primitive_get array (offset+1) (width_of constructor))

    let create n elt =
      let a = Array.create (n * elem_size) (Obj.repr 0) in
      for i = 0 to n - 1 do
        set a i elt
      done;
      a

    let init n f =
      let a = Array.create (n * elem_size) (Obj.repr 0) in
      for i = 0 to n - 1 do
        set a i (f i)
      done;
      a

    let length t =
      Array.length t / elem_size

    let copy t =
      Array.copy t

  end
end

(**********************************************************************)
module MonomorphicVariantDynArray = struct
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

  module Make (D : Element_Descriptor) = struct
    module Elt = D
    module D_Array = MonomorphicVariantArray.Make (D)

    open D

    let elem_size = 1 + int_of_natural size

    type t =
        { mutable elements : Obj.t array
        ; mutable end_ptr  : int
        }

    type elt = D.t

    let create () =
      { elements = Array.create (16 * elem_size) (Obj.repr 0)
      ; end_ptr  = 0
      }

    let add t elt =
      let current_len = Array.length t.elements in
      if t.end_ptr = current_len then
        (let new_array = Array.create (current_len * 2) (Obj.repr 0) in
         Array.blit t.elements 0 new_array 0 current_len;
         t.elements <- new_array);
      D_Array.set t.elements t.end_ptr elt;
      t.end_ptr <- t.end_ptr + elem_size

    let set t i elt =
      if i < 0 || i >= t.end_ptr then
        raise (Invalid_argument "index out of bounds");
      D_Array.set t.elements i elt

    let get t i =
      if i < 0 || i >= t.end_ptr then
        raise (Invalid_argument "index out of bounds");
      D_Array.get t.elements i

    let length t =
      t.end_ptr / elem_size

    let to_array t =
      Array.sub t.elements 0 t.end_ptr

    let copy t =
      { elements = Array.copy t.elements
      ; end_ptr  = t.end_ptr
      }

  end
end

(**********************************************************************)
module PolymorphicVariantArray = struct
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
      val empty : 'a t
      val create : int -> 'a elt -> 'a t
      val init : int -> (int -> 'a elt) -> 'a t
      val get : 'a t -> int -> 'a elt
      val set : 'a t -> int -> 'a elt -> unit
      val length : 'a t -> int
      val copy : 'a t -> 'a t
    end

  module Make (D : Element_Descriptor) = struct
    module Elt = D
    open D

    let elem_size = 1 + int_of_natural size

    type 'a t = Obj.t array

    type 'a elt = 'a D.t

    let empty = [| |]

    let set array i elt =
      let offset = i * elem_size in
      match elt with
      | Data (constructor, value) ->
         array.(offset) <- Obj.repr constructor;
         primitive_set array (offset+1) value (width_of constructor)

    let get array i =
      let offset = i * elem_size in
      let constructor = Obj.obj (Array.get array offset) in
      Data (constructor, primitive_get array (offset+1) (width_of constructor))

    let create n elt =
      let a = Array.create (n * elem_size) (Obj.repr 0) in
      for i = 0 to n - 1 do
        set a i elt
      done;
      a

    let init n f =
      let a = Array.create (n * elem_size) (Obj.repr 0) in
      for i = 0 to n - 1 do
        set a i (f i)
      done;
      a

    let length t =
      Array.length t / elem_size

    let copy t =
      Array.copy t

  end
end

(**********************************************************************)
module TupleArray = struct
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

      val create : int -> (int -> elt) -> t
      val get    : t -> int -> elt
      val set    : t -> int -> elt -> unit
    end

  module Make (Elt : Element_Descriptor) = struct
    module Elt = Elt
    open Elt

    let elt_size =
      int_of_width width

    type t = Obj.t array

    type elt = Elt.t

    let create length init_f =
      let a = Array.make (length * elt_size) (Obj.repr 0) in
      let rec init_loop i offset =
        if i < length then
          (primitive_set a offset (init_f i) width;
           init_loop (i+1) (offset + elt_size))
      in
      init_loop 0 0;
      a

    let get a i =
      let offset = i * elt_size in
      if offset < 0 || offset >= Array.length a then
        raise (Invalid_argument "index out of bounds");
      primitive_get a offset width

    let set a i v =
      let offset = i * elt_size in
      if offset < 0 || offset >= Array.length a then
        raise (Invalid_argument "index out of bounds");
      primitive_set a offset v width
  end
end

module PartialTupleArray = struct
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

  module Make (Elt : Element_Descriptor) = struct
    module Elt = Elt
    open Elt

    let elt_size =
      int_of_width width

    type t = Obj.t array

    type elt = Elt.t

  (* Plan:
     - array is split into chunks of up to 1+31*elt_size words each
     - each chunk starts with a 31-bit bitmap header detailing which of the following chunks are filled in
   *)

    let chunk_size =
      1 + 31*elt_size

    let create n =
      if n < 0 then
        raise (Invalid_argument "Unboxed.PackedPartialArray.create");
      let chunks   = n / 31 in
      let leftover = n mod 31 in
      let size =
        chunks * chunk_size +
          if leftover > 0 then 1 + leftover*elt_size else 0
      in
      Array.make size (Obj.repr 0)

    let get array i =
      if i < 0 then
        raise (Invalid_argument "index out of bounds");
      let chunk_start  = (i / 31) * chunk_size in
      let chunk_offset = i mod 31 in
      let offset       = chunk_start + 1 + chunk_offset*elt_size in
      if offset >= Array.length array then
        raise (Invalid_argument "index out of bounds");
      let header = Obj.obj array.(chunk_start) in
      if header land (1 lsl chunk_offset) <> 0 then
        Some (primitive_get array offset width)
      else
        None

    let set array i elt =
      if i < 0 then
        raise (Invalid_argument "index out of bounds");
      let chunk_start  = (i / 31) * chunk_size in
      let chunk_offset = i mod 31 in
      let offset = chunk_start + 1 + chunk_offset*elt_size in
      if offset >= Array.length array then
        raise (Invalid_argument "index out of bounds");
      let header = Obj.obj array.(chunk_start) in
      array.(chunk_start) <- Obj.repr (header lor (1 lsl chunk_offset));
      primitive_set array offset elt width

    let clear array i =
      if i < 0 then
        raise (Invalid_argument "index out of bounds");
      let chunk_start  = (i / 31) * chunk_size in
      let chunk_offset = i mod 31 in
      let offset = chunk_start + 1 + chunk_offset*elt_size in
      if offset >= Array.length array then
        raise (Invalid_argument "index out of bounds");
      let header = Obj.obj array.(chunk_start) in
      array.(chunk_start) <- Obj.repr (header land (lnot (1 lsl chunk_offset)));
      primitive_unset array offset width
  end

end
