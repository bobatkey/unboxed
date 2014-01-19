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

(**********************************************************************)
module type Element_Descriptor = sig
  type 'a constructor
  type size
  val size : size is_a_natural
  val width_of : 'a constructor -> ('a, size) width
  type t = Data : 'a constructor * 'a -> t
end

module type S = sig
  type t
  type elt 
  val create : int -> elt -> t
  val init : int -> (int -> elt) -> t
  val get : t -> int -> elt
  val set : t -> int -> elt -> unit
  val length : t -> int
end

module Make (D : Element_Descriptor)
  : S with type elt = D.t
  =
struct
  open D

  let elem_size = 1 + int_of_natural size

  type t = Obj.t array

  type elt = D.t

  let set array i elt =
    let offset = i * elem_size in
    match elt with Data (constructor, value) ->
      array.(offset) <- Obj.repr constructor;
      match width_of constructor, value with
        | Width0, ()    ->
          ()
        | Width1, a     ->
          array.(offset+1) <- Obj.repr a
        | Width2, (a,b) ->
          array.(offset+1) <- Obj.repr a;
          array.(offset+2) <- Obj.repr b
        | Width3, (a,b,c) ->
          array.(offset+1) <- Obj.repr a;
          array.(offset+2) <- Obj.repr b;
          array.(offset+3) <- Obj.repr c
        | Width4, (a,b,c,d) ->
          array.(offset+1) <- Obj.repr a;
          array.(offset+2) <- Obj.repr b;
          array.(offset+3) <- Obj.repr c;
          array.(offset+4) <- Obj.repr d
        | Width5, (a,b,c,d,e) ->
          array.(offset+1) <- Obj.repr a;
          array.(offset+2) <- Obj.repr b;
          array.(offset+3) <- Obj.repr c;
          array.(offset+4) <- Obj.repr d;
          array.(offset+5) <- Obj.repr e
        | Width6, (a,b,c,d,e,f) ->
          array.(offset+1) <- Obj.repr a;
          array.(offset+2) <- Obj.repr b;
          array.(offset+3) <- Obj.repr c;
          array.(offset+4) <- Obj.repr d;
          array.(offset+5) <- Obj.repr e;
          array.(offset+6) <- Obj.repr f

  let get' (type a) array offset (constructor : a constructor) : a =
    match width_of constructor with
      | Width0 ->
        ()
      | Width1 ->
        Obj.obj array.(offset+1)
      | Width2 ->
        (Obj.obj array.(offset+1),
         Obj.obj array.(offset+2))
      | Width3 ->
        (Obj.obj array.(offset+1),
         Obj.obj array.(offset+2),
         Obj.obj array.(offset+3))
      | Width4 ->
        (Obj.obj array.(offset+1),
         Obj.obj array.(offset+2),
         Obj.obj array.(offset+3),
         Obj.obj array.(offset+4))
      | Width5 ->
        (Obj.obj array.(offset+1),
         Obj.obj array.(offset+2),
         Obj.obj array.(offset+3),
         Obj.obj array.(offset+4),
         Obj.obj array.(offset+5))
      | Width6 ->
        (Obj.obj array.(offset+1),
         Obj.obj array.(offset+2),
         Obj.obj array.(offset+3),
         Obj.obj array.(offset+4),
         Obj.obj array.(offset+5),
         Obj.obj array.(offset+6))

  let get array i =
    let offset = i * elem_size in
    let constructor = Obj.obj (Array.get array offset) in
    Data (constructor, get' array offset constructor)

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

end

(**********************************************************************)
module type Element_Descriptor_1 = sig
  type ('a,'d) constructor
  type size
  val size : size is_a_natural
  val width_of : ('a,'d) constructor -> ('d,size) width
  type 'a t = Data : ('a,'d) constructor * 'd -> 'a t
end

module type S1 = sig
  type 'a t
  type 'a elt
  val create : int -> 'a elt -> 'a t
  val init : int -> (int -> 'a elt) -> 'a t
  val get : 'a t -> int -> 'a elt
  val set : 'a t -> int -> 'a elt -> unit
  val length : 'a t -> int
end

module Make1 (D : Element_Descriptor_1)
  : (S1 with type 'a elt = 'a D.t)
  =
struct
  open D

  let elem_size = 1 + int_of_natural size

  type 'a t = Obj.t array

  type 'a elt = 'a D.t

  let set array i elt =
    let offset = i * elem_size in
    match elt with Data (constructor, value) ->
      array.(offset) <- Obj.repr constructor;
      match width_of constructor, value with
        | Width0, ()    ->
          ()
        | Width1, a     ->
          array.(offset+1) <- Obj.repr a
        | Width2, (a,b) ->
          array.(offset+1) <- Obj.repr a;
          array.(offset+2) <- Obj.repr b
        | Width3, (a,b,c) ->
          array.(offset+1) <- Obj.repr a;
          array.(offset+2) <- Obj.repr b;
          array.(offset+3) <- Obj.repr c
        | Width4, (a,b,c,d) ->
          array.(offset+1) <- Obj.repr a;
          array.(offset+2) <- Obj.repr b;
          array.(offset+3) <- Obj.repr c;
          array.(offset+4) <- Obj.repr d
        | Width5, (a,b,c,d,e) ->
          array.(offset+1) <- Obj.repr a;
          array.(offset+2) <- Obj.repr b;
          array.(offset+3) <- Obj.repr c;
          array.(offset+4) <- Obj.repr d;
          array.(offset+5) <- Obj.repr e
        | Width6, (a,b,c,d,e,f) ->
          array.(offset+1) <- Obj.repr a;
          array.(offset+2) <- Obj.repr b;
          array.(offset+3) <- Obj.repr c;
          array.(offset+4) <- Obj.repr d;
          array.(offset+5) <- Obj.repr e;
          array.(offset+6) <- Obj.repr f

  let get' (type a) (type d) array offset
      (constructor : (a,d) constructor) : d =
    match width_of constructor with
      | Width0 ->
        ()
      | Width1 ->
        Obj.obj array.(offset+1)
      | Width2 ->
        (Obj.obj array.(offset+1),
         Obj.obj array.(offset+2))
      | Width3 ->
        (Obj.obj array.(offset+1),
         Obj.obj array.(offset+2),
         Obj.obj array.(offset+3))
      | Width4 ->
        (Obj.obj array.(offset+1),
         Obj.obj array.(offset+2),
         Obj.obj array.(offset+3),
         Obj.obj array.(offset+4))
      | Width5 ->
        (Obj.obj array.(offset+1),
         Obj.obj array.(offset+2),
         Obj.obj array.(offset+3),
         Obj.obj array.(offset+4),
         Obj.obj array.(offset+5))
      | Width6 ->
        (Obj.obj array.(offset+1),
         Obj.obj array.(offset+2),
         Obj.obj array.(offset+3),
         Obj.obj array.(offset+4),
         Obj.obj array.(offset+5),
         Obj.obj array.(offset+6))

  let get array i =
    let offset = i * elem_size in
    let constructor = Obj.obj (Array.get array offset) in
    Data (constructor, get' array offset constructor)

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

end
