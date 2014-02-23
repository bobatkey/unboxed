(* Hashtables implemented with linear probing *)

module type Hashable = sig
  type t
  val equal : t -> t -> bool
  val hash  : t -> int
end

module String = struct
  include String
  let equal (x : string) (y : string) = x = y
  let hash (x : string) = Hashtbl.hash x
end

module type S = sig
  type 'a t
  type key
  val create : int -> 'a t
  val add    : 'a t -> key -> 'a -> unit
  val find   : 'a t -> key -> 'a
  val remove : 'a t -> key -> unit
  val count  : 'a t -> int
end

module Make (T : Hashable) : S with type key = T.t
  =
struct

  module Elt = struct
    open Unboxed

    type ('a,'d) constructor =
      | Empty   : ('a, unit) constructor
      | Deleted : ('a, unit) constructor
      | Value   : ('a, T.t * 'a) constructor

    type size = zero succ succ
    let size = Succ (Succ Zero)

    let width_of (type a) (type d) (c : (a,d) constructor) : (d,size) width =
      match c with
        | Empty   -> Width0
        | Deleted -> Width0
        | Value   -> Width2

    type 'a t = Data : ('a,'d) constructor * 'd -> 'a t
  end

  module Table = Unboxed.PolymorphicVariantArray.Make (Elt)

  open Elt

  type 'a t =
      { mutable table       : 'a Table.t
      ; mutable entry_count : int
      }

  type key = T.t

  let create size =
    let table = Table.create size (Data (Empty, ())) in
    { table; entry_count = 0 }

  exception Full

  let step_size = 1

  let primitive_add (type a) ({table} : a t) key value =
    let table_size = Table.length table in
    let base = T.hash key mod table_size in
    let rec search_and_insert step =
      if step = table_size then
        raise Full
      else
        let idx = (base + step * step_size) mod table_size in
        match Table.get table idx with
          | Data (Empty, ()) ->
            Table.set table idx (Data (Value, (key, value)))
          | Data (Deleted, ()) ->
            Table.set table idx (Data (Value, (key, value)))
          | Data (Value, _) ->
            search_and_insert (step+1)
    in
    search_and_insert 0

  let expand (type a) (({table} as t) : a t) =
    let table_size = Table.length table in
    let new_size   = table_size * 2 in
    let new_table  = Table.create new_size (Data (Empty, ())) in
    t.table <- new_table;
    (* Insert all the old elements into the new table *)
    for i = 0 to table_size - 1 do
      match (Table.get table i : a Elt.t) with
        | Data (Empty, ()) -> ()
        | Data (Deleted, ()) -> ()
        | Data (Value, (key, value)) ->
          primitive_add t key value
    done

  let add ({table} as t) key value =
    let rec insert () =
      try primitive_add t key value
      with Full -> (expand t; insert ())
    in
    insert ();
    t.entry_count <- t.entry_count + 1

  let find (type a) ({table} : a t) key : a =
    let table_size = Table.length table in
    let base = T.hash key mod table_size in
    let rec search step =
      if step = table_size then raise Not_found
      else
        let idx = (base + step * step_size) mod table_size in
        match Table.get table idx with
          | Data (Empty, ()) ->
            raise Not_found
          | Data (Value, (key', value)) when T.equal key key' ->
            value
          | Data (Value, _) ->
            search (step+1)
          | Data (Deleted, _) ->
            search (step+1)
    in
    search 0

  let remove (type a) ({table} as t : a t) key =
    let table_size = Table.length table in
    let base = T.hash key mod table_size in
    let rec search_and_delete step =
      if step = table_size then ()
      else
        let idx = (base + step * step_size) mod table_size in
        match Table.get table idx with
          | Data (Empty, ()) ->
            ()
          | Data (Value, (key', value)) when T.equal key key' ->
            t.entry_count <- t.entry_count - 1;
            Table.set table idx (Data (Deleted, ()))
          | Data (Value, _) ->
            search_and_delete (step+1)
          | Data (Deleted, _) ->
            search_and_delete (step+1)
    in
    search_and_delete 0

  let count {entry_count} =
    entry_count
end
