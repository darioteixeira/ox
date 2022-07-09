module type HASHABLE = sig
  include MoreLabels.Hashtbl.HashedType

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module type IMPL = sig
  type key
  type 'a t

  val key_to_yojson : key -> Yojson.Safe.t

  val key_of_yojson : Yojson.Safe.t -> (key, string) result

  val length : 'a t -> int

  val create : int -> 'a t

  val add : 'a t -> key:key -> data:'a -> unit

  val remove : 'a t -> key -> unit

  val find_opt : 'a t -> key -> 'a option

  val mem : 'a t -> key -> bool

  val iter : f:(key:key -> data:'a -> unit) -> 'a t -> unit

  val filter : f:(key:key -> data:'a -> bool) -> 'a t -> 'a t

  val filter_map_inplace : f:(key:key -> data:'a -> 'a option) -> 'a t -> unit

  val fold : f:(key:key -> data:'a -> 'b -> 'b) -> 'a t -> init:'b -> 'b

  val parallel_fold :
    init:'b ->
    neutral:'b ->
    combine:('b -> 'b -> 'b) ->
    f:(key:key -> data:'a -> 'b -> 'b) ->
    'a t ->
    'b

  val to_seq : 'a t -> (key * 'a) Seq.t
  val of_seq : (key * 'a) Seq.t -> 'a t

end

module type MAKETBL = functor (H : HASHABLE) -> IMPL with type key = H.t

module type S = sig
  include IMPL

  val random_opt : 'a t -> (key * 'a) option
  val random_exn : 'a t -> key * 'a

  val to_yojson : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t
  val of_yojson : (Yojson.Safe.t -> ('a, string) result) -> Yojson.Safe.t -> ('a t, string) result
end
