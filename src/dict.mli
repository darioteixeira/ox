include module type of Dict_intf

module Make (Impl : IMPL) : S with type key = Impl.key and type 'a t = 'a Impl.t
