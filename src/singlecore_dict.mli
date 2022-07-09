module Make (H : Dict.HASHABLE) : Dict.IMPL with type key = H.t 
