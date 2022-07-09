include module type of Multicore_hashtbl_intf

module Make (C : MULTICORE_CONFIG) : Ox.Dict.MAKETBL
