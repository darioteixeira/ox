type _ t =
  | [] : unit t
  | ( :: ) : 'a * 'b t -> ('a -> 'b) t
