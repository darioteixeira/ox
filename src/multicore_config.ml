module type S = sig
  val num_domains : int
  val task_pool : Domainslib.Task.pool
end
