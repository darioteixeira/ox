module type MULTICORE_CONFIG = sig
  val num_domains : int
  val task_pool : Domainslib.Task.pool
end
