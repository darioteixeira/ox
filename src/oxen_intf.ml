module type S = sig
  module Action : Action_intf.S

  type state

  val init :
    Config.t ->
    state

  val provide_environment :
    state ->
    bool array ->
    Action.t

  val provide_feedback :
    reward:float ->
    is_final_step:bool ->
    state ->
    unit
end
