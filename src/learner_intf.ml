module type S = sig
  module Action : Action_intf.S

  type state [@@deriving repr]

  val create :
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
