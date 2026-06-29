module Game_State = Model.Game_state

(** [handle_movement_input state current_frame] reads the current keyboard
    state, returns a new game state in which the player has been moved (clamped
    to the window of size [window_x] by [window_y] in [state]) and any
    newly-fired player bullets have been added. [current_frame] is the elapsed
    game time in frames; *)
val handle_movement_input : Game_State.t -> int -> Game_State.t
