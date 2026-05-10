module Game_State = Model.Game_state

(** [handle_movement_input state current_time window_x window_y] reads the
    current keyboard state, returns a new game state in which the player has
    been moved (clamped to the window of size [window_x] by [window_y]) and
    any newly-fired player bullets have been added. [current_time] is the
    elapsed game time in seconds; the player's fire rate is gated against it.
    Pure: does not mutate [state] beyond appending to its bullet list. *)
val handle_movement_input : Game_State.t -> float -> int -> int -> Game_State.t
