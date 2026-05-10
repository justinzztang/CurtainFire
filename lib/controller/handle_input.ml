module Game_State = Model.Game_state
module Player = Model.Player
module Bullet = Model.Bullet
open Raylib

(* handles input continuously for smooth movement instead of waiting for
   discrete key presses. it takes the current state and returns a brand new
   state record instead of mutating variables, keeping things pure. opposing
   keys (like A and D) cancel out to 0 *)
let handle_movement_input (state : Game_State.t) current_time window_x window_y
    =
  let dx =
    (if is_key_down Key.D || is_key_down Key.Right then 1 else 0)
    - if is_key_down Key.A || is_key_down Key.Left then 1 else 0
  in
  let dy =
    (if is_key_down Key.S || is_key_down Key.Down then 1 else 0)
    - if is_key_down Key.W || is_key_down Key.Up then 1 else 0
  in
  let dir =
    match (dx, dy) with
    | 0, 0 -> Player.NO_DIR
    | 0, 1 -> Player.N
    | 1, 0 -> Player.E
    | 0, -1 -> Player.S
    | -1, 0 -> Player.W
    | 1, 1 -> Player.NE
    | 1, -1 -> Player.SE
    | -1, -1 -> Player.SW
    | -1, 1 -> Player.NW
    | _ -> assert false
  in
  let moved_player =
    Player.move_player state.player dir
      (is_key_down Key.Left_shift)
      window_x window_y
  in

  if is_key_down Key.Space || is_key_down Key.Z then begin
    let prev_idx = int_of_float (state.elapsed_time /. 0.10) in
    let cur_idx = int_of_float (current_time /. 0.10) in
    let p_x = float_of_int Player.(moved_player.x) in
    let p_y = float_of_int Player.(moved_player.y) in
    if prev_idx < cur_idx then (*we want to limit player firerate*)
      let new_bullet i =
        let focus_multiplier =
          (*tightens the shot when focusing*)
          if is_key_down Key.Left_shift then 0.2 else 1.0
        in
        Bullet.create_bullet p_x p_y Bullet.Circle 5.0
          (Linear (1.5708 +. (i *. 0.1 *. focus_multiplier), fun t -> 15.))
          current_time
      in
      let _ =
        BatDllist.add state.player_bullets (new_bullet 0.0);
        BatDllist.add state.player_bullets (new_bullet (-1.0));
        BatDllist.add state.player_bullets (new_bullet 1.0)
      in
      { state with player = moved_player }
    else { state with player = moved_player }
  end
  else { state with player = moved_player }
