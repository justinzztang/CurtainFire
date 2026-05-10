type shot_type =
  | Linear
  | Cone
  | Homing

type player = {
  x : int;
  y : int;
  lives : int;
  bombs : int;
  is_shooting : bool;
  shot_type : shot_type;
  sprite_size : int * int;
  sprite_filename : string;
  death_filename : string;
  hitbox_radius : int;
  hitbox_position : int * int;
  max_speed : float;
  focus_speed : float;
  last_hit : float;
}

let create_player x y lives bombs shot_type sprite_size sprite_filename
    death_filename hitbox_radius hitbox_position max_speed focus_speed =
  {
    x;
    y;
    lives;
    bombs;
    shot_type;
    is_shooting = false;
    sprite_size;
    sprite_filename;
    death_filename;
    hitbox_radius;
    hitbox_position;
    max_speed;
    focus_speed;
    last_hit = -99.;
  }

type direction =
  | NO_DIR
  | N
  | E
  | S
  | W
  | NE
  | SE
  | SW
  | NW

let move_player player_object dir focus_held window_x window_y =
  let dx, dy =
    match dir with
    | NO_DIR -> (0., 0.)
    | N -> (0., 1.)
    | E -> (1., 0.)
    | S -> (0., -1.)
    | W -> (-1., 0.)
    (*1/sqrt2*)
    | NE -> (0.70710678, 0.70710678)
    | SE -> (0.70710678, -0.70710678)
    | SW -> (-0.70710678, -0.70710678)
    | NW -> (-0.70710678, 0.70710678)
  in
  let speed =
    if focus_held then player_object.focus_speed else player_object.max_speed
  in
  {
    player_object with
    x = min window_x (max 0 (player_object.x + int_of_float (dx *. speed)));
    y = min window_y (max 0 (player_object.y + int_of_float (dy *. speed)));
  }

let get_position player_object = (player_object.x, player_object.y)

let get_hitbox_position player_object =
  ( player_object.x + fst player_object.hitbox_position,
    player_object.y + snd player_object.hitbox_position )
