type shot_type =
  | Straight
  | Cone
  | Homing
  | Custom

type t = {
  x : float;
  y : float;
  lives : int;
  bombs : int;
  is_shooting : bool;
  shot_type : shot_type;
  sprite_size : int * int;
  sprite_filename : string;
  focus_filename : string;
  hitbox_radius : float;
  hitbox_position : float * float;
  max_speed : float;
  focus_speed : float;
  last_hit : int;
  focus : bool;
}

let create_player x y lives bombs shot_type sprite_size sprite_filename
    focus_filename hitbox_radius hitbox_position max_speed focus_speed =
  {
    x;
    y;
    lives;
    bombs;
    shot_type;
    is_shooting = false;
    sprite_size;
    sprite_filename;
    focus_filename;
    hitbox_radius;
    hitbox_position;
    max_speed;
    focus_speed;
    last_hit = -99;
    focus = false;
  }

let get_position player_object = (player_object.x, player_object.y)

let get_hitbox_position player_object =
  ( player_object.x +. fst player_object.hitbox_position,
    player_object.y +. snd player_object.hitbox_position )
