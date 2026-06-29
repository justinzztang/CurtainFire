(**unused*)
type shot_type =
  | Straight
  | Cone
  | Homing
  | Custom

(**Type for the user-controlled player object. Able to move, shoot, and interact
   with other objects.
   - [x] and [y] are the coordinates of the object
   - [sprite_size], [sprite_filename], and [focus_filename] define the objects
     appearance
   - [hitbox_radius] and [hitbox_position] define the collision hitbox relative
     to the player's position.
   - [max_speed] and [focus_speed] determine movement speed under normal and
     focused conditions.
   - [focus] is a flag for if the player is focused, used to tell the renderer
     to render the focus sprite
   - [lives] [bombs] [is_shooting] [shot_type] [last_hit] are unused *)
type t = {
  x : float;
  y : float;
  lives : int;
  bombs : int;
  is_shooting : bool;
  shot_type : shot_type;
  sprite_size : int * int; (*width by height in pixels*)
  sprite_filename : string; (* the path to the file*)
  focus_filename : string;
  hitbox_radius : float; (* in pixels *)
  (* the position of the center of the hitbox relative to the center of the
     player sprite. so for example, (0,0) represents the same position as the
     player and (0,10) means the hitbox is considered to be 10 pixels below
     (x,y)*)
  hitbox_position : float * float;
  (*regular movement speed*)
  max_speed : float;
  (*slowed down focus speed*)
  focus_speed : float;
  last_hit : int;
  focus : bool;
}

(** [create_player x y lives bombs shot_type sprite_size sprite_filename
     focus_filename hitbox_radius hitbox_position max_speed focus_speed]
    initializes a player with specified arguments.*)
val create_player :
  float ->
  float ->
  int ->
  int ->
  shot_type ->
  int * int ->
  string ->
  string ->
  float ->
  float * float ->
  float ->
  float ->
  t

(** [get_position player_object] return a pair with the players x and y position*)
val get_position : t -> float * float

(** [get_position player_object] return a pair with the player hitbox's x and y
    position*)
val get_hitbox_position : t -> float * float
