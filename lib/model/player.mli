(** The kind of shot fired by the player: [Linear] is a straight shot, [Cone]
    spreads out, and [Homing] tracks a target. *)
type shot_type =
  | Linear
  | Cone
  | Homing

(** Type for the player-controlled game object. Needs to be able to move, shoot
    enemies, and interact with other objects.

    Abstraction Function: A value [p : player] represents the current state of
    the player in the game, where:
    - [(p.x, p.y)] is the position of the player on the screen.
    - [p.lives] is the number of remaining lives.
    - [p.bombs] is the number of bombs available.
    - [p.is_shooting] indicates whether the player is currently firing.
    - [p.shot_type] determines the pattern of the player's attacks.
    - [p.sprite_size] and [p.sprite_filename] describe the player's visual
      representation, with [p.death_filename] describing the sprite the player
      object takes on after dying.
    - [p.hitbox_radius] and [p.hitbox_position] define the collision hitbox
      relative to the player's position.
    - [p.max_speed] and [p.focus_speed] determine movement speed under normal
      and focused conditions.
    - [p.last_hit] tells the renderer when the player last died, so it can
      render the death sprite.

    Representation Invariant:
    - [p.lives >= 0]
    - [p.bombs >= 0]
    - [p.sprite_size] has positive width and height
    - [p.hitbox_radius >= 0]
    - [p.max_speed >= 0.0] and [p.focus_speed >= 0.0] *)
type player = {
  x : int;
  y : int;
  lives : int;
  bombs : int;
  is_shooting : bool;
  shot_type : shot_type;
  sprite_size : int * int; (*width by height in pixels*)
  sprite_filename : string; (* the path to the file in the assets directory*)
  death_filename : string;
  hitbox_radius : int; (* in pixels *)
  (* the position of the center of the hitbox relative to the center of the
     player sprite. so for example, (0,0) represents the same position as the
     player and (0,10) means the hitbox is considered to be 10 pixels below
     (x,y)*)
  hitbox_position : int * int;
  (*regular movement speed*)
  max_speed : float;
  (*slowed down "focus" speed*)
  focus_speed : float;
  last_hit : float;
}

(** [create_player starting_x starting_y lives bombs shot_type sprite_size
     sprite_filename death_filename hitbox_radius hitbox_position max_speed
     focus_speed] initializes a player with specified arguments. Ugly but
    sometimes necessary to be this verbose*)
val create_player :
  int ->
  int ->
  int ->
  int ->
  shot_type ->
  int * int ->
  string ->
  string ->
  int ->
  int * int ->
  float ->
  float ->
  player

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

(** [move_player player_object dir focus_held window_x window_y] update the
    player's position based on a direction and whether the focus slowdown is
    active, and stops the player from leaving the screen*)
val move_player : player -> direction -> bool -> int -> int -> player

(** [get_position player_object] return a pair with the players x and y position*)
val get_position : player -> int * int

(** [get_position player_object] return a pair with the player hitbox's x and y
    position*)
val get_hitbox_position : player -> int * int
