module Player = Player
module Bullet_Pattern = Bullet_pattern
module Game_State = Game_state
module Enemy = Enemy
open Raylib

(** A placeholder player used as the default starting point for the
    [set_player_*] builders. All fields have safe (but uninteresting) values; a
    real level should override the relevant ones. *)
val default_player : Player.player

(** A placeholder enemy used as the default starting point for the [set_enemy_*]
    builders. Despawns after one second; a real level should override the
    relevant fields. *)
val default_enemy : Enemy.enemy

(** [set_player_appearance ?player (w, h) filename death_filename hb_radius
     hb_pos] returns a copy of [player] (or [default_player] if omitted) with
    its sprite size set to [(w, h)], its sprite filenames are set to [filename]
    and [death_filename], its hitbox radius set to [hb_radius], and its hitbox
    offset set to [hb_pos]. *)
val set_player_appearance :
  ?player:Player.player ->
  int * int ->
  string ->
  string ->
  int ->
  int * int ->
  Player.player

(** [set_player_state ?player (x, y) lives bombs shot_type max_speed
     focus_speed] returns a copy of [player] (or [default_player] if omitted)
    with its position, lives, bombs, shot type, and movement speeds set to the
    given values. *)
val set_player_state :
  ?player:Player.player ->
  int * int ->
  int ->
  int ->
  Player.shot_type ->
  float ->
  float ->
  Player.player

(** [set_enemy_appearance ?enemy filename size] returns a copy of [enemy] (or
    [default_enemy] if omitted) with its sprite filename and sprite size set to
    the given values. *)
val set_enemy_appearance :
  ?enemy:Enemy.enemy -> string -> int * int -> Enemy.enemy

(** [set_enemy_state ?enemy (x, y) health spawn_time ttl pathing spawn_pattern]
    returns a copy of [enemy] (or [default_enemy] if omitted) with its position,
    health, spawn time, ttl, movement pattern, and bullet-spawn pattern set to
    the given values. The [(x, y)] integer coordinates are converted to floats.
*)
val set_enemy_state :
  ?enemy:Enemy.enemy ->
  int * int ->
  int ->
  float ->
  float ->
  Movement.velocity ->
  Bullet_pattern.bullet_pattern ->
  Enemy.enemy

(** [initialize_state enemies player] builds a fresh [Game_State.t] in the
    [StartScreen] phase, populated with [player] and the given list of queued
    [enemies] (sorted by spawn time). The bullet lists and active-enemy list are
    seeded with sentinel anchors required by the [BatDllist] representation. *)
val initialize_state : Enemy.enemy list -> Player.player -> Game_State.t
