module Player = Player
module Bullet = Bullet
module Enemy = Enemy
open Batteries

(** Which top-level phase the game is in: the title screen, active gameplay, or
    the game-over screen. *)
type game_phase =
  | StartScreen
  | Playing
  | GameOver
  | LevelEnd

(** The full state of the game at one point in time.

    Abstraction Function: A value [gs : t] represents the current state of the
    game at time [gs.elapsed_time], where:
    - [gs.player_bullets] is the set of all bullets fired by the player that are
      currently active (i.e., whose lifetime has not yet expired).
    - [gs.enemy_bullets] is the set of all bullets fired by enemies that are
      currently active.
    - [gs.active_enemies] is the set of all enemies currently active in the game
      (i.e., enemies whose spawn time has passed and whose lifetime has not
      expired).
    - [gs.queued_enemies] is the set of enemies that have not yet spawned,
      ordered by increasing spawn_time.
    - [gs.player] represents the current state of the player.

    Representation Invariant:
    - All bullets in [player_bullets] and [enemy_bullets] satisfy:
      [b.spawn_time <= elapsed_time <= b.spawn_time + b.ttl].
    - All enemies in [active_enemies] satisfy:
      [e.spawn_time <= elapsed_time <= e.spawn_time + e.ttl].
    - All enemies in [queued_enemies] satisfy: [e.spawn_time > elapsed_time].
    - [queued_enemies] is sorted in non-decreasing order of spawn_time.
    - The [BatDllist] structures are non-empty (BatDllist cannot represent an
      empty list); the head element of each is a sentinel anchor. *)
type t = {
  phase : game_phase;
  player_bullets : Bullet.bullet BatDllist.t;
  enemy_bullets : Bullet.bullet BatDllist.t;
  active_enemies : Enemy.enemy BatDllist.t;
  queued_enemies : Enemy.enemy BatDllist.t;
  player : Player.player;
  elapsed_time : float;
  score : int;
}

(** [update_state current_time game_state] advances the game one tick to the
    given [current_time]. This spawns any queued enemies whose spawn_time has
    arrived, expires bullets and enemies whose ttl has run out, moves all
    surviving bullets and enemies, emits new enemy bullets per their patterns,
    and resolves player-bullet vs. enemy collisions (each hit deals 1 damage).
*)
val update_state : float -> t -> t

(** [detect_collision game_state] is [true] iff at least one enemy bullet's
    position lies within the player's circular hitbox. *)
val detect_collision : t -> bool

(** [detect_collision game_state] is [true] iff at least one player bullet's
    position lies within the enemy's bounding box. *)

val detect_enemy_body_collision : t -> bool

(** [enemies_killed_this_frame] tells us how many enemies have died and been
    removed from the state since the last update. *)

val enemies_killed_this_frame : int ref
