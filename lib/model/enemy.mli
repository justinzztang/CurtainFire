module Movement = Movement
module Bullet_Pattern = Bullet_pattern

(** Abstraction Function: A value [e : enemy] represents an enemy in the game
    world, where:
    - (e.x, e.y) is the position of the enemy.
    - [e.health] is the remaining health of the enemy.
    - [e.max_health] is the enemy's starting health (used to render its health
      bar).
    - [e.spawn_time] is the time at which the enemy appears.
    - [e.ttl] is the duration for which the enemy remains active.
    - [e.pathing] determines how the enemy moves over time.
    - [e.spawn_pattern] determines how the enemy generates bullets.
    - [e.sprite_filename] and [e.sprite_size] describe its visual appearance.
    - [e.was_hit] tells the renderer if the enemy should indicate that it was
      damaged.

    Representation Invariant:
    - [e.health >= 0]
    - [e.ttl >= 0.0]
    - [e.sprite_size] has positive width and height
    - [e.speed >= 0.0] *)
type enemy = {
  x : float;
  y : float;
  health : int;
  max_health : int;
  spawn_time : float;
  ttl : float; (* time-to-live; how long its active *)
  pathing : Movement.velocity;
  spawn_pattern : Bullet_Pattern.bullet_pattern;
  sprite_filename : string;
  sprite_size : int * int;
  was_hit : bool;
}

(** [create_enemy x y health spawn_time ttl pathing spawn_pattern
     sprite_filename sprite_size] builds a fresh enemy at position [(x, y)] with
    the given fields. *)
val create_enemy :
  float ->
  float ->
  int ->
  float ->
  float ->
  Movement.velocity ->
  Bullet_Pattern.bullet_pattern ->
  string ->
  int * int ->
  enemy

(** [update_enemy enemy current_time] updates the position of the enemy in
    accordance to its velocity function defined in [enemy.pathing] *)
val update_enemy : enemy -> float -> enemy

(** [spawn_newer_bullets enemy previous_time current_time player pattern]
    returns the list of bullets that [enemy] would emit during the time window
    from [previous_time] (exclusive) to [current_time] (inclusive) under the
    bullet [pattern]. The [player] argument is consulted by aimed sub-patterns.
*)
val spawn_newer_bullets :
  enemy ->
  float ->
  float ->
  Player.player ->
  Bullet_Pattern.bullet_pattern ->
  Bullet.bullet list

(** A sentinel enemy at [(0, 0)] with infinite [ttl] and no spawn pattern. Used
    as a placeholder so the [active_enemies] [BatDllist] is never empty
    (BatDllist does not represent empty lists). *)
val anchor : enemy

(** A sentinel enemy with [spawn_time = Float.infinity] and no spawn pattern.
    Used as a tail sentinel for the [queued_enemies] [BatDllist] so the
    [queued_enemy_transfer] traversal always has a stop element. *)
val queue_anchor : enemy
