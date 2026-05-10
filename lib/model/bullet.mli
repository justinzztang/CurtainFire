module Movement = Movement

(** The visual / behavioral kind of a bullet. [Anchor] is a sentinel used as a
    placeholder in the bullet list and is never moved or rendered as a normal
    bullet; [Circle], [Oval], [Arrow], [Laser], and [Pellet] are the standard
    bullet shapes; [Custom] is reserved for one-off shapes. *)
type bullet_type =
  | Anchor
  | Circle
  | Oval
  | Arrow
  | Laser
  | Pellet
  | Custom

(** Abstraction Function: A value [b : bullet] represents a projectile in the
    game, where:
    - (b.x, b.y) is the position of the bullet in continuous (subpixel) space.
    - [b.bullet_type] determines the visual or behavioral type of the bullet.
    - [b.spawn_time] is the time at which the bullet was created.
    - [b.ttl] is the duration for which the bullet remains active.
    - [b.angle] and [b.speed] define the direction and magnitude of motion.

    Representation Invariant:
    - [b.ttl >= 0.0]
    - [b.speed >= 0.0] *)
type bullet = {
  x : float; (* we need subpixel precision to make things actually move right*)
  y : float;
  bullet_type : bullet_type;
  spawn_time : float;
  ttl : float; (* time-to-live; how long its active *)
  velocity : Movement.velocity;
}

(** [create_bullet x y bullet_type ttl path current_time] initializes bullet
    with specified arguments *)
val create_bullet :
  float -> float -> bullet_type -> float -> Movement.velocity -> float -> bullet

(** [move_bullet b current_time] updates the position of a bullet based on the
    current time and the velocity function defined in [b.velocity]*)
val move_bullet : bullet -> float -> bullet
