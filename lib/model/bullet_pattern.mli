module Bullet = Bullet
module Player = Player

(** [Int_handler] Allows for randomness within a pattern, so that a random
    number isn't just generated once and used throughout the pattern.*)
type int_handler =
  | Int of int
  | RandInt of int * int

(** [Float_handler] Allows for randomness within a pattern, so that a random
    number isn't just generated once and used throughout the pattern. Similar to
    [Int_handler]*)
type float_handler =
  | Float of float
  | RandFloat of float * float

(** Abstraction function: A value [bp : bullet_pattern] represents how bullets
    will be fired, where
    - [Bullet] fires a single, "reference" bullet
    - [Timed (bpp, start, end)] fires pattern [bpp] from time [start] to time
      [end]
    - [Angled (bpp, angle)] fires pattern [bpp] at an angle offset [angle]
      counterclockwise of the horizontal
    - [Pulse (bpp, p)] fires pattern [bpp] every [p] seconds
    - [Arc (bpp, n, theta)] fires pattern [bpp] [n] times in an arc of angle
      [theta], evenly spaced
    - [Aimed (bpp)] fires pattern [bpp] at the angle between the spawn point and
      the player.
    - [Spin (bpp, omega)] fires pattern [bpp] at a certain angle determined
      after "spinning" with an angular speed of [omega]
    - [Iterate (bpp, n, mod_function)] fires pattern [bpp] [n times], with each
      iteration having a "modification" described by adding the modifiable
      properties (spawn position, ttl, velocity) of [mod_bullet] to each bullet
      in [bpp].
    - [Combo (bpps)] Fires each pattern in [bpps] simultaneously.
    - [Sequence (bpps)] Fires each pattern in [bpps] in sequence, where each
      element of [bpps] is a pair containing a bullet pattern and what time to
      move on to the next pattern.
    - [Manual (bpps)] Spawns each bullet contained in [bpps] *)

type bullet_pattern =
  | Nothing
  | Bullet of Bullet.bullet
  | Timed of bullet_pattern * float_handler * float_handler
  | Angled of bullet_pattern * float_handler
  | Pulse of bullet_pattern * float_handler
  | Arc of bullet_pattern * int_handler * float_handler
  | Aimed of bullet_pattern
  | Spin of bullet_pattern * float_handler
  | Iterate of bullet_pattern * int_handler * (int -> Bullet.bullet)
    (*take in an int and spit out a mod bullet*)
  | Combo of bullet_pattern list
  | Sequence of (bullet_pattern * float) list (*pattern, time limit*)
  | Manual of Bullet.bullet list

(** [eval_bp pattern player previous_time current_time enemy_x enemy_y
     enemy_spawn mods_bullet] evaluates the given pattern to a list of bullets
    to spawn *)
val eval_bp :
  bullet_pattern ->
  Player.player ->
  float ->
  float ->
  float ->
  float ->
  float ->
  Bullet.bullet ->
  Bullet.bullet list
