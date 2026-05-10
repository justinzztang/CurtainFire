module Movement = Movement
module Bullet = Bullet
module Bullet_Pattern = Bullet_pattern

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

let create_enemy x y health spawn_time ttl pathing spawn_pattern sprite_filename
    sprite_size =
  {
    x;
    y;
    health;
    max_health = health;
    spawn_time;
    ttl;
    pathing;
    spawn_pattern;
    sprite_filename;
    sprite_size;
    was_hit = false;
  }

let update_enemy enemy current_time =
  let angle, speed =
    Movement.eval_velocity enemy.pathing
      (current_time -. enemy.spawn_time)
      enemy.x enemy.y
  in
  let dx, dy = (cos angle *. speed, sin angle *. speed) in
  { enemy with x = enemy.x +. dx; y = enemy.y +. dy }

let spawn_newer_bullets enemy previous_time current_time
    (player : Player.player) bp =
  Bullet_Pattern.(
    eval_bp bp player previous_time current_time enemy.x enemy.y
      enemy.spawn_time
      (Bullet.create_bullet 0. 0. Circle 0.
         (Linear (0.0, fun t -> 0.0))
         current_time))

let anchor =
  {
    x = 0.;
    y = 0.;
    health = 1;
    max_health = 1;
    ttl = Float.infinity;
    pathing = Linear (0.0, fun t -> 0.0);
    spawn_pattern = Nothing;
    spawn_time = 0.;
    sprite_filename = "anchor";
    sprite_size = (1, 1);
    was_hit = false;
  }

let queue_anchor =
  {
    x = 0.;
    y = 0.;
    health = 1;
    max_health = 1;
    ttl = Float.infinity;
    pathing = Linear (0.0, fun t -> 0.0);
    spawn_pattern = Nothing;
    spawn_time = Float.infinity;
    sprite_filename = "anchor";
    sprite_size = (1, 1);
    was_hit = false;
  }
