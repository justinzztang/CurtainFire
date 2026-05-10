module Movement = Movement

type bullet_type =
  | Anchor
  | Circle
  | Oval
  | Arrow
  | Laser
  | Pellet
  | Custom

type bullet = {
  x : float; (* we need subpixel precision to make things actually move right*)
  y : float;
  bullet_type : bullet_type;
  spawn_time : float;
  ttl : float; (* time-to-live; how long its active *)
  velocity : Movement.velocity;
}

let create_bullet x y bullet_type ttl velocity current_time =
  { x; y; bullet_type; ttl; velocity; spawn_time = current_time }

let move_bullet bullet current_time =
  let angle, speed =
    Movement.eval_velocity bullet.velocity
      (current_time -. bullet.spawn_time)
      bullet.x bullet.y
  in
  let dx, dy = (cos angle *. speed, sin angle *. speed) in
  (*print_endline (string_of_float dx ^ "X"); print_endline (string_of_float
    (cos 3.14159 *. 0.7) ^ "V");*)
  { bullet with x = bullet.x +. dx; y = bullet.y +. dy }
