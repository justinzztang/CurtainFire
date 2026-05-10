open OUnit2
module Movement = Model.Movement
module Bullet = Model.Bullet
module Player = Model.Player
module Bullet_pattern = Model.Bullet_pattern
module Enemy = Model.Enemy

(** A zero velocity for tests that do not care about movement. *)
let zero_vel = Movement.Linear (0.0, fun _ -> 0.0)

(** [almost_equal expected actual] is [true] when [expected] and [actual] differ
    by less than 0.001. Used for float field comparisons. *)
let almost_equal expected actual = Float.abs (expected -. actual) < 0.001

(** [make_player x y] creates a player at [(x, y)] with default stats. *)
let make_player x y =
  Player.create_player x y 3 3 Player.Linear (50, 50)
    "assets/missing_texture.png" "assets/missing_texture.png" 5 (0, 0) 10. 5.

(** [make_enemy x y pattern] creates an enemy at [(x, y)] with health 3, spawn
    time 0.0, ttl 9999.0, zero velocity, and the given bullet pattern. *)
let make_enemy x y pattern =
  Enemy.create_enemy x y 3 0.0 9999.0 zero_vel pattern
    "assets/missing_texture.png" (50, 50)

(** [bullets_str bs] is a human-readable string showing how many bullets are in
    [bs]. Used as a [~printer] for [assert_equal]. *)
let bullets_str bs = "<" ^ string_of_int (List.length bs) ^ " bullets>"

(** [size_str s] is a string representation of a sprite size pair [s]. Used as a
    [~printer] for [assert_equal]. *)
let size_str (w, h) = "(" ^ string_of_int w ^ ", " ^ string_of_int h ^ ")"

(** Tests that [create_enemy] stores x, y, and health correctly. *)
let test_create _ =
  let e = make_enemy 100.0 200.0 Bullet_pattern.Nothing in
  assert_equal ~printer:string_of_float 100.0 e.x;
  assert_equal ~printer:string_of_float 200.0 e.y;
  assert_equal ~printer:string_of_int 3 e.health

(** Tests that [create_enemy] sets [max_health] equal to [health]. *)
let test_create_max_health _ =
  let e = make_enemy 0.0 0.0 Bullet_pattern.Nothing in
  assert_equal ~printer:string_of_int 3 e.max_health

(** Tests that [create_enemy] sets [was_hit] to [false]. *)
let test_create_was_hit _ =
  let e = make_enemy 0.0 0.0 Bullet_pattern.Nothing in
  assert_equal ~printer:string_of_bool false e.was_hit

(** Tests that [create_enemy] stores [spawn_time] correctly. *)
let test_create_spawn_time _ =
  let e =
    Enemy.create_enemy 0.0 0.0 3 2.5 9999.0 zero_vel Bullet_pattern.Nothing
      "assets/missing_texture.png" (50, 50)
  in
  assert_bool "spawn_time = 2.5" (almost_equal 2.5 e.spawn_time)

(** Tests that [create_enemy] stores [ttl] correctly. *)
let test_create_ttl _ =
  let e =
    Enemy.create_enemy 0.0 0.0 3 0.0 42.0 zero_vel Bullet_pattern.Nothing
      "assets/missing_texture.png" (50, 50)
  in
  assert_bool "ttl = 42.0" (almost_equal 42.0 e.ttl)

(** Tests that [create_enemy] stores [sprite_size] correctly. *)
let test_create_sprite_size _ =
  let e = make_enemy 0.0 0.0 Bullet_pattern.Nothing in
  assert_equal ~printer:size_str (50, 50) e.sprite_size

(** Tests that [update_enemy] does not change position when velocity is zero. *)
let test_update_stationary _ =
  let e = make_enemy 100.0 200.0 Bullet_pattern.Nothing in
  let updated = Enemy.update_enemy e 1.0 in
  assert_equal ~printer:string_of_float 100.0 updated.x;
  assert_equal ~printer:string_of_float 200.0 updated.y

(** Tests that [update_enemy] increases x when given a rightward velocity. *)
let test_update_moves _ =
  let e =
    Enemy.create_enemy 100.0 200.0 3 0.0 9999.0
      (Movement.Linear (0.0, fun _ -> 5.0))
      Bullet_pattern.Nothing "assets/missing_texture.png" (50, 50)
  in
  let updated = Enemy.update_enemy e 0.0 in
  assert_bool "position changed" (updated.x <> 100.0 || updated.y <> 200.0)

(** Tests that [update_enemy] with a rightward velocity produces an x
    displacement of approximately 5.0. *)
let test_update_x_exact _ =
  let e =
    Enemy.create_enemy 0.0 0.0 3 0.0 9999.0
      (Movement.Linear (0.0, fun _ -> 5.0))
      Bullet_pattern.Nothing "assets/missing_texture.png" (50, 50)
  in
  let updated = Enemy.update_enemy e 0.0 in
  assert_bool "x ~ 5" (almost_equal 5.0 updated.x)

(** Tests that [update_enemy] with a leftward velocity (angle [Float.pi])
    decreases x. *)
let test_update_moves_left _ =
  let e =
    Enemy.create_enemy 100.0 100.0 3 0.0 9999.0
      (Movement.Linear (Float.pi, fun _ -> 5.0))
      Bullet_pattern.Nothing "assets/missing_texture.png" (50, 50)
  in
  let updated = Enemy.update_enemy e 0.0 in
  assert_bool "x decreased" (updated.x < 100.0)

(** Tests that [update_enemy] with a downward velocity (angle [-pi/2]) increases
    y. *)
let test_update_moves_down _ =
  let e =
    Enemy.create_enemy 100.0 100.0 3 0.0 9999.0
      (Movement.Linear (-.Float.pi /. 2.0, fun _ -> 5.0))
      Bullet_pattern.Nothing "assets/missing_texture.png" (50, 50)
  in
  let updated = Enemy.update_enemy e 0.0 in
  assert_bool "y increased" (updated.y > 100.0)

(** Tests that [update_enemy] with an upward velocity (angle [pi/2]) decreases
    y. *)
let test_update_moves_up _ =
  let e =
    Enemy.create_enemy 100.0 100.0 3 0.0 9999.0
      (Movement.Linear (Float.pi /. 2.0, fun _ -> 5.0))
      Bullet_pattern.Nothing "assets/missing_texture.png" (50, 50)
  in
  let updated = Enemy.update_enemy e 0.0 in
  assert_bool "y decreased" (updated.y < 100.0)

(** Tests that [update_enemy] passes elapsed time (current_time - spawn_time) to
    the velocity function, not raw current_time. *)
let test_update_elapsed_time _ =
  let e =
    Enemy.create_enemy 0.0 0.0 3 2.0 9999.0
      (Movement.Linear (0.0, fun t -> t))
      Bullet_pattern.Nothing "assets/missing_texture.png" (50, 50)
  in
  (* elapsed = 5.0 - 2.0 = 3.0, so dx = cos(0) * 3.0 = 3.0 *)
  let updated = Enemy.update_enemy e 5.0 in
  assert_bool "x ~ 3 (elapsed = 3)" (almost_equal 3.0 updated.x)

(** Tests that [update_enemy] does not alter [health]. *)
let test_update_preserves_health _ =
  let e =
    Enemy.create_enemy 0.0 0.0 5 0.0 9999.0
      (Movement.Linear (0.0, fun _ -> 1.0))
      Bullet_pattern.Nothing "assets/missing_texture.png" (50, 50)
  in
  let updated = Enemy.update_enemy e 0.0 in
  assert_equal ~printer:string_of_int 5 updated.health

(** Tests that [update_enemy] does not alter [was_hit]. *)
let test_update_preserves_was_hit _ =
  let e = make_enemy 0.0 0.0 Bullet_pattern.Nothing in
  let updated = Enemy.update_enemy e 1.0 in
  assert_equal ~printer:string_of_bool false updated.was_hit

(** Tests that [update_enemy] does not alter [spawn_time]. *)
let test_update_preserves_spawn_time _ =
  let e =
    Enemy.create_enemy 0.0 0.0 3 4.0 9999.0 zero_vel Bullet_pattern.Nothing
      "assets/missing_texture.png" (50, 50)
  in
  let updated = Enemy.update_enemy e 5.0 in
  assert_bool "spawn_time preserved" (almost_equal 4.0 updated.spawn_time)

(** Tests that [spawn_newer_bullets] with [Nothing] produces no bullets. *)
let test_spawn_nothing _ =
  let e = make_enemy 0.0 0.0 Bullet_pattern.Nothing in
  let result =
    Enemy.spawn_newer_bullets e 0.0 1.0 (make_player 200 200)
      Bullet_pattern.Nothing
  in
  assert_equal ~printer:bullets_str [] result

(** Tests that [spawn_newer_bullets] with a firing [Pulse] produces one bullet.
*)
let test_spawn_pulse _ =
  let b = Bullet.create_bullet 0.0 0.0 Bullet.Circle 5.0 zero_vel 0.0 in
  let bp =
    Bullet_pattern.Pulse (Bullet_pattern.Bullet b, Bullet_pattern.Float 1.0)
  in
  let e = make_enemy 0.0 0.0 bp in
  let result = Enemy.spawn_newer_bullets e 0.9 1.1 (make_player 200 200) bp in
  assert_equal ~printer:string_of_int 1 (List.length result)

(** Tests that [spawn_newer_bullets] with an [Arc] of 4 produces 4 bullets. *)
let test_spawn_arc _ =
  let b = Bullet.create_bullet 0.0 0.0 Bullet.Circle 5.0 zero_vel 0.0 in
  let bp =
    Bullet_pattern.Arc
      (Bullet_pattern.Bullet b, Bullet_pattern.Int 4, Bullet_pattern.Float 1.0)
  in
  let e = make_enemy 0.0 0.0 bp in
  let result = Enemy.spawn_newer_bullets e 0.0 1.0 (make_player 200 200) bp in
  assert_equal ~printer:string_of_int 4 (List.length result)

(** Tests that [spawn_newer_bullets] with [Aimed] produces one bullet. *)
let test_spawn_aimed _ =
  let b = Bullet.create_bullet 0.0 0.0 Bullet.Circle 5.0 zero_vel 0.0 in
  let bp = Bullet_pattern.Aimed (Bullet_pattern.Bullet b) in
  let e = make_enemy 0.0 0.0 bp in
  let result = Enemy.spawn_newer_bullets e 0.0 1.0 (make_player 200 200) bp in
  assert_equal ~printer:string_of_int 1 (List.length result)

(** Tests that bullets spawned by [spawn_newer_bullets] are positioned at the
    enemy's location. *)
let test_spawn_at_enemy_position _ =
  let b = Bullet.create_bullet 0.0 0.0 Bullet.Circle 5.0 zero_vel 0.0 in
  let bp = Bullet_pattern.Bullet b in
  let e = make_enemy 50.0 75.0 bp in
  let result = Enemy.spawn_newer_bullets e 0.0 1.0 (make_player 200 200) bp in
  let spawned = List.hd result in
  assert_bool "bullet x at enemy x" (almost_equal 50.0 spawned.x);
  assert_bool "bullet y at enemy y" (almost_equal 75.0 spawned.y)

(** Tests that [spawn_newer_bullets] uses [current_time] as [spawn_time] for new
    bullets. *)
let test_spawn_bullet_spawn_time _ =
  let b = Bullet.create_bullet 0.0 0.0 Bullet.Circle 5.0 zero_vel 0.0 in
  let bp = Bullet_pattern.Bullet b in
  let e = make_enemy 0.0 0.0 bp in
  let result = Enemy.spawn_newer_bullets e 0.0 3.5 (make_player 200 200) bp in
  let spawned = List.hd result in
  assert_bool "spawn_time = current_time" (almost_equal 3.5 spawned.spawn_time)

(** Tests that [anchor] has position (0, 0) and infinite ttl. *)
let test_anchor _ =
  assert_equal ~printer:string_of_float 0.0 Enemy.anchor.x;
  assert_equal ~printer:string_of_float 0.0 Enemy.anchor.y;
  assert_equal ~printer:string_of_bool true (Enemy.anchor.ttl = Float.infinity)

(** Tests that [anchor] has [sprite_filename] "anchor". *)
let test_anchor_sprite _ =
  assert_equal ~printer:(fun s -> s) "anchor" Enemy.anchor.sprite_filename

(** Tests that [anchor] has [health] 1 and [was_hit] false. *)
let test_anchor_fields _ =
  assert_equal ~printer:string_of_int 1 Enemy.anchor.health;
  assert_equal ~printer:string_of_bool false Enemy.anchor.was_hit

(** Tests that [anchor]'s [spawn_time] is 0.0. *)
let test_anchor_spawn_time _ =
  assert_bool "anchor spawn_time = 0" (almost_equal 0.0 Enemy.anchor.spawn_time)

(** Tests that [queue_anchor]'s [spawn_time] is [Float.infinity]. *)
let test_queue_anchor _ =
  assert_equal ~printer:string_of_bool true
    (Enemy.queue_anchor.spawn_time = Float.infinity)

(** Tests that [queue_anchor]'s [ttl] is [Float.infinity]. *)
let test_queue_anchor_ttl _ =
  assert_equal ~printer:string_of_bool true
    (Enemy.queue_anchor.ttl = Float.infinity)

(** Tests that [queue_anchor] has [sprite_filename] "anchor". *)
let test_queue_anchor_sprite _ =
  assert_equal ~printer:(fun s -> s) "anchor" Enemy.queue_anchor.sprite_filename

(** Tests that [update_enemy] on [anchor] leaves x at 0.0. *)
let test_anchor_update _ =
  let updated = Enemy.update_enemy Enemy.anchor 1.0 in
  assert_equal ~printer:string_of_float 0.0 updated.x

(** Tests that [update_enemy] on [queue_anchor] leaves x at 0.0. *)
let test_queue_anchor_update _ =
  let updated = Enemy.update_enemy Enemy.queue_anchor 1.0 in
  assert_equal ~printer:string_of_float 0.0 updated.x

(** Tests that bullets spawned by a pulse pattern can be moved after spawning.
*)
let test_spawn_move_bullet _ =
  let b = Bullet.create_bullet 0.0 0.0 Bullet.Circle 5.0 zero_vel 0.0 in
  let bp =
    Bullet_pattern.Pulse (Bullet_pattern.Bullet b, Bullet_pattern.Float 1.0)
  in
  let e = make_enemy 0.0 0.0 bp in
  let bullets = Enemy.spawn_newer_bullets e 0.9 1.1 (make_player 200 200) bp in
  let moved = List.map (fun blt -> Bullet.move_bullet blt 1.2) bullets in
  assert_equal ~printer:string_of_int 1 (List.length moved)

let suite =
  "enemy"
  >::: [
         "create" >:: test_create;
         "create_max_health" >:: test_create_max_health;
         "create_was_hit" >:: test_create_was_hit;
         "create_spawn_time" >:: test_create_spawn_time;
         "create_ttl" >:: test_create_ttl;
         "create_sprite_size" >:: test_create_sprite_size;
         "update_stationary" >:: test_update_stationary;
         "update_moves" >:: test_update_moves;
         "update_x_exact" >:: test_update_x_exact;
         "update_moves_left" >:: test_update_moves_left;
         "update_moves_down" >:: test_update_moves_down;
         "update_moves_up" >:: test_update_moves_up;
         "update_elapsed_time" >:: test_update_elapsed_time;
         "update_preserves_health" >:: test_update_preserves_health;
         "update_preserves_was_hit" >:: test_update_preserves_was_hit;
         "update_preserves_spawn_time" >:: test_update_preserves_spawn_time;
         "spawn_nothing" >:: test_spawn_nothing;
         "spawn_pulse" >:: test_spawn_pulse;
         "spawn_arc" >:: test_spawn_arc;
         "spawn_aimed" >:: test_spawn_aimed;
         "spawn_at_enemy_position" >:: test_spawn_at_enemy_position;
         "spawn_bullet_spawn_time" >:: test_spawn_bullet_spawn_time;
         "anchor" >:: test_anchor;
         "anchor_sprite" >:: test_anchor_sprite;
         "anchor_fields" >:: test_anchor_fields;
         "anchor_spawn_time" >:: test_anchor_spawn_time;
         "queue_anchor" >:: test_queue_anchor;
         "queue_anchor_ttl" >:: test_queue_anchor_ttl;
         "queue_anchor_sprite" >:: test_queue_anchor_sprite;
         "anchor_update" >:: test_anchor_update;
         "queue_anchor_update" >:: test_queue_anchor_update;
         "spawn_move_bullet" >:: test_spawn_move_bullet;
       ]

let () = run_test_tt_main suite
