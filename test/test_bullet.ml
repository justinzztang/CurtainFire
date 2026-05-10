open OUnit2
module Movement = Model.Movement
module Bullet = Model.Bullet

(** A zero velocity for use in tests that do not care about movement. *)
let zero_vel = Movement.Linear (0.0, fun _ -> 0.0)

(** [bt_str bt] is a string representation of bullet type [bt], used as a
    printer for [assert_equal]. *)
let bt_str = function
  | Bullet.Anchor -> "Anchor"
  | Bullet.Circle -> "Circle"
  | Bullet.Oval -> "Oval"
  | Bullet.Arrow -> "Arrow"
  | Bullet.Laser -> "Laser"
  | Bullet.Pellet -> "Pellet"
  | Bullet.Custom -> "Custom"

(** [almost_equal expected actual] is [true] when [expected] and [actual] differ
    by less than 0.001. *)
let almost_equal expected actual = Float.abs (expected -. actual) < 0.001

(** Tests that [create_bullet] correctly stores x, y, spawn_time, and ttl. *)
let test_create _ =
  let b = Bullet.create_bullet 100.0 200.0 Bullet.Circle 5.0 zero_vel 1.0 in
  assert_equal ~printer:string_of_float 100.0 b.x;
  assert_equal ~printer:string_of_float 200.0 b.y;
  assert_equal ~printer:string_of_float 1.0 b.spawn_time;
  assert_equal ~printer:string_of_float 5.0 b.ttl

(** Tests that [create_bullet] stores the given bullet type. *)
let test_create_bullet_type _ =
  let b = Bullet.create_bullet 0.0 0.0 Bullet.Laser 1.0 zero_vel 0.0 in
  assert_equal ~printer:bt_str Bullet.Laser b.bullet_type

(** Tests that [create_bullet] at the origin stores x=0 and y=0. *)
let test_create_origin _ =
  let b = Bullet.create_bullet 0.0 0.0 Bullet.Circle 1.0 zero_vel 0.0 in
  assert_equal ~printer:string_of_float 0.0 b.x;
  assert_equal ~printer:string_of_float 0.0 b.y

(** Tests that [create_bullet] uses [current_time] as [spawn_time], not zero. *)
let test_create_nonzero_spawn_time _ =
  let b = Bullet.create_bullet 0.0 0.0 Bullet.Circle 1.0 zero_vel 3.5 in
  assert_equal ~printer:string_of_float 3.5 b.spawn_time

(** Tests that a rightward bullet (angle 0) increases x after [move_bullet]. *)
let test_moves_right _ =
  let b =
    Bullet.create_bullet 100.0 100.0 Bullet.Circle 5.0
      (Movement.Linear (0.0, fun _ -> 5.0))
      0.0
  in
  let moved = Bullet.move_bullet b 0.0 in
  assert_bool "x increased" (moved.x > 100.0)

(** Tests that a leftward bullet (angle pi) decreases x after [move_bullet]. *)
let test_moves_left _ =
  let b =
    Bullet.create_bullet 100.0 100.0 Bullet.Circle 5.0
      (Movement.Linear (Float.pi, fun _ -> 5.0))
      0.0
  in
  let moved = Bullet.move_bullet b 0.0 in
  assert_bool "x decreased" (moved.x < 100.0)

(** Tests that a downward bullet (angle -pi/2) increases y after [move_bullet].
*)
let test_moves_down _ =
  let b =
    Bullet.create_bullet 100.0 100.0 Bullet.Circle 5.0
      (Movement.Linear (-.Float.pi /. 2.0, fun _ -> 5.0))
      0.0
  in
  let moved = Bullet.move_bullet b 0.0 in
  assert_bool "y increased" (moved.y > 100.0)

(** Tests that an upward bullet (angle pi/2) decreases y after [move_bullet]. *)
let test_moves_up _ =
  let b =
    Bullet.create_bullet 100.0 100.0 Bullet.Circle 5.0
      (Movement.Linear (Float.pi /. 2.0, fun _ -> 5.0))
      0.0
  in
  let moved = Bullet.move_bullet b 0.0 in
  assert_bool "y decreased" (moved.y < 100.0)

(** Tests that a diagonal bullet (angle -pi/4) increases both x and y after
    [move_bullet]. *)
let test_moves_diagonal _ =
  let b =
    Bullet.create_bullet 50.0 50.0 Bullet.Circle 5.0
      (Movement.Linear (-.Float.pi /. 4.0, fun _ -> 5.0))
      0.0
  in
  let moved = Bullet.move_bullet b 0.0 in
  assert_bool "x increased diag" (moved.x > 50.0);
  assert_bool "y increased diag" (moved.y > 50.0)

(** Tests that a zero-speed bullet does not change position after [move_bullet].
*)
let test_stationary _ =
  let b = Bullet.create_bullet 50.0 75.0 Bullet.Circle 5.0 zero_vel 0.0 in
  let moved = Bullet.move_bullet b 0.0 in
  assert_equal ~printer:string_of_float 50.0 moved.x;
  assert_equal ~printer:string_of_float 75.0 moved.y

(** Tests that [move_bullet] passes elapsed time (current_time - spawn_time) to
    the velocity function rather than raw current_time. *)
let test_move_uses_elapsed_time _ =
  let b =
    Bullet.create_bullet 0.0 0.0 Bullet.Circle 99.0
      (Movement.Linear (0.0, fun t -> t))
      2.0
  in
  (* elapsed = 5.0 - 2.0 = 3.0, so dx = cos(0)*3 = 3 *)
  let moved = Bullet.move_bullet b 5.0 in
  assert_bool "x = 3.0" (almost_equal 3.0 moved.x)

(** Tests that [move_bullet] does not alter the bullet's [bullet_type]. *)
let test_move_preserves_type _ =
  let b =
    Bullet.create_bullet 0.0 0.0 Bullet.Pellet 5.0
      (Movement.Linear (0.0, fun _ -> 1.0))
      0.0
  in
  let moved = Bullet.move_bullet b 0.0 in
  assert_equal ~printer:bt_str Bullet.Pellet moved.bullet_type

(** Tests that [move_bullet] does not alter the bullet's [ttl]. *)
let test_move_preserves_ttl _ =
  let b =
    Bullet.create_bullet 0.0 0.0 Bullet.Circle 7.5
      (Movement.Linear (0.0, fun _ -> 1.0))
      0.0
  in
  let moved = Bullet.move_bullet b 0.0 in
  assert_equal ~printer:string_of_float 7.5 moved.ttl

(** Tests that [move_bullet] does not alter the bullet's [spawn_time]. *)
let test_move_preserves_spawn_time _ =
  let b =
    Bullet.create_bullet 0.0 0.0 Bullet.Circle 5.0
      (Movement.Linear (0.0, fun _ -> 1.0))
      3.0
  in
  let moved = Bullet.move_bullet b 4.0 in
  assert_equal ~printer:string_of_float 3.0 moved.spawn_time

(** Tests that every [bullet_type] variant can be stored and retrieved from a
    bullet without modification. *)
let test_all_bullet_types _ =
  let types =
    [
      Bullet.Anchor;
      Bullet.Circle;
      Bullet.Oval;
      Bullet.Arrow;
      Bullet.Laser;
      Bullet.Pellet;
      Bullet.Custom;
    ]
  in
  List.iter
    (fun bt ->
      let b = Bullet.create_bullet 0.0 0.0 bt 5.0 zero_vel 0.0 in
      assert_equal ~printer:bt_str bt b.bullet_type)
    types

(** Tests that [move_bullet] with a zero-speed Anchor bullet leaves position
    unchanged, consistent with how Anchor bullets are treated specially in the
    game loop. *)
let test_anchor_stationary _ =
  let b = Bullet.create_bullet 10.0 20.0 Bullet.Anchor 5.0 zero_vel 0.0 in
  let moved = Bullet.move_bullet b 0.0 in
  assert_equal ~printer:string_of_float 10.0 moved.x;
  assert_equal ~printer:string_of_float 20.0 moved.y

(** Tests that the x displacement from [move_bullet] with a rightward velocity
    of speed 5 is approximately 5.0. *)
let test_move_x_displacement _ =
  let b =
    Bullet.create_bullet 0.0 0.0 Bullet.Circle 5.0
      (Movement.Linear (0.0, fun _ -> 5.0))
      0.0
  in
  let moved = Bullet.move_bullet b 0.0 in
  assert_bool "dx ~ 5" (almost_equal 5.0 moved.x)

(** Tests that multiple successive calls to [move_bullet] accumulate position
    correctly. *)
let test_move_accumulates _ =
  let b =
    Bullet.create_bullet 0.0 0.0 Bullet.Circle 99.0
      (Movement.Linear (0.0, fun _ -> 3.0))
      0.0
  in
  let b1 = Bullet.move_bullet b 0.0 in
  let b2 = Bullet.move_bullet b1 0.0 in
  assert_bool "x after two moves ~ 6" (almost_equal 6.0 b2.x)

let suite =
  "bullet"
  >::: [
         "create" >:: test_create;
         "create_bullet_type" >:: test_create_bullet_type;
         "create_origin" >:: test_create_origin;
         "create_nonzero_spawn_time" >:: test_create_nonzero_spawn_time;
         "moves_right" >:: test_moves_right;
         "moves_left" >:: test_moves_left;
         "moves_down" >:: test_moves_down;
         "moves_up" >:: test_moves_up;
         "moves_diagonal" >:: test_moves_diagonal;
         "stationary" >:: test_stationary;
         "move_uses_elapsed_time" >:: test_move_uses_elapsed_time;
         "move_preserves_type" >:: test_move_preserves_type;
         "move_preserves_ttl" >:: test_move_preserves_ttl;
         "move_preserves_spawn_time" >:: test_move_preserves_spawn_time;
         "all_bullet_types" >:: test_all_bullet_types;
         "anchor_stationary" >:: test_anchor_stationary;
         "move_x_displacement" >:: test_move_x_displacement;
         "move_accumulates" >:: test_move_accumulates;
       ]

let () = run_test_tt_main suite
