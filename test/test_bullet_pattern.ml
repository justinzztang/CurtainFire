open OUnit2
module Movement = Model.Movement
module Bullet = Model.Bullet
module Player = Model.Player
module Bullet_pattern = Model.Bullet_pattern

(** A zero velocity for tests that do not care about movement direction. *)
let zero_vel = Movement.Linear (0.0, fun _ -> 0.0)

(** Pattern used to test pulse, aim, and arc together: fires a 10-bullet arc
    aimed at the player once per second. *)
let pulsing_aimed_arc =
  Bullet_pattern.(
    Pulse
      ( Aimed
          (Arc
             ( Bullet
                 (Bullet.create_bullet 0.0 0.0 Circle 5.0
                    (Linear (0.0, fun t -> 1.0))
                    0.0),
               Int 10,
               Float 1.57 )),
        Float 1.0 ))

(** [make_player x y] creates a player at [(x, y)] with default stats. *)
let make_player x y =
  Player.create_player x y 3 3 Player.Linear (50, 50)
    "assets/missing_texture.png" "assets/missing_texture.png" 5 (0, 0) 10. 5.

(** [make_bullet x y vel] creates a Circle bullet at [(x, y)] with velocity
    [vel], ttl 5.0, and spawn time 0.0. *)
let make_bullet x y vel = Bullet.create_bullet x y Bullet.Circle 5.0 vel 0.0

(** [make_mod_bullet ()] creates a zero Circle bullet used as the modifier
    accumulator in [eval]. *)
let make_mod_bullet () =
  Bullet.create_bullet 0.0 0.0 Bullet.Circle 0.0 zero_vel 0.0

(** [bullets_str bs] is a human-readable string showing how many bullets are in
    [bs]. Used as a [~printer] for [assert_equal]. *)
let bullets_str bs = "<" ^ string_of_int (List.length bs) ^ " bullets>"

(** [almost_equal expected actual] is [true] when [expected] and [actual] differ
    by less than 0.001. Used for float field comparisons. *)
let almost_equal expected actual = Float.abs (expected -. actual) < 0.001

(** A dummy player positioned at (200, 200) used as the default player in
    [eval]. *)
let dummy_player = make_player 200 200

(** [eval bp prev cur] evaluates bullet pattern [bp] with [dummy_player], time
    window [(prev, cur)], and the enemy fixed at the origin with spawn time 0.0.
*)
let eval bp prev cur =
  Bullet_pattern.eval_bp bp dummy_player prev cur 0.0 0.0 0.0
    (make_mod_bullet ())

(** Tests that [Nothing] produces no bullets. *)
let test_bp_nothing _ =
  assert_equal ~printer:bullets_str [] (eval Bullet_pattern.Nothing 0.0 1.0)

(** Tests that a single [Bullet] pattern produces exactly one bullet. *)
let test_bp_bullet _ =
  let b = make_bullet 0.0 0.0 zero_vel in
  assert_equal ~printer:string_of_int 1
    (List.length (eval (Bullet_pattern.Bullet b) 0.0 1.0))

(** Tests that [Bullet] sets the spawned bullet's position to the enemy's
    position (origin in [eval]). *)
let test_bp_bullet_position _ =
  let b = make_bullet 0.0 0.0 zero_vel in
  let bullets = eval (Bullet_pattern.Bullet b) 0.0 1.0 in
  let spawned = List.hd bullets in
  assert_bool "x at enemy" (almost_equal 0.0 spawned.x);
  assert_bool "y at enemy" (almost_equal 0.0 spawned.y)

(** Tests that [Pulse] fires when the period boundary is crossed between
    [previous_time] and [current_time]. *)
let test_bp_pulse_fires _ =
  let bp =
    Bullet_pattern.Pulse
      ( Bullet_pattern.Bullet (make_bullet 0.0 0.0 zero_vel),
        Bullet_pattern.Float 1.0 )
  in
  assert_equal ~printer:string_of_int 1 (List.length (eval bp 0.9 1.1))

(** Tests that [Pulse] does not fire when no period boundary is crossed. *)
let test_bp_pulse_no_fire _ =
  let bp =
    Bullet_pattern.Pulse
      ( Bullet_pattern.Bullet (make_bullet 0.0 0.0 zero_vel),
        Bullet_pattern.Float 1.0 )
  in
  assert_equal ~printer:bullets_str [] (eval bp 0.1 0.9)

(** Tests that [Pulse] does not fire when both times are in the same interval
    even late in the game. *)
let test_bp_pulse_same_interval _ =
  let bp =
    Bullet_pattern.Pulse
      ( Bullet_pattern.Bullet (make_bullet 0.0 0.0 zero_vel),
        Bullet_pattern.Float 1.0 )
  in
  assert_equal ~printer:bullets_str [] (eval bp 5.1 5.9)

(** Tests that [Arc] with [Int 5] produces exactly 5 bullets. *)
let test_bp_arc _ =
  let bp =
    Bullet_pattern.Arc
      ( Bullet_pattern.Bullet (make_bullet 0.0 0.0 zero_vel),
        Bullet_pattern.Int 5,
        Bullet_pattern.Float 1.0 )
  in
  assert_equal ~printer:string_of_int 5 (List.length (eval bp 0.0 1.0))

(** Tests that [Arc] with [Int 0] produces no bullets, exercising the empty
    [List.init] path. *)
let test_bp_arc_zero _ =
  let bp =
    Bullet_pattern.Arc
      ( Bullet_pattern.Bullet (make_bullet 0.0 0.0 zero_vel),
        Bullet_pattern.Int 0,
        Bullet_pattern.Float 1.0 )
  in
  assert_equal ~printer:bullets_str [] (eval bp 0.0 1.0)

(** Tests that [Aimed] produces exactly one bullet. *)
let test_bp_aimed _ =
  let bp =
    Bullet_pattern.Aimed (Bullet_pattern.Bullet (make_bullet 0.0 0.0 zero_vel))
  in
  assert_equal ~printer:string_of_int 1 (List.length (eval bp 0.0 1.0))

(** Tests that [Combo] of two [Bullet] patterns produces two bullets. *)
let test_bp_combo _ =
  let b = make_bullet 0.0 0.0 zero_vel in
  let bp =
    Bullet_pattern.Combo [ Bullet_pattern.Bullet b; Bullet_pattern.Bullet b ]
  in
  assert_equal ~printer:string_of_int 2 (List.length (eval bp 0.0 1.0))

(** Tests that [Combo []] (the empty list) produces no bullets, exercising the
    empty [List.map] path. *)
let test_bp_combo_empty _ =
  assert_equal ~printer:bullets_str [] (eval (Bullet_pattern.Combo []) 0.0 1.0)

(** Tests that [Combo] of three [Bullet] patterns produces three bullets. *)
let test_bp_combo_three _ =
  let b = make_bullet 0.0 0.0 zero_vel in
  let bp =
    Bullet_pattern.Combo
      [
        Bullet_pattern.Bullet b;
        Bullet_pattern.Bullet b;
        Bullet_pattern.Bullet b;
      ]
  in
  assert_equal ~printer:string_of_int 3 (List.length (eval bp 0.0 1.0))

(** Tests that [Timed] fires when the current time falls within the window. *)
let test_bp_timed_active _ =
  let bp =
    Bullet_pattern.Timed
      ( Bullet_pattern.Bullet (make_bullet 0.0 0.0 zero_vel),
        Bullet_pattern.Float 0.0,
        Bullet_pattern.Float 5.0 )
  in
  assert_equal ~printer:string_of_int 1 (List.length (eval bp 0.0 1.0))

(** Tests that [Timed] does not fire when the current time is before the start
    of the window. *)
let test_bp_timed_before _ =
  let bp =
    Bullet_pattern.Timed
      ( Bullet_pattern.Bullet (make_bullet 0.0 0.0 zero_vel),
        Bullet_pattern.Float 3.0,
        Bullet_pattern.Float 5.0 )
  in
  assert_equal ~printer:bullets_str [] (eval bp 0.0 1.0)

(** Tests that [Timed] does not fire when the current time is after the end of
    the window. *)
let test_bp_timed_after _ =
  let bp =
    Bullet_pattern.Timed
      ( Bullet_pattern.Bullet (make_bullet 0.0 0.0 zero_vel),
        Bullet_pattern.Float 0.0,
        Bullet_pattern.Float 5.0 )
  in
  assert_equal ~printer:bullets_str [] (eval bp 0.0 10.0)

(** Tests that [Timed] fires exactly at the start boundary (elapsed = s). *)
let test_bp_timed_at_start _ =
  let bp =
    Bullet_pattern.Timed
      ( Bullet_pattern.Bullet (make_bullet 0.0 0.0 zero_vel),
        Bullet_pattern.Float 1.0,
        Bullet_pattern.Float 5.0 )
  in
  assert_equal ~printer:string_of_int 1 (List.length (eval bp 0.0 1.0))

(** Tests that [Spin] produces exactly one bullet. *)
let test_bp_spin _ =
  let bp =
    Bullet_pattern.Spin
      ( Bullet_pattern.Bullet (make_bullet 0.0 0.0 zero_vel),
        Bullet_pattern.Float 1.0 )
  in
  assert_equal ~printer:string_of_int 1 (List.length (eval bp 0.0 1.0))

(** Tests that [Angled] produces exactly one bullet. *)
let test_bp_angled _ =
  let bp =
    Bullet_pattern.Angled
      ( Bullet_pattern.Bullet (make_bullet 0.0 0.0 zero_vel),
        Bullet_pattern.Float 1.0 )
  in
  assert_equal ~printer:string_of_int 1 (List.length (eval bp 0.0 1.0))

(** Tests that [Iterate] with [Int 3] produces exactly 3 bullets. *)
let test_bp_iterate _ =
  let bp =
    Bullet_pattern.Iterate
      ( Bullet_pattern.Bullet (make_bullet 0.0 0.0 zero_vel),
        Bullet_pattern.Int 3,
        fun _ -> make_mod_bullet () )
  in
  assert_equal ~printer:string_of_int 3 (List.length (eval bp 0.0 1.0))

(** Tests that [Iterate] with [Int 0] produces no bullets, exercising the empty
    [List.init] path. *)
let test_bp_iterate_zero _ =
  let bp =
    Bullet_pattern.Iterate
      ( Bullet_pattern.Bullet (make_bullet 0.0 0.0 zero_vel),
        Bullet_pattern.Int 0,
        fun _ -> make_mod_bullet () )
  in
  assert_equal ~printer:bullets_str [] (eval bp 0.0 1.0)

(** Tests that a [Sequence] whose first element's condition has not yet elapsed
    fires that element's pattern. *)
let test_bp_sequence_active _ =
  let bp =
    Bullet_pattern.Sequence
      [ (Bullet_pattern.Bullet (make_bullet 0.0 0.0 zero_vel), 5.0) ]
  in
  assert_equal ~printer:string_of_int 1 (List.length (eval bp 0.0 1.0))

(** Tests that a single-element [Sequence] past its condition falls through to
    [Sequence []] and returns no bullets. *)
let test_bp_sequence_expired _ =
  let bp =
    Bullet_pattern.Sequence
      [ (Bullet_pattern.Bullet (make_bullet 0.0 0.0 zero_vel), 0.5) ]
  in
  assert_equal ~printer:bullets_str [] (eval bp 0.0 1.0)

(** Tests that a two-element [Sequence] whose first condition is exceeded
    recurses to the second element and fires it. *)
let test_bp_sequence_second _ =
  let b = make_bullet 0.0 0.0 zero_vel in
  let bp =
    Bullet_pattern.Sequence
      [ (Bullet_pattern.Nothing, 0.5); (Bullet_pattern.Bullet b, 5.0) ]
  in
  assert_equal ~printer:string_of_int 1 (List.length (eval bp 0.0 1.0))

(** Tests that a [Sequence] whose every condition is exceeded falls all the way
    through to [Sequence []] and returns no bullets. *)
let test_bp_sequence_exhausted _ =
  let bp =
    Bullet_pattern.Sequence
      [
        (Bullet_pattern.Bullet (make_bullet 0.0 0.0 zero_vel), 0.3);
        (Bullet_pattern.Bullet (make_bullet 0.0 0.0 zero_vel), 0.7);
      ]
  in
  assert_equal ~printer:bullets_str [] (eval bp 0.0 1.0)

(** Tests that [Sequence []] directly produces no bullets. *)
let test_bp_sequence_empty _ =
  assert_equal ~printer:bullets_str []
    (eval (Bullet_pattern.Sequence []) 0.0 1.0)

(** Tests that [Manual] (and any unhandled variant) produces no bullets. *)
let test_bp_manual _ =
  assert_equal ~printer:bullets_str [] (eval (Bullet_pattern.Manual []) 0.0 1.0)

(** Tests that [RandInt] in an [Arc] produces a count within the specified
    range. *)
let test_bp_rand_int _ =
  let bp =
    Bullet_pattern.Arc
      ( Bullet_pattern.Bullet (make_bullet 0.0 0.0 zero_vel),
        Bullet_pattern.RandInt (3, 6),
        Bullet_pattern.Float 1.0 )
  in
  let n = List.length (eval bp 0.0 1.0) in
  assert_bool "3-5 bullets from RandInt" (n >= 3 && n <= 5)

(** Tests that [RandFloat] in an [Angled] pattern still produces exactly one
    bullet, regardless of the sampled angle. *)
let test_bp_rand_float _ =
  let bp =
    Bullet_pattern.Angled
      ( Bullet_pattern.Bullet (make_bullet 0.0 0.0 zero_vel),
        Bullet_pattern.RandFloat (0.0, 1.0) )
  in
  assert_equal ~printer:string_of_int 1 (List.length (eval bp 0.0 1.0))

(** The pulsing aimed arc fires 10 bullets when a pulse boundary is crossed. *)
let test_bp_pulsing_aimed_arc _ =
  assert_equal ~printer:string_of_int 10
    (List.length (eval pulsing_aimed_arc 0.9 1.1))

(** The bullets from [pulsing_aimed_arc] can be moved without error. *)
let test_bp_pulsing_aimed_arc_move _ =
  let bullets = eval pulsing_aimed_arc 0.9 1.1 in
  let moved = List.map (fun b -> Bullet.move_bullet b 1.2) bullets in
  assert_equal ~printer:string_of_int 10 (List.length moved)

(** Tests that bullets from an [Arc] can be moved without error. *)
let test_move_arc_bullets _ =
  let bp =
    Bullet_pattern.Arc
      ( Bullet_pattern.Bullet (make_bullet 0.0 0.0 zero_vel),
        Bullet_pattern.Int 3,
        Bullet_pattern.Float 1.0 )
  in
  let bullets = eval bp 0.0 1.0 in
  let moved = List.map (fun b -> Bullet.move_bullet b 1.1) bullets in
  assert_equal ~printer:string_of_int 3 (List.length moved)

(** Tests that bullets from an [Aimed] pattern can be moved without error. *)
let test_move_aimed_bullets _ =
  let bp =
    Bullet_pattern.Aimed (Bullet_pattern.Bullet (make_bullet 0.0 0.0 zero_vel))
  in
  let bullets = eval bp 0.0 1.0 in
  let moved = List.map (fun b -> Bullet.move_bullet b 1.1) bullets in
  assert_equal ~printer:string_of_int 1 (List.length moved)

(** Tests that bullets from a [Spin] pattern can be moved without error. *)
let test_move_spin_bullets _ =
  let bp =
    Bullet_pattern.Spin
      ( Bullet_pattern.Bullet (make_bullet 0.0 0.0 zero_vel),
        Bullet_pattern.Float 1.0 )
  in
  let bullets = eval bp 0.0 1.0 in
  let moved = List.map (fun b -> Bullet.move_bullet b 1.1) bullets in
  assert_equal ~printer:string_of_int 1 (List.length moved)

(** Tests that bullets from an [Angled] pattern can be moved without error. *)
let test_move_angled_bullets _ =
  let bp =
    Bullet_pattern.Angled
      ( Bullet_pattern.Bullet (make_bullet 0.0 0.0 zero_vel),
        Bullet_pattern.Float 0.5 )
  in
  let bullets = eval bp 0.0 1.0 in
  let moved = List.map (fun b -> Bullet.move_bullet b 1.1) bullets in
  assert_equal ~printer:string_of_int 1 (List.length moved)

let suite =
  "bullet_pattern tests"
  >::: [
         "bp_nothing" >:: test_bp_nothing;
         "bp_bullet" >:: test_bp_bullet;
         "bp_bullet_position" >:: test_bp_bullet_position;
         "bp_pulse_fires" >:: test_bp_pulse_fires;
         "bp_pulse_no_fire" >:: test_bp_pulse_no_fire;
         "bp_pulse_same_interval" >:: test_bp_pulse_same_interval;
         "bp_arc" >:: test_bp_arc;
         "bp_arc_zero" >:: test_bp_arc_zero;
         "bp_aimed" >:: test_bp_aimed;
         "bp_combo" >:: test_bp_combo;
         "bp_combo_empty" >:: test_bp_combo_empty;
         "bp_combo_three" >:: test_bp_combo_three;
         "bp_timed_active" >:: test_bp_timed_active;
         "bp_timed_before" >:: test_bp_timed_before;
         "bp_timed_after" >:: test_bp_timed_after;
         "bp_timed_at_start" >:: test_bp_timed_at_start;
         "bp_spin" >:: test_bp_spin;
         "bp_angled" >:: test_bp_angled;
         "bp_iterate" >:: test_bp_iterate;
         "bp_iterate_zero" >:: test_bp_iterate_zero;
         "bp_sequence_active" >:: test_bp_sequence_active;
         "bp_sequence_expired" >:: test_bp_sequence_expired;
         "bp_sequence_second" >:: test_bp_sequence_second;
         "bp_sequence_exhausted" >:: test_bp_sequence_exhausted;
         "bp_sequence_empty" >:: test_bp_sequence_empty;
         "bp_manual" >:: test_bp_manual;
         "bp_rand_int" >:: test_bp_rand_int;
         "bp_rand_float" >:: test_bp_rand_float;
         "bp_pulsing_aimed_arc" >:: test_bp_pulsing_aimed_arc;
         "move_arc_bullets" >:: test_move_arc_bullets;
         "move_aimed_bullets" >:: test_move_aimed_bullets;
         "move_spin_bullets" >:: test_move_spin_bullets;
         "move_angled_bullets" >:: test_move_angled_bullets;
         "pulsing_aimed_arc_move" >:: test_bp_pulsing_aimed_arc_move;
       ]

let () = run_test_tt_main suite
