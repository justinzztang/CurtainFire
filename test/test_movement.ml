open OUnit2
module Movement = Model.Movement

(** [almost_equal expected actual] is [true] when [expected] and [actual] differ
    by less than 0.001.*)
let almost_equal expected actual = Float.abs (expected -. actual) < 0.001

(** Tests that [Linear] with a nonzero angle negates it and returns the correct
    speed. *)
let test_linear _ =
  let angle, speed =
    Movement.eval_velocity (Movement.Linear (1.0, fun _ -> 3.0)) 0.0 0.0 0.0
  in
  assert_bool "angle" (almost_equal (-1.0) angle);
  assert_bool "speed" (almost_equal 3.0 speed)

(** Tests that [Linear] with angle 0.0 produces a zero angle and the expected
    speed. *)
let test_linear_zero _ =
  let angle, speed =
    Movement.eval_velocity (Movement.Linear (0.0, fun _ -> 5.0)) 0.0 0.0 0.0
  in
  assert_bool "zero angle" (almost_equal 0.0 angle);
  assert_bool "speed 5" (almost_equal 5.0 speed)

(** Tests that [Linear] with speed 0.0 yields zero speed regardless of angle. *)
let test_linear_no_speed _ =
  let _, speed =
    Movement.eval_velocity (Movement.Linear (0.0, fun _ -> 0.0)) 0.0 0.0 0.0
  in
  assert_bool "no speed" (almost_equal 0.0 speed)

(** Tests that [Linear] with angle [Float.pi] negates it correctly. *)
let test_linear_pi _ =
  let angle, _ =
    Movement.eval_velocity
      (Movement.Linear (Float.pi, fun _ -> 1.0))
      0.0 0.0 0.0
  in
  assert_bool "pi angle negated" (almost_equal (-.Float.pi) angle)

(** Tests that [Linear] with a negative input angle negates it to a positive
    result. *)
let test_linear_negative_angle _ =
  let angle, _ =
    Movement.eval_velocity (Movement.Linear (-1.5, fun _ -> 1.0)) 0.0 0.0 0.0
  in
  assert_bool "negative angle negated" (almost_equal 1.5 angle)

(** Tests that [Linear]'s speed function is evaluated at the supplied time. *)
let test_linear_time_dependent_speed _ =
  let _, speed =
    Movement.eval_velocity
      (Movement.Linear (0.0, fun t -> t *. 2.0))
      3.0 0.0 0.0
  in
  assert_bool "speed = 2*t" (almost_equal 6.0 speed)

(** Tests that a two-element [Sequence] at a time before the first cutoff uses
    the first velocity. *)
let test_sequence_picks_first _ =
  let v =
    Movement.Sequence
      [
        (Movement.Linear (1.0, fun _ -> 10.0), 5.0);
        (Movement.Linear (2.0, fun _ -> 20.0), 10.0);
      ]
  in
  let _, speed = Movement.eval_velocity v 2.0 0.0 0.0 in
  assert_bool "uses first" (almost_equal 10.0 speed)

(** Tests that a two-element [Sequence] at a time past the first cutoff switches
    to the second velocity. *)
let test_sequence_picks_second _ =
  let v =
    Movement.Sequence
      [
        (Movement.Linear (1.0, fun _ -> 10.0), 5.0);
        (Movement.Linear (2.0, fun _ -> 20.0), 10.0);
      ]
  in
  let _, speed = Movement.eval_velocity v 6.0 0.0 0.0 in
  assert_bool "uses second" (almost_equal 20.0 speed)

(** Tests that an empty [Sequence] returns angle 0.0 and speed 0.0. *)
let test_sequence_empty _ =
  let a, s = Movement.eval_velocity (Movement.Sequence []) 0.0 0.0 0.0 in
  assert_bool "zero angle" (almost_equal 0.0 a);
  assert_bool "zero speed" (almost_equal 0.0 s)

(** Tests that a [Sequence] whose time has exceeded every cutoff recursively
    falls through to the empty case and returns (0, 0). *)
let test_sequence_exhausted _ =
  let v =
    Movement.Sequence
      [
        (Movement.Linear (1.0, fun _ -> 10.0), 5.0);
        (Movement.Linear (2.0, fun _ -> 20.0), 10.0);
      ]
  in
  let a, s = Movement.eval_velocity v 11.0 0.0 0.0 in
  assert_bool "exhausted angle zero" (almost_equal 0.0 a);
  assert_bool "exhausted speed zero" (almost_equal 0.0 s)

(** Tests that a single-element [Sequence] at a time before its cutoff uses the
    element's velocity. *)
let test_sequence_single_active _ =
  let v = Movement.Sequence [ (Movement.Linear (0.0, fun _ -> 7.0), 10.0) ] in
  let _, speed = Movement.eval_velocity v 5.0 0.0 0.0 in
  assert_bool "single active speed" (almost_equal 7.0 speed)

(** Tests that a single-element [Sequence] past its cutoff recursively produces
    (0, 0). *)
let test_sequence_single_expired _ =
  let v = Movement.Sequence [ (Movement.Linear (0.0, fun _ -> 7.0), 10.0) ] in
  let a, s = Movement.eval_velocity v 15.0 0.0 0.0 in
  assert_bool "single expired angle zero" (almost_equal 0.0 a);
  assert_bool "single expired speed zero" (almost_equal 0.0 s)

(** Tests that [Custom] correctly negates its angle function and applies its
    speed function at a nonzero time. *)
let test_custom _ =
  let v = Movement.Custom ((fun t -> t), fun t -> t *. 2.0) in
  let angle, speed = Movement.eval_velocity v 3.0 0.0 0.0 in
  assert_bool "angle = -t" (almost_equal (-3.0) angle);
  assert_bool "speed = 2t" (almost_equal 6.0 speed)

(** Tests that [Custom] at time 0.0 returns angle 0.0 and speed 0.0 when both
    functions are the identity. *)
let test_custom_zero_time _ =
  let v = Movement.Custom ((fun t -> t), fun t -> t) in
  let angle, speed = Movement.eval_velocity v 0.0 0.0 0.0 in
  assert_bool "angle zero" (almost_equal 0.0 angle);
  assert_bool "speed zero" (almost_equal 0.0 speed)

(** Tests that [Custom] with a constant negative angle negates it to a positive
    value. *)
let test_custom_negative_angle _ =
  let v = Movement.Custom ((fun _ -> -2.0), fun _ -> 1.0) in
  let angle, _ = Movement.eval_velocity v 0.0 0.0 0.0 in
  assert_bool "negated negative angle" (almost_equal 2.0 angle)

(** Tests that [CustomDXDY] with a 3-4-5 right triangle yields speed 5.0. *)
let test_custom_dxdy _ =
  let v = Movement.CustomDXDY ((fun _ -> 3.0), fun _ -> 4.0) in
  let _, speed = Movement.eval_velocity v 0.0 0.0 0.0 in
  assert_bool "speed = 5" (almost_equal 5.0 speed)

(** Tests that [CustomDXDY] with a purely horizontal dx produces angle 0.0. *)
let test_custom_dxdy_angle_horizontal _ =
  let v = Movement.CustomDXDY ((fun _ -> 1.0), fun _ -> 0.0) in
  let angle, _ = Movement.eval_velocity v 0.0 0.0 0.0 in
  assert_bool "horizontal angle = 0" (almost_equal 0.0 angle)

(** Tests that [CustomDXDY] with negative dy produces a positive angle of pi/2
    and speed 1.0. *)
let test_custom_dxdy_neg_dy _ =
  let v = Movement.CustomDXDY ((fun _ -> 0.0), fun _ -> -1.0) in
  let angle, speed = Movement.eval_velocity v 0.0 0.0 0.0 in
  assert_bool "speed = 1" (almost_equal 1.0 speed);
  assert_bool "angle = pi/2" (almost_equal (Float.pi /. 2.0) angle)

(** Tests that [CustomDXDY] with zero dx and dy yields speed 0.0. *)
let test_custom_dxdy_zero _ =
  let v = Movement.CustomDXDY ((fun _ -> 0.0), fun _ -> 0.0) in
  let _, speed = Movement.eval_velocity v 0.0 0.0 0.0 in
  assert_bool "zero speed" (almost_equal 0.0 speed)

(** Tests that [Combo] of two [Linear] velocities at angle 0 sums their speeds.
*)
let test_combo _ =
  let v =
    Movement.Combo
      [
        Movement.Linear (0.0, fun _ -> 1.0); Movement.Linear (0.0, fun _ -> 2.0);
      ]
  in
  let _, speed = Movement.eval_velocity v 0.0 0.0 0.0 in
  assert_bool "speeds add up" (almost_equal 3.0 speed)

(** Tests that [Combo []] (the empty list) returns angle 0.0 and speed 0.0 via
    the fold's initial accumulator. *)
let test_combo_empty _ =
  let a, s = Movement.eval_velocity (Movement.Combo []) 0.0 0.0 0.0 in
  assert_bool "empty combo angle" (almost_equal 0.0 a);
  assert_bool "empty combo speed" (almost_equal 0.0 s)

(** Tests that [Combo] of three velocities at angle 0 sums all three speeds
    correctly. *)
let test_combo_three _ =
  let v =
    Movement.Combo
      [
        Movement.Linear (0.0, fun _ -> 1.0);
        Movement.Linear (0.0, fun _ -> 2.0);
        Movement.Linear (0.0, fun _ -> 3.0);
      ]
  in
  let _, speed = Movement.eval_velocity v 0.0 0.0 0.0 in
  assert_bool "three speeds add" (almost_equal 6.0 speed)

(** Tests that [Orbit] with zero radial speed still produces a non-negative
    speed (purely tangential motion). This case has dy = 0, exercising the dy
    epsilon branch. *)
let test_orbit _ =
  let v = Movement.Orbit ((0.0, 0.0), (fun _ -> 1.0), fun _ -> 0.0) in
  let _, speed = Movement.eval_velocity v 0.0 1.0 0.0 in
  assert_bool "orbit speed ok" (speed >= 0.0)

(** Tests that [Orbit] handles a zero dx by using the epsilon substitute without
    crashing, and still returns a non-negative speed. *)
let test_orbit_zero_dx _ =
  let v = Movement.Orbit ((0.0, 0.0), (fun _ -> 1.0), fun _ -> 1.0) in
  let _, speed = Movement.eval_velocity v 0.0 0.0 1.0 in
  assert_bool "orbit zero dx speed ok" (speed >= 0.0)

(** Tests that [Orbit] with both dx and dy nonzero (the normal, non-epsilon
    case) produces a strictly positive speed when angular velocity is nonzero.
*)
let test_orbit_nonzero_both _ =
  let v = Movement.Orbit ((0.0, 0.0), (fun _ -> 1.0), fun _ -> 1.0) in
  let _, speed = Movement.eval_velocity v 0.0 3.0 4.0 in
  assert_bool "orbit nonzero both speed > 0" (speed > 0.0)

(** Tests that [Orbit] with zero angular velocity but nonzero radial speed still
    produces a non-negative speed. *)
let test_orbit_zero_angular _ =
  let v = Movement.Orbit ((0.0, 0.0), (fun _ -> 0.0), fun _ -> 2.0) in
  let _, speed = Movement.eval_velocity v 0.0 1.0 1.0 in
  assert_bool "orbit zero angular speed >= 0" (speed >= 0.0)

(** Tests that [Point] returns the expected speed toward the target. *)
let test_point _ =
  let v = Movement.Point ((5.0, 5.0), fun _ -> 3.0) in
  let _, speed = Movement.eval_velocity v 0.0 0.0 0.0 in
  assert_bool "speed ok" (almost_equal 3.0 speed)

(** Tests that [Point] with a target directly to the right of the origin returns
    the correct speed. *)
let test_point_horizontal _ =
  let v = Movement.Point ((10.0, 0.0), fun _ -> 2.0) in
  let _, speed = Movement.eval_velocity v 0.0 0.0 0.0 in
  assert_bool "horizontal point speed" (almost_equal 2.0 speed)

(** Tests that [Point] with a speed function of 0.0 always yields zero speed
    regardless of target position. *)
let test_point_zero_speed _ =
  let v = Movement.Point ((3.0, 4.0), fun _ -> 0.0) in
  let _, speed = Movement.eval_velocity v 0.0 0.0 0.0 in
  assert_bool "point zero speed" (almost_equal 0.0 speed)

(** Tests that [string_of_velo] on a base (non-Combo) velocity returns a
    non-empty string. *)
let test_string_of_velo_base _ =
  let s =
    Movement.string_of_velo (Movement.Linear (0.0, fun _ -> 1.0)) 0.0 0.0 0.0
  in
  assert_bool "non-empty" (String.length s > 0)

(** Tests that [string_of_velo] on a single-element [Combo] delegates to the
    inner velocity and returns a non-empty string. *)
let test_string_of_velo_combo_single _ =
  let v = Movement.Combo [ Movement.Linear (0.0, fun _ -> 1.0) ] in
  let s = Movement.string_of_velo v 0.0 0.0 0.0 in
  assert_bool "non-empty" (String.length s > 0)

(** Tests that [string_of_velo] on a multi-element [Combo] wraps the result in
    the expected format and returns a non-empty string. *)
let test_string_of_velo_combo_multi _ =
  let v =
    Movement.Combo
      [
        Movement.Linear (0.0, fun _ -> 1.0); Movement.Linear (0.0, fun _ -> 2.0);
      ]
  in
  let s = Movement.string_of_velo v 0.0 0.0 0.0 in
  assert_bool "non-empty" (String.length s > 0)

let suite =
  "movement"
  >::: [
         "linear" >:: test_linear;
         "linear_zero_angle" >:: test_linear_zero;
         "linear_no_speed" >:: test_linear_no_speed;
         "linear_pi" >:: test_linear_pi;
         "linear_negative_angle" >:: test_linear_negative_angle;
         "linear_time_dependent_speed" >:: test_linear_time_dependent_speed;
         "sequence_first" >:: test_sequence_picks_first;
         "sequence_second" >:: test_sequence_picks_second;
         "sequence_empty" >:: test_sequence_empty;
         "sequence_exhausted" >:: test_sequence_exhausted;
         "sequence_single_active" >:: test_sequence_single_active;
         "sequence_single_expired" >:: test_sequence_single_expired;
         "custom" >:: test_custom;
         "custom_zero_time" >:: test_custom_zero_time;
         "custom_negative_angle" >:: test_custom_negative_angle;
         "custom_dxdy" >:: test_custom_dxdy;
         "custom_dxdy_angle_horizontal" >:: test_custom_dxdy_angle_horizontal;
         "custom_dxdy_neg_dy" >:: test_custom_dxdy_neg_dy;
         "custom_dxdy_zero" >:: test_custom_dxdy_zero;
         "combo" >:: test_combo;
         "combo_empty" >:: test_combo_empty;
         "combo_three" >:: test_combo_three;
         "orbit" >:: test_orbit;
         "orbit_zero_dx" >:: test_orbit_zero_dx;
         "orbit_nonzero_both" >:: test_orbit_nonzero_both;
         "orbit_zero_angular" >:: test_orbit_zero_angular;
         "point" >:: test_point;
         "point_horizontal" >:: test_point_horizontal;
         "point_zero_speed" >:: test_point_zero_speed;
         "string_of_velo_base" >:: test_string_of_velo_base;
         "string_of_velo_combo_single" >:: test_string_of_velo_combo_single;
         "string_of_velo_combo_multi" >:: test_string_of_velo_combo_multi;
       ]

let () = run_test_tt_main suite
