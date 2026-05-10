open OUnit2
module Player = Model.Player

(** [make_player x y] creates a player at [(x, y)] with max_speed 10.0,
    focus_speed 5.0, hitbox_radius 5, zero hitbox offset, and default assets. *)
let make_player x y =
  Player.create_player x y 3 3 Player.Linear (50, 50)
    "assets/missing_texture.png" "assets/missing_texture.png" 5 (0, 0) 10. 5.

(** [pair_str (x, y)] is a human-readable string for an int pair. Used as a
    [~printer] for [assert_equal]. *)
let pair_str (x, y) = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

(** [almost_equal expected actual] is [true] when [expected] and [actual] differ
    by less than 0.001. Used for float field comparisons. *)
let almost_equal expected actual = Float.abs (expected -. actual) < 0.001

(** Tests that [create_player] stores x and y correctly. *)
let test_create_position _ =
  let p = make_player 42 99 in
  assert_equal ~printer:string_of_int 42 p.x;
  assert_equal ~printer:string_of_int 99 p.y

(** Tests that [create_player] stores [lives] correctly. *)
let test_create_lives _ =
  let p = make_player 0 0 in
  assert_equal ~printer:string_of_int 3 p.lives

(** Tests that [create_player] stores [bombs] correctly. *)
let test_create_bombs _ =
  let p = make_player 0 0 in
  assert_equal ~printer:string_of_int 3 p.bombs

(** Tests that [create_player] sets [is_shooting] to [false]. *)
let test_create_is_shooting _ =
  let p = make_player 0 0 in
  assert_equal ~printer:string_of_bool false p.is_shooting

(** Tests that [create_player] sets [last_hit] to -99.0. *)
let test_create_last_hit _ =
  let p = make_player 0 0 in
  assert_bool "last_hit = -99" (almost_equal (-99.0) p.last_hit)

(** Tests that [create_player] stores [hitbox_radius] correctly. *)
let test_create_hitbox_radius _ =
  let p = make_player 0 0 in
  assert_equal ~printer:string_of_int 5 p.hitbox_radius

(** Tests that [create_player] stores [max_speed] correctly. *)
let test_create_max_speed _ =
  let p = make_player 0 0 in
  assert_bool "max_speed = 10" (almost_equal 10.0 p.max_speed)

(** Tests that [create_player] stores [focus_speed] correctly. *)
let test_create_focus_speed _ =
  let p = make_player 0 0 in
  assert_bool "focus_speed = 5" (almost_equal 5.0 p.focus_speed)

(** Tests that [E] increases x. *)
let test_east _ =
  let p = make_player 100 100 in
  assert_bool "x increases"
    ((Player.move_player p Player.E false 1000 1000).x > 100)

(** Tests that [W] decreases x. *)
let test_west _ =
  let p = make_player 100 100 in
  assert_bool "x decreases"
    ((Player.move_player p Player.W false 1000 1000).x < 100)

(** Tests that [N] increases y. *)
let test_north _ =
  let p = make_player 100 100 in
  assert_bool "y increases"
    ((Player.move_player p Player.N false 1000 1000).y > 100)

(** Tests that [S] decreases y. *)
let test_south _ =
  let p = make_player 100 100 in
  assert_bool "y decreases"
    ((Player.move_player p Player.S false 1000 1000).y < 100)

(** Tests that [NE] increases both x and y. *)
let test_ne _ =
  let p = make_player 100 100 in
  let moved = Player.move_player p Player.NE false 1000 1000 in
  assert_bool "x up" (moved.x > 100);
  assert_bool "y up" (moved.y > 100)

(** Tests that [SE] increases x and decreases y. *)
let test_se _ =
  let p = make_player 100 100 in
  let moved = Player.move_player p Player.SE false 1000 1000 in
  assert_bool "x up" (moved.x > 100);
  assert_bool "y down" (moved.y < 100)

(** Tests that [SW] decreases both x and y. *)
let test_sw _ =
  let p = make_player 100 100 in
  let moved = Player.move_player p Player.SW false 1000 1000 in
  assert_bool "x down" (moved.x < 100);
  assert_bool "y down" (moved.y < 100)

(** Tests that [NW] decreases x and increases y. *)
let test_nw _ =
  let p = make_player 100 100 in
  let moved = Player.move_player p Player.NW false 1000 1000 in
  assert_bool "x down" (moved.x < 100);
  assert_bool "y up" (moved.y > 100)

(** Tests that [NO_DIR] leaves position unchanged. *)
let test_no_dir _ =
  let p = make_player 100 100 in
  let moved = Player.move_player p Player.NO_DIR false 1000 1000 in
  assert_equal ~printer:string_of_int 100 moved.x;
  assert_equal ~printer:string_of_int 100 moved.y

(** Tests that [E] at full speed displaces x by exactly 10 (max_speed = 10.0, dx
    = 1.0, int_of_float(10.0) = 10). *)
let test_east_exact _ =
  let p = make_player 100 100 in
  let moved = Player.move_player p Player.E false 1000 1000 in
  assert_equal ~printer:string_of_int 110 moved.x

(** Tests that [N] at full speed displaces y by exactly 10. *)
let test_north_exact _ =
  let p = make_player 100 100 in
  let moved = Player.move_player p Player.N false 1000 1000 in
  assert_equal ~printer:string_of_int 110 moved.y

(** Tests that [E] in focus mode displaces x by exactly 5 (focus_speed = 5.0).
*)
let test_east_focus_exact _ =
  let p = make_player 100 100 in
  let moved = Player.move_player p Player.E true 1000 1000 in
  assert_equal ~printer:string_of_int 105 moved.x

(** Tests that [NO_DIR] in focus mode also leaves position unchanged. *)
let test_no_dir_focus _ =
  let p = make_player 100 100 in
  let moved = Player.move_player p Player.NO_DIR true 1000 1000 in
  assert_equal ~printer:string_of_int 100 moved.x;
  assert_equal ~printer:string_of_int 100 moved.y

(** Tests that focus mode produces a smaller x displacement than normal mode
    when moving [E]. *)
let test_focus_is_slower _ =
  let p = make_player 100 100 in
  let normal = Player.move_player p Player.E false 1000 1000 in
  let focused = Player.move_player p Player.E true 1000 1000 in
  assert_bool "focus slower" (focused.x < normal.x)

(** Tests that focus mode produces a smaller y displacement than normal mode
    when moving [N]. *)
let test_focus_slower_vertical _ =
  let p = make_player 100 100 in
  let normal = Player.move_player p Player.N false 1000 1000 in
  let focused = Player.move_player p Player.N true 1000 1000 in
  assert_bool "focus slower vertical" (focused.y < normal.y)

(** Tests that moving [NE] from near the window corner clamps both axes to the
    window bounds. *)
let test_clamp_max _ =
  let p = make_player 999 999 in
  let moved = Player.move_player p Player.NE false 1000 1000 in
  assert_bool "x clamped" (moved.x <= 1000);
  assert_bool "y clamped" (moved.y <= 1000)

(** Tests that moving [SW] from the origin clamps both axes to 0. *)
let test_clamp_min _ =
  let p = make_player 0 0 in
  let moved = Player.move_player p Player.SW false 1000 1000 in
  assert_bool "x not negative" (moved.x >= 0);
  assert_bool "y not negative" (moved.y >= 0)

(** Tests that x is clamped to [window_x] when moving [E] from near the right
    edge, while y is unaffected. *)
let test_clamp_x_at_boundary _ =
  let p = make_player 995 500 in
  let moved = Player.move_player p Player.E false 1000 1000 in
  assert_equal ~printer:string_of_int 1000 moved.x;
  assert_equal ~printer:string_of_int 500 moved.y

(** Tests that y is clamped to [window_y] when moving [N] from near the top
    edge, while x is unaffected. *)
let test_clamp_y_at_boundary _ =
  let p = make_player 500 995 in
  let moved = Player.move_player p Player.N false 1000 1000 in
  assert_equal ~printer:string_of_int 500 moved.x;
  assert_equal ~printer:string_of_int 1000 moved.y

(** Tests that x is clamped to 0 when moving [W] from x=0. *)
let test_clamp_x_min _ =
  let p = make_player 0 500 in
  let moved = Player.move_player p Player.W false 1000 1000 in
  assert_equal ~printer:string_of_int 0 moved.x;
  assert_equal ~printer:string_of_int 500 moved.y

(** Tests that y is clamped to 0 when moving [S] from y=0. *)
let test_clamp_y_min _ =
  let p = make_player 500 0 in
  let moved = Player.move_player p Player.S false 1000 1000 in
  assert_equal ~printer:string_of_int 500 moved.x;
  assert_equal ~printer:string_of_int 0 moved.y

(** Tests that [move_player] does not alter [lives]. *)
let test_move_preserves_lives _ =
  let p = make_player 100 100 in
  let moved = Player.move_player p Player.E false 1000 1000 in
  assert_equal ~printer:string_of_int 3 moved.lives

(** Tests that [move_player] does not alter [bombs]. *)
let test_move_preserves_bombs _ =
  let p = make_player 100 100 in
  let moved = Player.move_player p Player.E false 1000 1000 in
  assert_equal ~printer:string_of_int 3 moved.bombs

(** Tests that [move_player] does not alter [hitbox_radius]. *)
let test_move_preserves_hitbox_radius _ =
  let p = make_player 100 100 in
  let moved = Player.move_player p Player.E false 1000 1000 in
  assert_equal ~printer:string_of_int 5 moved.hitbox_radius

(** Tests that [get_position] returns the player's (x, y) coordinates. *)
let test_get_position _ =
  let p = make_player 42 77 in
  assert_equal ~printer:pair_str (42, 77) (Player.get_position p)

(** Tests that [get_position] reflects coordinates after a move. *)
let test_get_position_after_move _ =
  let p = make_player 100 100 in
  let moved = Player.move_player p Player.E false 1000 1000 in
  assert_equal ~printer:pair_str (moved.x, moved.y) (Player.get_position moved)

(** Tests that [get_hitbox_position] adds the hitbox offset to the player
    position. *)
let test_hitbox_offset _ =
  let p =
    Player.create_player 100 200 3 3 Player.Linear (50, 50)
      "assets/missing_texture.png" "assets/missing_texture.png" 5 (10, 20) 10.
      5.
  in
  assert_equal ~printer:pair_str (110, 220) (Player.get_hitbox_position p)

(** Tests that [get_hitbox_position] with zero offset returns the player
    position unchanged. *)
let test_hitbox_no_offset _ =
  let p = make_player 50 75 in
  assert_equal ~printer:pair_str (50, 75) (Player.get_hitbox_position p)

(** Tests that [get_hitbox_position] is correct after a move. *)
let test_hitbox_after_move _ =
  let p = make_player 100 100 in
  let moved = Player.move_player p Player.E false 1000 1000 in
  assert_equal ~printer:pair_str (moved.x, moved.y)
    (Player.get_hitbox_position moved)

(** Tests that [get_hitbox_position] correctly handles a non-zero hitbox offset
    after a move. *)
let test_hitbox_offset_after_move _ =
  let p =
    Player.create_player 100 100 3 3 Player.Linear (50, 50)
      "assets/missing_texture.png" "assets/missing_texture.png" 5 (5, 5) 10. 5.
  in
  let moved = Player.move_player p Player.E false 1000 1000 in
  assert_equal ~printer:pair_str
    (moved.x + 5, moved.y + 5)
    (Player.get_hitbox_position moved)

let suite =
  "player"
  >::: [
         "create_position" >:: test_create_position;
         "create_lives" >:: test_create_lives;
         "create_bombs" >:: test_create_bombs;
         "create_is_shooting" >:: test_create_is_shooting;
         "create_last_hit" >:: test_create_last_hit;
         "create_hitbox_radius" >:: test_create_hitbox_radius;
         "create_max_speed" >:: test_create_max_speed;
         "create_focus_speed" >:: test_create_focus_speed;
         "east" >:: test_east;
         "west" >:: test_west;
         "north" >:: test_north;
         "south" >:: test_south;
         "ne" >:: test_ne;
         "se" >:: test_se;
         "sw" >:: test_sw;
         "nw" >:: test_nw;
         "no_dir" >:: test_no_dir;
         "east_exact" >:: test_east_exact;
         "north_exact" >:: test_north_exact;
         "east_focus_exact" >:: test_east_focus_exact;
         "no_dir_focus" >:: test_no_dir_focus;
         "focus_slower" >:: test_focus_is_slower;
         "focus_slower_vertical" >:: test_focus_slower_vertical;
         "clamp_max" >:: test_clamp_max;
         "clamp_min" >:: test_clamp_min;
         "clamp_x_at_boundary" >:: test_clamp_x_at_boundary;
         "clamp_y_at_boundary" >:: test_clamp_y_at_boundary;
         "clamp_x_min" >:: test_clamp_x_min;
         "clamp_y_min" >:: test_clamp_y_min;
         "move_preserves_lives" >:: test_move_preserves_lives;
         "move_preserves_bombs" >:: test_move_preserves_bombs;
         "move_preserves_hitbox_radius" >:: test_move_preserves_hitbox_radius;
         "get_position" >:: test_get_position;
         "get_position_after_move" >:: test_get_position_after_move;
         "hitbox_offset" >:: test_hitbox_offset;
         "hitbox_no_offset" >:: test_hitbox_no_offset;
         "hitbox_after_move" >:: test_hitbox_after_move;
         "hitbox_offset_after_move" >:: test_hitbox_offset_after_move;
       ]

let () = run_test_tt_main suite
