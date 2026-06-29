module Game_State = Model.Game_state
module Player = Model.Player
module Bullet = Model.Bullet
module Obj = Model.Object
module Parser = Parse.Parser
module Tokenizer = Parse.Tokenizer
module Util = Parse.Util
open Raylib

type direction =
  | NO_DIR
  | N
  | E
  | S
  | W
  | NE
  | SE
  | SW
  | NW

(** Creates missing tables instead of erroring *)
let hashtbl_safe_find tbl tag =
  if Hashtbl.mem tbl tag then Hashtbl.find tbl tag
  else
    let () = Hashtbl.add tbl tag (Dynarray.create ()) in
    Hashtbl.find tbl tag

let last_main_shot = ref (-10)
let last_homing_shot = ref (-10)

let homing_behavior =
  "While (%ALIVE_ENEMIES_COUNT > 0) Point (%CLOSEST_ENEMY_X , \
   %CLOSEST_ENEMY_Y) 30 until false;;"

let homing_behavior_parsed =
  Parser.parse_behavior
    (ref (Tokenizer.tokenize homing_behavior))
    Util.{ values = StringMap.empty; next = None }

let handle_movement_input (state : Game_State.t) current_frame =
  let dx =
    if is_key_down Key.Right then 1
    else 0 - if is_key_down Key.Left then 1 else 0
  in
  let dy =
    if is_key_down Key.Up then 1 else 0 - if is_key_down Key.Down then 1 else 0
  in
  let dir =
    match (dx, dy) with
    | 0, 0 -> NO_DIR
    | 0, 1 -> N
    | 1, 0 -> E
    | 0, -1 -> S
    | -1, 0 -> W
    | 1, 1 -> NE
    | 1, -1 -> SE
    | -1, -1 -> SW
    | -1, 1 -> NW
    | _ -> assert false
  in
  let moved_player =
    let dx, dy =
      match dir with
      | NO_DIR -> (0., 0.)
      | N -> (0., 1.)
      | E -> (1., 0.)
      | S -> (0., -1.)
      | W -> (-1., 0.)
      (*1/sqrt2*)
      | NE -> (0.70710678, 0.70710678)
      | SE -> (0.70710678, -0.70710678)
      | SW -> (-0.70710678, -0.70710678)
      | NW -> (-0.70710678, 0.70710678)
    in
    let speed =
      if is_key_down Key.Left_shift then state.player.focus_speed
      else state.player.max_speed
    in
    {
      state.player with
      x =
        min
          (float_of_int state.window_x)
          (max 0. (state.player.x +. (dx *. speed)));
      y =
        min
          (float_of_int state.window_y)
          (max 0. (state.player.y +. (dy *. speed)));
      focus = is_key_down Key.Left_shift;
    }
  in
  if is_key_down Key.Z then (
    (*every 3 frames shoot main, every 5 frames shoot homing*)
    if !last_main_shot <= current_frame - 3 then (
      last_main_shot := current_frame;
      (*create 2 straight main bullets*)
      let b1 =
        Bullet.(
          create_bullet "" (state.player.x -. 13.) state.player.y 25.
            (Float.pi /. 2.) current_frame 120 Red Circle Nothing 5. true 1.0
            None
            Util.{ values = StringMap.empty; next = None })
      in
      let b2 =
        Bullet.(
          create_bullet "" (state.player.x +. 13.) state.player.y 25.
            (Float.pi /. 2.) current_frame 120 Red Circle Nothing 5. true 1.0
            None
            Util.{ values = StringMap.empty; next = None })
      in
      Dynarray.add_last (hashtbl_safe_find state.player_bullets "") b1;
      Dynarray.add_last (hashtbl_safe_find state.player_bullets "") b2);
    if !last_homing_shot <= current_frame - 5 then (
      last_homing_shot := current_frame;
      (*create 4 homing bullets*)
      let b1 =
        Bullet.(
          create_bullet "" (state.player.x -. 50.) state.player.y 15.
            (Float.pi /. 2.) current_frame 120 Blue Circle
            homing_behavior_parsed 5. true 1.0 None
            Util.{ values = StringMap.empty; next = None })
      in
      let b2 =
        Bullet.(
          create_bullet "" (state.player.x -. 25.) (state.player.y -. 25.) 15.
            (Float.pi /. 2.) current_frame 120 Blue Circle
            homing_behavior_parsed 5. true 1.0 None
            Util.{ values = StringMap.empty; next = None })
      in
      let b3 =
        Bullet.(
          create_bullet "" (state.player.x +. 25.) (state.player.y -. 25.) 15.
            (Float.pi /. 2.) current_frame 120 Blue Circle
            homing_behavior_parsed 5. true 1.0 None
            Util.{ values = StringMap.empty; next = None })
      in
      let b4 =
        Bullet.(
          create_bullet "" (state.player.x +. 50.) state.player.y 15.
            (Float.pi /. 2.) current_frame 120 Blue Circle
            homing_behavior_parsed 5. true 1.0 None
            Util.{ values = StringMap.empty; next = None })
      in
      Dynarray.add_last (hashtbl_safe_find state.player_bullets "") b1;
      Dynarray.add_last (hashtbl_safe_find state.player_bullets "") b2;
      Dynarray.add_last (hashtbl_safe_find state.player_bullets "") b3;
      Dynarray.add_last (hashtbl_safe_find state.player_bullets "") b4));
  { state with player = moved_player }
