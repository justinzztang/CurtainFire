module Game_State = Model.Game_state
module Player = Model.Player
module Handle_Input = Controller.Handle_input
module Render_State = View.Render_state
module Object = Model.Object
module Ast = Parse.Ast
module Util = Parse.Util
module Tokenizer = Parse.Tokenizer
module Parser = Parse.Parser
module Core = Core
open Raylib
open Unix
open Mtime

let filename = ref None
let () = if Array.length Sys.argv > 1 then filename := Some Sys.argv.(1)

let main_loop () =
  let pattern_string =
    if !filename = None then (
      print_endline "Write spawn pattern:";
      Util.read_console ())
    else
      let fn = Option.get !filename in
      List.nth (Str.split (Str.regexp "<end>") Core.(In_channel.read_all fn)) 0
  in
  let behavior_string =
    if !filename = None then (
      print_endline "Write enemy behavior:";
      Util.read_console ())
    else
      let fn = Option.get !filename in
      List.nth (Str.split (Str.regexp "<end>") Core.(In_channel.read_all fn)) 1
  in

  (*maybe ill clean this up one day*)
  let en_x =
    if
      !filename = None
      || List.length
           (Str.split (Str.regexp "<end>")
              Core.(In_channel.read_all (Stdlib.Option.get !filename)))
         < 3
    then (
      print_string "Enemy spawn X: ";
      float_of_string (read_line ()))
    else
      let fn = Option.get !filename in
      float_of_string
        (String.trim
           (List.nth
              (Str.split (Str.regexp "<end>") Core.(In_channel.read_all fn))
              2))
  in
  let en_y =
    if
      !filename = None
      || List.length
           (Str.split (Str.regexp "<end>")
              Core.(In_channel.read_all (Stdlib.Option.get !filename)))
         < 3
    then (
      print_string "Enemy spawn Y: ";
      float_of_string (read_line ()))
    else
      let fn = Option.get !filename in
      float_of_string
        (String.trim
           (List.nth
              (Str.split (Str.regexp "<end>") Core.(In_channel.read_all fn))
              3))
  in
  let en_hp =
    if
      !filename = None
      || List.length
           (Str.split (Str.regexp "<end>")
              Core.(In_channel.read_all (Stdlib.Option.get !filename)))
         < 3
    then (
      print_string "Enemy HP: ";
      int_of_float (float_of_string (read_line ())))
    else
      let fn = Option.get !filename in
      int_of_float
        (float_of_string
           (String.trim
              (List.nth
                 (Str.split (Str.regexp "<end>") Core.(In_channel.read_all fn))
                 4)))
  in

  let startscreen () =
    Raylib.draw_text "Press Enter to begin" 70 150 40 Raylib.Color.green
  in
  let deathscreen () =
    Raylib.draw_text "how did you end up here?" 70 150 40 Raylib.Color.green
  in
  let endscreen () =
    Raylib.draw_text "how did you end up here?" 70 150 40 Raylib.Color.green
  in
  let draw_background () =
    Raylib.draw_texture_ex
      (Render_State.render_loaded_texture "assets/generic_space.png")
      (Raylib.Vector2.create 0. 0.)
      0. 0.5 Color.white
  in
  let pattern_env = Util.{ values = StringMap.empty; next = None } in
  let in_pattern =
    Parser.parse_spawn_pattern
      (ref (Tokenizer.tokenize pattern_string))
      Util.{ values = StringMap.empty; next = Some pattern_env }
  in
  let behavior_env = Util.{ values = StringMap.empty; next = None } in
  let in_behavior =
    Parser.parse_behavior
      (ref (Tokenizer.tokenize behavior_string))
      Util.{ values = StringMap.empty; next = Some behavior_env }
  in

  let istate =
    Game_State.
      {
        phase = StartScreen;
        player_bullets = Hashtbl.create 1;
        enemy_bullets = Hashtbl.create 1;
        active_enemies = Hashtbl.create 1;
        queued_enemies =
          [
            Enemy.create_enemy "" en_x en_y 0. 0. 0 59940 true 1.0 None en_hp
              in_behavior in_pattern "assets/lowedgelowresfairy.png" (46, 49);
          ];
        player =
          Player.create_player 430. 200. 99 3 Straight (49, 90)
            "assets/lowreslow_edge_reimu.png"
            "assets/lowreslow_edge_reimu_hitbox.png" 3. (0., 4.) 6. 3.;
        elapsed_frames = 0;
        window_x = 860;
        window_y = 1000;
        debug_flag = not (Array.length Sys.argv > 2 && Sys.argv.(2) = "DEATH_ON");
      }
  in

  let initlvl =
    ( 860,
      1000,
      "CurtainFire Demo",
      [
        "assets/generic_space.png";
        "assets/lowedgelowresfairy.png";
        "assets/lowreslow_edge_reimu.png";
        "assets/lowreslow_edge_reimu_hitbox.png";
      ],
      istate,
      startscreen,
      deathscreen,
      endscreen,
      draw_background )
  in
  let ( windowx,
        windowy,
        windowmessage,
        texturelist,
        istate,
        startscreen,
        deathscreen,
        endscreen,
        background ) =
    initlvl
  in
  let query_time () = Mtime.Span.to_float_ns (Mtime_clock.elapsed ()) /. 1e9 in
  let initial_time = ref 0.0 in
  let initial_state = istate in
  Raylib.init_window windowx windowy windowmessage;
  Raylib.set_window_size windowx windowy;
  Raylib.set_target_fps 60;

  Render_State.initialize_textures texturelist;
  Render_State.set_start_screen startscreen;
  Render_State.set_death_screen deathscreen;
  Render_State.set_end_screen endscreen;
  Render_State.set_background background;

  (* just keep this at 60 always *)
  (* this recursive loop completely replaces the imperative while loop. we pass
     the current state in, calculate updates, render the frame, and recursively
     call it again with the new state *)
  let frame_counter = ref 0 in
  let rec game_loop state =
    if window_should_close () then close_window ()
    else
      match state.Game_State.phase with
      | Game_State.StartScreen ->
          (* Draw menu *)
          Render_State.render_state state;

          (* Wait for the user to press Enter to start *)
          if Raylib.is_key_pressed Raylib.Key.Enter then begin
            initial_time := query_time () -. 0.;
            game_loop Game_State.{ state with phase = Playing }
          end
          else game_loop state
      | Game_State.Playing ->
          let start_time = query_time () -. !initial_time in
          let updated_state =
            Game_State.update_state (state.elapsed_frames + 1) state
          in
          let moved_state =
            Handle_Input.handle_movement_input updated_state
              (state.elapsed_frames + 1)
          in
          Render_State.render_state moved_state;
          let end_time = query_time () -. !initial_time in
          if state.elapsed_frames mod 60 = 0 then
            print_endline
              ("bullets: "
              ^ string_of_int
                  (Game_State.count_hashtable_stuff state.enemy_bullets)
              ^ ", frame: "
              ^ string_of_int !frame_counter
              ^ ", frametime = "
              ^ string_of_float (end_time -. start_time));
          (*print_endline "new frame";*)
          game_loop moved_state
      | Game_State.GameOver | Game_State.LevelEnd ->
          (* Draw the game over screen *)
          Render_State.render_state state;

          (* Loop endlessly until the window is closed *)
          game_loop state
  in

  (* start recursion with initial state *)
  game_loop initial_state

let () =
  try main_loop ()
  with exn ->
    prerr_endline ("error: " ^ Printexc.to_string exn);
    exit 1
