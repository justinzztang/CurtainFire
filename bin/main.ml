module Game_State = Model.Game_state
module Player = Model.Player
module Handle_Input = Controller.Handle_input
module Render_State = View.Render_state
module Movement = Model.Movement
module EIL = Level.Example_init_level
module DS = Level.Demo_stage
open Raylib
open Unix
open Mtime

let main_loop () =
  let ( windowx,
        windowy,
        windowmessage,
        texturelist,
        istate,
        startscreen,
        deathscreen,
        endscreen,
        background ) =
    DS.init_level ()
    (*the init helper goes here*)
  in
  let query_time () = Mtime.Span.to_float_ns (Mtime_clock.elapsed ()) /. 1e9 in
  let initial_time = ref 0.0 in
  let initial_state = istate in
  Raylib.init_window windowx windowy windowmessage;
  Raylib.set_target_fps 60;

  Raylib.init_audio_device ();
  let bgm = Raylib.load_music_stream "assets/rins-theme_loop.ogg" in
  let snd_shoot = Raylib.load_sound "assets/shoot.wav" in
  let snd_hit = Raylib.load_sound "assets/hit.wav" in
  let snd_enemy_die = Raylib.load_sound "assets/enemy_die.wav" in
  Raylib.set_sound_volume snd_enemy_die 0.2;

  Render_State.initialize_textures texturelist;
  Render_State.set_start_screen startscreen;
  Render_State.set_death_screen deathscreen;
  Render_State.set_end_screen endscreen;
  Render_State.set_background background;

  (* just keep this at 60 always *)
  (* this recursive loop completely replaces the imperative while loop. we pass
     the current state in, calculate updates, render the frame, and recursively
     call it again with the new state *)
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
            Raylib.play_music_stream bgm;
            game_loop Game_State.{ state with phase = Playing }
          end
          else game_loop state
      | Game_State.Playing ->
          Raylib.update_music_stream bgm;
          let current_time = query_time () -. !initial_time in
          let prev_bullet_count =
            BatDllist.length state.Game_State.player_bullets
          in
          let moved_state =
            Handle_Input.handle_movement_input state current_time windowx
              windowy
          in
          let updated_state =
            Game_State.update_state current_time moved_state
          in
          let new_bullet_count =
            BatDllist.length updated_state.Game_State.player_bullets
          in
          if new_bullet_count > prev_bullet_count then
            Raylib.play_sound snd_shoot;
          if !Game_State.enemies_killed_this_frame > 0 then
            Raylib.play_sound snd_enemy_die;
          let collision_checked =
            if
              Game_State.detect_collision updated_state
              || Game_State.detect_enemy_body_collision updated_state
            then
              if updated_state.player.lives <= 1 then begin
                Raylib.play_sound snd_hit;
                Game_State.
                  {
                    updated_state with
                    phase = GameOver;
                    player =
                      {
                        updated_state.player with
                        sprite_filename = "assets/deltarune_explosion.png";
                      };
                  }
              end
              else begin
                Raylib.play_sound snd_hit;
                {
                  updated_state with
                  enemy_bullets =
                    BatDllist.of_list
                      [
                        Game_State.(
                          Bullet.create_bullet 65537. 65537. Anchor
                            Float.infinity
                            (Linear (0.0, fun t -> 0.0))
                            0.0);
                      ];
                  player =
                    {
                      updated_state.player with
                      lives = updated_state.player.lives - 1;
                      last_hit = current_time;
                    };
                }
              end
            else updated_state
          in

          Render_State.render_state collision_checked;
          (* recurse *)
          game_loop collision_checked
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
