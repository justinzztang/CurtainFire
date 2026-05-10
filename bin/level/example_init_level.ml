module Player = Model.Player
module Bullet_Pattern = Model.Bullet_pattern
module Game_State = Model.Game_state
module Render_State = View.Render_state
open Model.Init_helper
open Raylib

let game_player =
  let appear_player =
    set_player_appearance (54, 65) "assets/dodonpachi_ship_c_hitbox.png"
      "assets/deltarune_explosion.png" 6 (0, 0)
  in
  set_player_state ~player:appear_player (640, 560) 3 3 Player.Linear 10. 5.

let enemy_list =
  let enemy1 =
    let appearance = set_enemy_appearance "assets/space_invader.png" (77, 56) in
    let pattern =
      Bullet_Pattern.(
        Timed
          ( Pulse
              ( Bullet
                  Bullet.(
                    create_bullet 0.0 0.0 Circle 10.
                      Movement.(
                        Orbit ((640., 290.), (fun t -> 0.05), fun t -> 0.05))
                      0.0),
                Float 1.0 ),
            Float 1.0,
            Float 1.9 ))
    in
    set_enemy_state ~enemy:appearance (640, 300) 3 1.0 999.
      (Linear (0.0, fun t -> 0.0))
      pattern
  in
  [ enemy1 ]

let texturelist =
  [
    "assets/ocaml-logo.png";
    "assets/red_th_bullet.png";
    "assets/dodonpachi_ship_c.png";
    "assets/dodonpachi_ship_c_hitbox.png";
    "assets/deltarune_explosion.png";
    "assets/space_invader.png";
  ]

let draw_start_screen () =
  Raylib.draw_text "3110 BULLET HELL" 200 150 40 Raylib.Color.green;
  Raylib.draw_text "HOW TO PLAY:" 200 250 20 Raylib.Color.white;
  Raylib.draw_text "WASD or Arrow Keys to Move" 200 280 20
    Raylib.Color.lightgray;
  Raylib.draw_text "Spacebar to Shoot" 200 310 20 Raylib.Color.lightgray;
  Raylib.draw_text "PRESS ENTER TO START" 200 400 20 Raylib.Color.yellow

let draw_death_screen () =
  Raylib.draw_text "GAME OVER" 200 400 40 Raylib.Color.red

let init_level () =
  ( 1280,
    720,
    "demo level",
    texturelist,
    initialize_state enemy_list game_player,
    draw_start_screen,
    draw_death_screen,
    (fun () -> ()),
    fun () -> () )
