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
  set_player_state ~player:appear_player (288, 650) 3 3 Player.Cone 10. 5.

let enemy_list =
  let ufo_appearance =
    set_enemy_appearance "assets/demo_level/ufo.png" (30, 30)
  in
  let orb_turret_appearance =
    set_enemy_appearance "assets/demo_level/orb_turret.png" (30, 38)
  in
  let intro1 i =
    set_enemy_state ~enemy:ufo_appearance (-100, 100) 3
      (0.0 +. (float_of_int i *. 0.1))
      30.
      (Sequence
         [
           (Linear (-0.523, fun t -> 5.0), 1.0);
           ( Custom
               ((fun t -> -0.523 +. (5.7596 /. 2. *. (t -. 1.0))), fun t -> 5.0),
             3.0 );
           (Linear (-1.0466, fun t -> 5.0), 999.0);
         ])
      Bullet_Pattern.(Nothing)
  in
  let intro2 i =
    set_enemy_state ~enemy:ufo_appearance (676, 100) 3
      (2.0 +. (float_of_int i *. 0.1))
      30.
      (Sequence
         [
           (Linear (-2.6186, fun t -> 5.0), 1.0);
           ( Custom
               ((fun t -> -2.6186 -. (5.7596 /. 2. *. (t -. 1.0))), fun t -> 5.0),
             3.0 );
           (Linear (-2.095, fun t -> 5.0), 999.0);
         ])
      Bullet_Pattern.(Nothing)
  in

  let intro3 i =
    set_enemy_state ~enemy:ufo_appearance (-100, 200) 3
      (4.0 +. (float_of_int i *. 0.1))
      30.
      (CustomDXDY ((fun t -> 5.0), fun t -> 6.0 -. (t *. 8.)))
      Bullet_Pattern.(Nothing)
  in
  let intro4 i =
    set_enemy_state ~enemy:ufo_appearance (676, 200) 3
      (6.0 +. (float_of_int i *. 0.1))
      30.
      (CustomDXDY ((fun t -> -5.0), fun t -> 6.0 -. (t *. 8.)))
      Bullet_Pattern.(Nothing)
  in

  let rapid_aimed t1 t2 =
    Bullet_Pattern.(
      Timed
        ( Pulse
            ( Aimed
                (Bullet
                   Bullet.(
                     create_bullet 0.0 0.0 Circle 10.
                       Movement.(Linear (0.0, fun t -> 10. +. (t *. 0.5)))
                       0.0)),
              Float 0.05 ),
          Float t1,
          Float t2 ))
  in
  let mid_aimed x y st =
    set_enemy_state ~enemy:orb_turret_appearance (x, y) 15 st 30.
      (Sequence
         [
           (CustomDXDY ((fun t -> 0.), fun t -> min (-6. +. (t *. 6.)) 0.), 1.1);
           (CustomDXDY ((fun t -> 0.), fun t -> 0.), 3.0);
           (CustomDXDY ((fun t -> 0.), fun t -> (t -. 3.0) *. 6.), 7.0);
         ])
      (rapid_aimed 1.5 2.2)
  in
  let aimed_circle t1 t2 =
    Bullet_Pattern.(
      Timed
        ( Pulse
            ( Aimed
                (Arc
                   ( Bullet
                       Bullet.(
                         create_bullet 0.0 0.0 Circle 10.
                           Movement.(Linear (0.0, fun t -> 5.))
                           0.0),
                     Int 16,
                     Float (Float.pi *. 2.) )),
              Float 1.0 ),
          Float t1,
          Float t2 ))
  in
  let mid_circle x y st dir =
    set_enemy_state ~enemy:ufo_appearance (x, y) 15 st 20.
      (Sequence
         [
           ( Custom
               ( (fun t -> -1.571 -. (t *. 0.5 *. dir)),
                 fun t -> 3. -. (1.5 *. t) ),
             2.0 );
           (CustomDXDY ((fun t -> 0.), fun t -> 0.), 4.0);
           ( Custom
               ( (fun t -> -1.571 -. ((t -. 2.) *. 0.5 *. dir)),
                 fun t -> 1.5 *. (t -. 4.) ),
             10.0 );
         ])
      (aimed_circle 1.0 5.0)
  in
  let mid_ufos i =
    set_enemy_state ~enemy:ufo_appearance (-100, 200) 3
      (24.0 +. (float_of_int i *. 0.1))
      10.
      (Sequence
         [
           (CustomDXDY ((fun t -> 5.), fun t -> 0.), 1.3);
           ( Custom ((fun t -> -.(t -. 1.3) *. 2. *. Float.pi), fun t -> 5.0),
             2.3 );
           (CustomDXDY ((fun t -> 5.), fun t -> 0.), 10.);
         ])
      Bullet_Pattern.(Nothing)
  in
  let midboss_appearance =
    set_enemy_appearance "assets/demo_level/boss_skull.png" (51, 60)
  in
  let midboss_pattern_11 st =
    Bullet_Pattern.(
      Sequence
        ((Nothing, st)
        :: List.init 50 (fun i ->
            ( Pulse
                ( Angled
                    ( Arc
                        ( Bullet
                            Bullet.(
                              create_bullet 0.0
                                (float_of_int i *. 5.)
                                Circle 10.
                                Movement.(
                                  Linear
                                    (0.0, fun t -> max (3. -. (t *. 0.25)) 1.5))
                                0.0),
                          Int 3,
                          Float (2. *. Float.pi) ),
                      RandFloat (0., 2. *. Float.pi) ),
                  Float 0.03 ),
              st +. (0.03 *. float_of_int (i + 1)) ))))
  in
  let midboss_pattern_12 st =
    Bullet_Pattern.(
      Timed
        ( Pulse
            ( Arc
                ( Bullet
                    Bullet.(
                      create_bullet 0.0 0.0 Circle 10.
                        Movement.(Linear (0.0, fun t -> 3.))
                        0.0),
                  Int 64,
                  Float (Float.pi *. 2.) ),
              Float 0.1 ),
          Float st,
          Float (st +. 0.11) ))
  in
  let extra_midboss_1 =
    Bullet_Pattern.(
      Sequence
        (List.flatten
           (List.init 3 (fun i ->
                [
                  ( midboss_pattern_11 (10. +. (float_of_int i *. 4.)),
                    12. +. (float_of_int i *. 4.) );
                  ( midboss_pattern_12 (12. +. (float_of_int i *. 4.)),
                    14. +. (float_of_int i *. 4.) );
                ]))))
  in
  let midboss_pattern_2 =
    Bullet_Pattern.(
      Combo
        [
          Pulse
            ( Arc
                ( Bullet
                    Bullet.(
                      create_bullet 0.0 0.0 Circle 10.
                        Movement.(Linear (0.0, fun t -> 2.))
                        0.0),
                  Int 32,
                  Float (Float.pi *. 2.) ),
              Float 2.0 );
          Pulse
            ( Combo
                [
                  Spin
                    ( Arc
                        ( Bullet
                            Bullet.(
                              create_bullet 0.0 0.0 Circle 10.
                                Movement.(Linear (0.0, fun t -> 3.))
                                0.0),
                          Int 6,
                          Float (Float.pi *. 2.) ),
                      Float 0.4 );
                  Spin
                    ( Arc
                        ( Bullet
                            Bullet.(
                              create_bullet 0.0 0.0 Circle 10.
                                Movement.(Linear (0.0, fun t -> 3.))
                                0.0),
                          Int 6,
                          Float (Float.pi *. 2.) ),
                      Float (-0.4) );
                ],
              Float 0.2 );
        ])
  in
  let midboss =
    set_enemy_state ~enemy:midboss_appearance (576, -50) 1000 35. 999.
      (Sequence
         [
           (CustomDXDY ((fun t -> -5.), fun t -> -3.), 0.96);
           (CustomDXDY ((fun t -> 0.), fun t -> 0.), 6.);
           ( CustomDXDY
               ( (fun t -> 3. *. cos (t -. 6.)),
                 fun t -> 3. *. ((cos (t -. 6.) ** 2.) -. (sin (t -. 6.) ** 2.))
               ),
             31.13 );
         ])
      Bullet_Pattern.(
        Sequence
          [
            (midboss_pattern_11 2.0, 4.0);
            (midboss_pattern_12 4.0, 6.0);
            (extra_midboss_1, 31.13);
            (midboss_pattern_2, 999.);
          ])
  in
  List.flatten
    [
      List.init 14 intro1;
      List.init 14 intro2;
      List.init 12 intro3;
      List.init 12 intro4;
      [ mid_aimed 70 (-50) 10.; mid_aimed 506 (-50) 10. ];
      [ mid_aimed 120 (-60) 12.; mid_aimed 456 (-60) 12. ];
      [ mid_aimed 170 (-70) 14.; mid_aimed 406 (-70) 14. ];
      [ mid_aimed 220 (-80) 16.; mid_aimed 356 (-80) 16. ];
      [ mid_circle 220 (-80) 20. 1.; mid_circle 356 (-80) 21. (-1.) ];
      [ mid_circle 170 (-70) 22. 1.; mid_circle 406 (-70) 23. (-1.) ];
      [ mid_circle 120 (-60) 24. 1.; mid_circle 456 (-60) 25. (-1.) ];
      List.init 16 mid_ufos;
      [ midboss ];
    ]

let texturelist =
  [
    "assets/ocaml-logo.png";
    "assets/red_th_bullet.png";
    "assets/dodonpachi_ship_c.png";
    "assets/dodonpachi_ship_c_hitbox.png";
    "assets/deltarune_explosion.png";
    "assets/demo_level/space_background.png";
    "assets/demo_level/ufo.png";
    "assets/demo_level/orb_turret.png";
    "assets/demo_level/boss_skull.png";
  ]

let draw_start_screen () =
  Raylib.draw_text "3110 BULLET HELL" 70 150 40 Raylib.Color.green;
  Raylib.draw_text "CONTROLS:" 70 250 20 Raylib.Color.white;
  Raylib.draw_text "WASD or Arrow Keys to Move" 70 280 20 Raylib.Color.lightgray;
  Raylib.draw_text "Spacebar or Z to Shoot" 70 310 20 Raylib.Color.lightgray;
  Raylib.draw_text "Shift to Enter \"Focus\" Mode and Slow Down " 70 340 20
    Raylib.Color.lightgray;
  Raylib.draw_text "GOAL: Defeat the boss to win" 70 370 20 Raylib.Color.green;
  Raylib.draw_text "PRESS ENTER TO START" 70 400 20 Raylib.Color.yellow

let draw_centered_text text y_offset size color =
  let window_w = 576 in
  let window_h = 672 in
  let w = Raylib.measure_text text size in
  let x = (window_w - w) / 2 in
  let y = ((window_h - size) / 2) + y_offset in
  Raylib.draw_text text x y size color

let draw_death_screen () = draw_centered_text "game over" 0 40 Raylib.Color.red
let draw_end_screen () = draw_centered_text "you win" 0 40 Raylib.Color.yellow

let draw_background () =
  Raylib.draw_texture
    (Render_State.render_loaded_texture "assets/demo_level/space_background.png")
    0 0 Color.white

(** [init_level ()] passes the level properties to main.ml, in exactly this
    order:
    - [window_x : int] the width of the window in pixels.
    - [window_y : int] the height of the window in pixels.
    - [window_title : string] the title shown on window.
    - [texturelist : string list] the textures the level must render.
    - [initial_state : Game_State.t] the initial state of the level.
    - [draw_start_screen : unit -> unit] the function that draws the start
      screen using Raylib.
    - [draw_death_screen] the function that draws the game over screen using
      Raylib.
    - [draw_end_screen] the function that draws the end of level screen using
      Raylib.
    - [draw_background] the function that draws the background of the level
      using Raylib. *)
let init_level () =
  ( 576,
    672,
    "Demo Stage",
    texturelist,
    initialize_state enemy_list game_player,
    draw_start_screen,
    draw_death_screen,
    draw_end_screen,
    draw_background )
