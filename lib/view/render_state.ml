module Game_State = Model.Game_state
module Enemy = Model.Enemy
module Bullet = Model.Bullet
module Player = Model.Player
open Raylib

(* TODO a lot of textures are gonna be needed in the final product, maybe this
   stuff should be its own file*)

let loaded_textures = Hashtbl.create 100

let initialize_textures alltextures =
  Hashtbl.add loaded_textures "assets/missing_texture.png"
    (ref (load_texture "assets/missing_texture.png"));
  List.iter
    (fun s -> Hashtbl.add loaded_textures s (ref (Raylib.load_texture s)))
    alltextures

let start_screen = ref (fun () -> ())
let set_start_screen some_function = start_screen := some_function
let death_screen = ref (fun () -> ())
let set_death_screen some_function = death_screen := some_function
let end_screen = ref (fun () -> ())
let set_end_screen some_function = end_screen := some_function
let background = ref (fun () -> ())
let set_background some_function = background := some_function

let render_loaded_texture filename =
  let missing_texture =
    !(Hashtbl.find loaded_textures "assets/missing_texture.png")
  in
  match Hashtbl.find_opt loaded_textures filename with
  | Some t -> !t
  | _ -> missing_texture

let draw_health_bar ~cx ~cy ~width ~current ~total =
  if total <= 0 then ()
  else
    let h = 4 in
    let w = int_of_float width in
    let x = cx - (w / 2) in
    let frac =
      Stdlib.max 0. (Stdlib.min 1. (float_of_int current /. float_of_int total))
    in
    let fill_w = int_of_float (float_of_int w *. frac) in
    Raylib.draw_rectangle x cy w h (Raylib.Color.create 80 0 0 200);
    Raylib.draw_rectangle x cy fill_w h (Raylib.Color.create 0 220 60 230)

let render_state state =
  (* render enemies then player then bullets*)
  (* newer bullets show on top of older bullets*)
  let draw_enemy e color =
    let enemy_tex = render_loaded_texture Enemy.(e.sprite_filename) in
    Game_State.(
      let sprite_w = float_of_int (fst Enemy.(e.sprite_size)) in
      let sprite_h = float_of_int (snd Enemy.(e.sprite_size)) in
      draw_texture_pro enemy_tex
        (Rectangle.create 0. 0.
           (float_of_int (Texture.width enemy_tex))
           (float_of_int (Texture.height enemy_tex)))
        (Rectangle.create e.x e.y sprite_w sprite_h)
        (Vector2.create (sprite_w /. 2.) (sprite_h /. 2.))
        0.0 color);
    if Enemy.(e.sprite_filename) <> "anchor" then
      draw_health_bar
        ~cx:(int_of_float Enemy.(e.x))
        ~cy:(int_of_float Enemy.(e.y) - (snd Enemy.(e.sprite_size) / 2) - 6)
        ~width:(float_of_int (fst Enemy.(e.sprite_size)))
        ~current:Enemy.(e.health) ~total:Enemy.(e.max_health)
  in
  let draw_player p dead =
    let player_tex =
      if not dead then render_loaded_texture Player.(p.sprite_filename)
      else render_loaded_texture Player.(p.death_filename)
    in
    Game_State.(
      let sprite_w = float_of_int (fst Player.(p.sprite_size)) in
      let sprite_h = float_of_int (snd Player.(p.sprite_size)) in
      draw_texture_pro player_tex
        (Rectangle.create 0. 0.
           (float_of_int (Texture.width player_tex))
           (float_of_int (Texture.height player_tex)))
        (Rectangle.create (float_of_int p.x) (float_of_int p.y) sprite_w
           sprite_h)
        (Vector2.create (sprite_w /. 2.) (sprite_h /. 2.))
        0.0 Color.white)
  in
  let draw_bullet color b =
    (* TODO bullets should probably be given textures based on specific type,
       not assigning one to each bullet*)
    let bullet_tex = render_loaded_texture "assets/red_th_bullet.png" in
    (* likewise, size should be based on type, not a specific assignment*)
    let scale = 1.0 in
    let bw = float_of_int (Texture.width bullet_tex) *. scale in
    let bh = float_of_int (Texture.height bullet_tex) *. scale in
    draw_texture_pro bullet_tex
      (Rectangle.create 0. 0.
         (float_of_int (Texture.width bullet_tex))
         (float_of_int (Texture.height bullet_tex)))
      (Rectangle.create Bullet.(b.x) Bullet.(b.y) bw bh)
      (Vector2.create (bw /. 2.) (bh /. 2.))
      0.0 color
  in
  (* MUST call begin_drawing before trying to render text or sprites *)
  Raylib.begin_drawing ();
  Raylib.clear_background Raylib.Color.black;

  (match state.Game_State.phase with
  | Game_State.StartScreen -> !start_screen ()
  | Game_State.Playing ->
      !background ();
      BatDllist.iter
        (fun e ->
          let c = if Enemy.(e.was_hit) then Color.red else Color.white in
          draw_enemy e c)
        Game_State.(state.active_enemies);
      draw_player state.player
        (state.elapsed_time -. state.player.last_hit < 0.5);
      BatDllist.iter
        (draw_bullet Color.skyblue)
        Game_State.(state.player_bullets);
      BatDllist.iter (draw_bullet Color.red) Game_State.(state.enemy_bullets);
      draw_text
        ("Lives: " ^ string_of_int state.player.lives)
        10 10 20 Color.white;
      draw_text
        ("Score: " ^ string_of_int state.Game_State.score)
        10 35 20 Color.white
  | Game_State.GameOver ->
      !death_screen ();
      draw_text
        ("score: " ^ string_of_int state.Game_State.score)
        200 450 30 Color.white
  | Game_State.LevelEnd ->
      !end_screen ();
      draw_text
        ("score: " ^ string_of_int state.Game_State.score)
        200 450 30 Color.white);

  (* MUST call end_drawing so macOS processes the window events *)
  Raylib.end_drawing ()
