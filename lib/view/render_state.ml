module Game_State = Model.Game_state
module Enemy = Model.Enemy
module Bullet = Model.Bullet
module Player = Model.Player
open Raylib
open Batteries

let loaded_textures = Hashtbl.create 100
let whitearr = ref [||]
let redarr = ref [||]
let orangearr = ref [||]
let yellowarr = ref [||]
let greenarr = ref [||]
let cyanarr = ref [||]
let bluearr = ref [||]
let purplearr = ref [||]

(*Since Raylib uses the typical "top left is 0,0" coordinate scheme in computer
  graphics, the positions of objects must be converted. X remains the same,
  while Y is set to window_y - Y, and theta is negated *)
let convert_x x = x
let convert_y (state : Game_State.t) y = float_of_int state.window_y -. y
let convert_theta t = -.t

let initialize_textures alltextures =
  Hashtbl.add loaded_textures "assets/missing_texture.png"
    (ref (load_texture "assets/missing_texture.png"));
  (*its easier this way*)
  whitearr :=
    [|
      load_texture "assets/bullet_textures/whitesimplebullet.png";
      load_texture "assets/bullet_textures/whitesimplelaser.png";
      load_texture "assets/bullet_textures/whitearrow.png";
    |];
  redarr :=
    [|
      load_texture "assets/bullet_textures/redsimplebullet.png";
      load_texture "assets/bullet_textures/redsimplelaser.png";
      load_texture "assets/bullet_textures/redarrow.png";
    |];
  orangearr :=
    [|
      load_texture "assets/bullet_textures/orangesimplebullet.png";
      load_texture "assets/bullet_textures/orangesimplelaser.png";
      load_texture "assets/bullet_textures/orangearrow.png";
    |];
  yellowarr :=
    [|
      load_texture "assets/bullet_textures/yellowsimplebullet.png";
      load_texture "assets/bullet_textures/yellowimplelaser.png";
      load_texture "assets/bullet_textures/yellowarrow.png";
    |];
  greenarr :=
    [|
      load_texture "assets/bullet_textures/greensimplebullet.png";
      load_texture "assets/bullet_textures/greensimplelaser.png";
      load_texture "assets/bullet_textures/greenarrow.png";
    |];
  cyanarr :=
    [|
      load_texture "assets/bullet_textures/cyansimplebullet.png";
      load_texture "assets/bullet_textures/cyansimplelaser.png";
      load_texture "assets/bullet_textures/cyanarrow.png";
    |];
  bluearr :=
    [|
      load_texture "assets/bullet_textures/bluesimplebullet.png";
      load_texture "assets/bullet_textures/bluesimplelaser.png";
      load_texture "assets/bullet_textures/bluearrow.png";
    |];
  purplearr :=
    [|
      load_texture "assets/bullet_textures/purplesimplebullet.png";
      load_texture "assets/bullet_textures/purplesimplelaser.png";
      load_texture "assets/bullet_textures/purplearrow.png";
    |];
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
        (Rectangle.create
           (convert_x e.object_properties.x)
           (convert_y state e.object_properties.y)
           sprite_w sprite_h)
        (Vector2.create (sprite_w /. 2.) (sprite_h /. 2.))
        0.0 color)
  in
  let draw_player p focus =
    let player_tex =
      if focus then render_loaded_texture Player.(p.focus_filename)
      else render_loaded_texture Player.(p.sprite_filename)
    in
    Game_State.(
      let sprite_w = float_of_int (fst Player.(p.sprite_size)) in
      let sprite_h = float_of_int (snd Player.(p.sprite_size)) in
      draw_texture_pro player_tex
        (Rectangle.create 0. 0.
           (float_of_int (Texture.width player_tex))
           (float_of_int (Texture.height player_tex)))
        (Rectangle.create (convert_x p.x) (convert_y state p.y) sprite_w
           sprite_h)
        (Vector2.create (sprite_w /. 2.) (sprite_h /. 2.))
        0.0 Color.white)
  in
  let draw_bullet (b : Bullet.t) =
    let bultex =
      match b.bullet_color with
      | White -> !whitearr.(if b.bullet_type = Arrow then 2 else 0)
      | Red -> !redarr.(if b.bullet_type = Arrow then 2 else 0)
      | Orange -> !orangearr.(if b.bullet_type = Arrow then 2 else 0)
      | Yellow -> !yellowarr.(if b.bullet_type = Arrow then 2 else 0)
      | Green -> !greenarr.(if b.bullet_type = Arrow then 2 else 0)
      | Blue -> !bluearr.(if b.bullet_type = Arrow then 2 else 0)
      | Cyan -> !cyanarr.(if b.bullet_type = Arrow then 2 else 0)
      | Purple -> !purplearr.(if b.bullet_type = Arrow then 2 else 0)
    in
    let lastex =
      match b.bullet_color with
      | White -> !whitearr.(1)
      | Red -> !redarr.(1)
      | Orange -> !orangearr.(1)
      | Yellow -> !yellowarr.(1)
      | Green -> !greenarr.(1)
      | Blue -> !bluearr.(1)
      | Cyan -> !cyanarr.(1)
      | Purple -> !purplearr.(1)
    in

    if
      (b.object_properties.x -. 100. > float_of_int state.window_x
      || b.object_properties.x < -100.
      || b.object_properties.y < -100.
      || b.object_properties.y -. 100. > float_of_int state.window_y)
      &&
      match b.bullet_type with
      | Trail (_, _, _) -> false
      | _ -> true
    then ()
    else
      let bullet_tex = bultex in
      let laser_tex = lastex in
      let scale = b.hitbox_radius /. 8. in
      let bw = float_of_int (Texture.width bullet_tex) *. scale in
      let bh = float_of_int (Texture.height bullet_tex) *. scale in
      match Bullet.(b.bullet_type) with
      | Trail (n, intv, memory) ->
          let prevx = ref None in
          let prevy = ref None in

          BatDeque.iteri
            (fun i (x, y) ->
              if i mod intv <> 0 then ()
              else if !prevx = None then (
                draw_texture_pro bullet_tex
                  (Rectangle.create 0. 0.
                     (float_of_int (Texture.width bullet_tex))
                     (float_of_int (Texture.height bullet_tex)))
                  (Rectangle.create
                     (convert_x Bullet.(b.object_properties.x))
                     (convert_y state Bullet.(b.object_properties.y))
                     bw bh)
                  (Vector2.create (bw /. 2.) (bh /. 2.))
                  (convert_theta
                     (b.object_properties.theta *. 180. /. Float.pi))
                  (Color.create 255 255 255
                     (int_of_float (b.object_properties.opacity *. 255.)));
                prevx := Some x;
                prevy := Some y)
              else
                let length =
                  max 1.
                    (sqrt
                       (((x -. Option.get !prevx) ** 2.)
                       +. ((y -. Option.get !prevy) ** 2.)))
                in
                let angle =
                  atan2 (Option.get !prevy -. y) (Option.get !prevx -. x)
                in
                draw_texture_pro bullet_tex
                  (Rectangle.create 0. 0.
                     (float_of_int (Texture.width bullet_tex))
                     (float_of_int (Texture.height bullet_tex)))
                  (Rectangle.create (convert_x x) (convert_y state y) bw bh)
                  (Vector2.create (bw /. 2.) (bh /. 2.))
                  0.0
                  (Color.create 255 255 255
                     (int_of_float (b.object_properties.opacity *. 255.)));
                draw_texture_pro laser_tex
                  (Rectangle.create 0. 0.
                     (float_of_int (Texture.width laser_tex))
                     (float_of_int (Texture.height laser_tex)))
                  (Rectangle.create (convert_x x) (convert_y state y) length bh)
                  (Vector2.create 0. (bh /. 2.))
                  (convert_theta (angle *. 180. /. Float.pi))
                  (Color.create 255 255 255
                     (int_of_float (b.object_properties.opacity *. 255.)));
                prevx := Some x;
                prevy := Some y)
            memory
      | Laser l ->
          draw_texture_pro bullet_tex
            (Rectangle.create 0. 0.
               (float_of_int (Texture.width bullet_tex))
               (float_of_int (Texture.height bullet_tex)))
            (Rectangle.create
               (convert_x Bullet.(b.object_properties.x))
               (convert_y state Bullet.(b.object_properties.y))
               bw bh)
            (Vector2.create (bw /. 2.) (bh /. 2.))
            0.0
            (Color.create 255 255 255
               (int_of_float (b.object_properties.opacity *. 255.)));
          draw_texture_pro bullet_tex
            (Rectangle.create 0. 0.
               (float_of_int (Texture.width bullet_tex))
               (float_of_int (Texture.height bullet_tex)))
            (Rectangle.create
               (convert_x
                  Bullet.(
                    b.object_properties.x +. (l *. cos b.object_properties.theta)))
               (convert_y state
                  Bullet.(
                    b.object_properties.y +. (l *. sin b.object_properties.theta)))
               bw bh)
            (Vector2.create (bw /. 2.) (bh /. 2.))
            0.0
            (Color.create 255 255 255
               (int_of_float (b.object_properties.opacity *. 255.)));
          draw_texture_pro laser_tex
            (Rectangle.create 0. 0.
               (float_of_int (Texture.width laser_tex))
               (float_of_int (Texture.height laser_tex)))
            (Rectangle.create
               (convert_x Bullet.(b.object_properties.x))
               (convert_y state Bullet.(b.object_properties.y))
               l bh)
            (Vector2.create 0. (bh /. 2.))
            (convert_theta (b.object_properties.theta *. 180. /. Float.pi))
            (Color.create 255 255 255
               (int_of_float (b.object_properties.opacity *. 255.)))
      | _ ->
          draw_texture_pro bullet_tex
            (Rectangle.create 0. 0.
               (float_of_int (Texture.width bullet_tex))
               (float_of_int (Texture.height bullet_tex)))
            (Rectangle.create
               (convert_x Bullet.(b.object_properties.x))
               (convert_y state Bullet.(b.object_properties.y))
               bw bh)
            (Vector2.create (bw /. 2.) (bh /. 2.))
            (convert_theta (b.object_properties.theta *. 180. /. Float.pi))
            (Color.create 255 255 255
               (int_of_float (b.object_properties.opacity *. 255.)))
  in
  (* MUST call begin_drawing before trying to render text or sprites *)
  Raylib.begin_drawing ();
  Raylib.clear_background Raylib.Color.black;

  (match state.Game_State.phase with
  | Game_State.StartScreen -> !start_screen ()
  | Game_State.Playing ->
      !background ();

      List.iter
        (fun e ->
          let c = if Enemy.(e.was_hit) then Color.red else Color.white in
          draw_enemy e c)
        Game_State.(get_active_enemies state);

      draw_player state.player state.player.focus;

      Game_State.apply_function_to_player_bullets state (fun b -> draw_bullet b);

      Game_State.apply_function_to_enemy_bullets state (fun b -> draw_bullet b)
  | Game_State.GameOver -> !death_screen ()
  | Game_State.LevelEnd -> !end_screen ());

  (* MUST call end_drawing so macOS processes the window events *)
  Raylib.end_drawing ()
