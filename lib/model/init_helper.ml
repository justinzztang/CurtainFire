module Player = Player
module Bullet_Pattern = Bullet_pattern
module Game_State = Game_state
module Enemy = Enemy
open Raylib

let default_player =
  Player.
    {
      x = 0;
      y = 0;
      lives = 1;
      bombs = 1;
      is_shooting = false;
      shot_type = Linear;
      sprite_size = (10, 10);
      sprite_filename = "missing";
      death_filename = "missing";
      hitbox_radius = 5;
      hitbox_position = (0, 0);
      max_speed = 1.;
      focus_speed = 1.;
      last_hit = -99.;
    }

let default_enemy =
  Enemy.
    {
      x = 0.;
      y = 0.;
      health = 1;
      max_health = 1;
      spawn_time = 0.;
      ttl = 1.;
      pathing = Movement.Linear (0.0, fun t -> 0.0);
      spawn_pattern = Bullet_Pattern.Nothing;
      sprite_filename = "missing";
      sprite_size = (10, 10);
      was_hit = false;
    }

let set_player_appearance ?player:(p = default_player) (w, h) filename
    death_filename hb_radius hb_pos =
  {
    p with
    sprite_size = (w, h);
    sprite_filename = filename;
    death_filename;
    hitbox_radius = hb_radius;
    hitbox_position = hb_pos;
  }

let set_player_state ?player:(p = default_player) (x, y) lives bombs shot_type
    max_speed focus_speed =
  { p with x; y; lives; bombs; shot_type; max_speed; focus_speed }

let set_enemy_appearance ?enemy:(e = default_enemy) sprite_filename sprite_size
    =
  { e with sprite_filename; sprite_size }

let set_enemy_state ?enemy:(e = default_enemy) (x, y) health spawn_time ttl
    pathing spawn_pattern =
  {
    e with
    x = float_of_int x;
    y = float_of_int y;
    health;
    max_health = health;
    spawn_time;
    ttl;
    pathing;
    spawn_pattern;
  }

let initialize_state elist player =
  Game_State.
    {
      phase = StartScreen;
      player_bullets =
        BatDllist.of_list
          [
            Bullet.create_bullet (-65537.) (-65537.) Anchor Float.infinity
              (Linear (0.0, fun t -> 0.0))
              0.0;
          ];
      enemy_bullets =
        BatDllist.of_list
          [
            Bullet.create_bullet 65537. 65537. Anchor Float.infinity
              (Linear (0.0, fun t -> 0.0))
              0.0;
          ];
      active_enemies = BatDllist.of_list [ Enemy.anchor ];
      queued_enemies =
        BatDllist.of_list
          (List.sort
             (fun e1 e2 ->
               Enemy.(
                 if e1.spawn_time > e2.spawn_time then 1
                 else if e1.spawn_time < e2.spawn_time then -1
                 else 0))
             (elist @ [ Enemy.queue_anchor ]));
      player;
      elapsed_time = 0.;
      score = 0;
    }
