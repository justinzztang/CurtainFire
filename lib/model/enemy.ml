module Obj = Object
module Ast = Parse.Ast
module Bullet = Bullet

type t = {
  mutable object_properties : Obj.properties;
  mutable health : int;
  max_health : int;
  behavior : Ast.behavior;
  spawn_pattern : Ast.bullet_pattern;
  sprite_filename : string;
  sprite_size : int * int;
  was_hit : bool;
}

let create_enemy tag x y speed theta current_frame ttl tangible opacity
    parent_properties health behavior spawn_pattern sprite_filename sprite_size
    =
  let op =
    Obj.
      {
        tag;
        x;
        y;
        speed;
        theta;
        spawn_x = x;
        spawn_y = y;
        spawn_speed = speed;
        spawn_theta = theta;
        spawn_frame = current_frame;
        ttl;
        tangible;
        opacity;
        parent_properties;
      }
  in
  {
    object_properties = op;
    health;
    max_health = health;
    behavior;
    spawn_pattern;
    sprite_filename;
    sprite_size;
    was_hit = false;
  }
