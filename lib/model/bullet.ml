module Obj = Object
module Ast = Parse.Ast
open Parse.Util
open Batteries

type bullet_type =
  | Circle
  | Arrow
  | Knife
  | Laser of float (*length of laser*)
  | Trail of
      int
      * int
      * (float * float) BatDeque.t (*repetitions, interval, list to store*)

type bullet_color =
  | White
  | Red
  | Orange
  | Yellow
  | Green
  | Cyan
  | Blue
  | Purple

type t = {
  mutable object_properties : Obj.properties;
  bullet_type : bullet_type;
  bullet_color : bullet_color;
  behavior : Ast.behavior;
  hitbox_radius : float;
  parent_env : float ref segmented_list;
}

let create_bullet tag x y speed theta current_frame ttl bullet_color bullet_type
    behavior hitbox_radius tangible opacity parent_properties parent_env =
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
    bullet_color;
    bullet_type;
    behavior;
    hitbox_radius;
    parent_env;
  }
