module Obj = Object
module Ast = Parse.Ast
open Batteries
open Parse.Util

(** Type of bullet that defines its appearance and collision detection. [Circle]
    [Arrow] and [Knife] all have simple radial hitboxes, while [Laser(length)]
    extends its hitbox and appearance forwards, and
    [Trail(repetitions, time_interval, memory_storage)] creates several line
    segments to previous locations. Note: [Knife] does not have a graphic yet.
*)
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

(** [bullet] extends a generic object with a type, color, [behavior], hitbox
    radius, and a parent environment for tracking variables*)

type t = {
  mutable object_properties : Obj.properties;
  bullet_type : bullet_type;
  bullet_color : bullet_color;
  behavior : Ast.behavior;
  hitbox_radius : float;
  parent_env : float ref segmented_list;
}

(** [create_bullet tag x y speed theta current_frame ttl bullet_color
     bullet_type behavior hitbox_radius tangible opacity parent_properties
     parent_env] creates a bullet with the given properties *)

val create_bullet :
  string ->
  float ->
  float ->
  float ->
  float ->
  int ->
  int ->
  bullet_color ->
  bullet_type ->
  Ast.behavior ->
  float ->
  bool ->
  float ->
  Obj.properties option ->
  float ref segmented_list ->
  t
