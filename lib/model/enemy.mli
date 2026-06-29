module Obj = Object
module Ast = Parse.Ast
module Bullet = Bullet

(**[enemy] represents an enemy in the game and extends a generic object with
   [health], [max_health], [behavior], a [spawn_pattern], a custom
   [sprite_filename] and [sprite_size], and (unused) [was_hit] flag*)
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

(** [create_enemy tag x y speed theta current_frame ttl tangible opacity
     parent_properties health behavior spawn_pattern sprite_filename
     sprite_size] creates an enemy with the given properties*)
val create_enemy :
  string ->
  float ->
  float ->
  float ->
  float ->
  int ->
  int ->
  bool ->
  float ->
  Obj.properties option ->
  int ->
  Ast.behavior ->
  Ast.bullet_pattern ->
  string ->
  int * int ->
  t
