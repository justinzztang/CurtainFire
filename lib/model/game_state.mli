module Player = Player
module Bullet = Bullet
module Enemy = Enemy
module Ast = Parse.Ast
open Batteries

(** Which top-level phase the game is in: the title screen, active gameplay, or
    the game-over screen. *)
type game_phase =
  | StartScreen
  | Playing
  | GameOver
  | LevelEnd

(**The full state of the game at a given point in time.
   - [phase] represents the phase of the game
   - [player_bullets] stores all live bullets originating from the player
   - [enemy_bullets] stores all live bullets originating from enemies
   - [active_enemies] stores all live enemies
   - [queued_enemies] stores upcoming enemies that will be spawned at a certain
     time; this list must be sorted by spawn time
   - [player] is the player object
   - [elapsed_frames] is how many frames the game has run
   - [window_x] and [window_y] are the size of the window
   - [debug_flag] sets debug mode on or off (disables player collisions) *)

type t = {
  phase : game_phase;
  player_bullets : (string, Bullet.t Dynarray.t) Hashtbl.t;
  enemy_bullets : (string, Bullet.t Dynarray.t) Hashtbl.t;
  active_enemies : (string, Enemy.t Dynarray.t) Hashtbl.t;
  queued_enemies : Enemy.t list;
  (*TODO: parts are for another day*)
  player : Player.t;
  elapsed_frames : int;
  window_x : int;
  window_y : int;
  debug_flag : bool;
}

(**Advance the game state one frame, spawning and updating enemies and bullets*)
val update_state : int -> t -> t

(**Returns a list of all live enemies*)
val get_active_enemies : t -> Enemy.t list

(**Returns a list of all live enemy bullets*)
val get_enemy_bullets : t -> Bullet.t list

(**Apply a function to all live enemy bullets*)
val apply_function_to_enemy_bullets : t -> (Bullet.t -> unit) -> unit

(**Returns a list of all live player bullets*)
val get_player_bullets : t -> Bullet.t list

(**Apply a function to all live player bullets*)
val apply_function_to_player_bullets : t -> (Bullet.t -> unit) -> unit

(**Count the total number of items in a Hashtbl of Dynarrays, used for counting
   the number of bullets and enemies and so on*)
val count_hashtable_stuff : ('a, 'b Dynarray.t) Hashtbl.t -> int
