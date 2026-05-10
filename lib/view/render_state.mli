module Game_State = Model.Game_state
open Raylib

(** Mutable hook called to render the start-screen overlay. Defaults to a no-op
    until set via [set_start_screen]. *)
val start_screen : (unit -> unit) ref

(** Mutable hook called to render the game-over screen. Defaults to a no-op
    until set via [set_death_screen]. *)
val death_screen : (unit -> unit) ref

(** Mutable hook called to render the end screen. Defaults to a no-op until set
    via [set_death_screen]. *)
val end_screen : (unit -> unit) ref

(** [initialize_textures filenames] loads the textures at each path in
    [filenames] (which must be valid relative paths) into the renderer's texture
    cache, alongside a fallback [missing_texture.png]. Must be called after
    [Raylib.init_window] and before any [render_state] call. *)
val initialize_textures : string list -> unit

(** [set_start_screen f] installs [f] as the start-screen render callback. *)
val set_start_screen : (unit -> unit) -> unit

(** [set_death_screen f] installs [f] as the game-over render callback. *)
val set_death_screen : (unit -> unit) -> unit

(** [set_end_screen f] installs [f] as the end_screen render callback. *)
val set_end_screen : (unit -> unit) -> unit

(** [set_background f] installs [f] as the per-frame background render callback.
    [f] is invoked at the start of each [Playing] frame, before any enemies,
    bullets, or the player are drawn. *)
val set_background : (unit -> unit) -> unit

(** [render_loaded_texture filename] returns the texture previously loaded under
    [filename]. If no such texture is in the cache, returns the fallback
    [missing_texture.png]. *)
val render_loaded_texture : string -> Raylib.Texture.t

(** [render_state state] draws one frame for the given game [state]. Behavior
    depends on [state.phase]: [StartScreen] calls the registered start-screen
    hook; [Playing] draws the background, enemies, player, and bullets;
    [GameOver] calls the death-screen hook. Must be called between
    [Raylib.begin_drawing] / [Raylib.end_drawing], which it manages itself. *)
val render_state : Game_State.t -> unit
