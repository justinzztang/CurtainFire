module Bullet = Bullet
module Player = Player
open Random

let () = Random.init 1337
(* pulse of pattern , delta do the pattern every delta seconds *)

(* arc of pattern, count, angle shoot count pattern spread evenly across count*)

(* aimed of pattern, set the reference angle of the pattern to the angle to
   player*)

(* spin of pattern, omega : shoot pattern at spinning refernce angle at omega*)

type int_handler =
  | Int of int
  | RandInt of int * int

type float_handler =
  | Float of float
  | RandFloat of float * float

type bullet_pattern =
  | Nothing
  | Bullet of Bullet.bullet
  | Timed of bullet_pattern * float_handler * float_handler (*start, end*)
  | Angled of bullet_pattern * float_handler
  | Pulse of bullet_pattern * float_handler
  | Arc of bullet_pattern * int_handler * float_handler
  | Aimed of bullet_pattern
  | Spin of bullet_pattern * float_handler
  | Iterate of bullet_pattern * int_handler * (int -> Bullet.bullet)
    (*take in an int and spit out a mod bullet*)
  | Combo of bullet_pattern list
  | Sequence of (bullet_pattern * float) list (*pattern, time limit*)
  | Manual of Bullet.bullet list

let eval_int n =
  match n with
  | Int x -> x
  | RandInt (a, b) -> Random.int (b - a) + a

let eval_float f =
  match f with
  | Float x -> x
  | RandFloat (a, b) -> Random.float (b -. a) +. a

(* "adds" b2 to b1, changing the modifiable properties of a bullet*)
let add_bullets (b1 : Bullet.bullet) (b2 : Bullet.bullet) =
  Bullet.
    {
      x = b1.x +. b2.x;
      y = b1.y +. b2.y;
      bullet_type = b1.bullet_type;
      spawn_time = b1.spawn_time;
      ttl = b1.ttl +. b2.ttl;
      velocity = Combo [ b1.velocity; b2.velocity ];
    }

let rec eval_bp bp player previous_time current_time enemyx enemyy enemy_spawn
    (mods_bullet : Bullet.bullet) =
  match bp with
  | Sequence ((bph, cond) :: bpt) ->
      if cond > current_time -. enemy_spawn then
        eval_bp bph player previous_time current_time enemyx enemyy enemy_spawn
          mods_bullet
      else
        eval_bp (Sequence bpt) player previous_time current_time enemyx enemyy
          enemy_spawn mods_bullet
  | Sequence [] -> []
  | Iterate (bpp, n, f) ->
      (* reall weird but it can work*)
      List.flatten
        (List.init (eval_int n) (fun i ->
             eval_bp bpp player previous_time current_time enemyx enemyy
               enemy_spawn
               (add_bullets mods_bullet (f i))))
      (*create starting_bullet + fi*)
  | Timed (bpp, s, e) ->
      if current_time -. enemy_spawn <= eval_float e then
        if current_time -. enemy_spawn >= eval_float s then
          eval_bp bpp player previous_time current_time enemyx enemyy
            enemy_spawn mods_bullet
        else []
      else []
  | Angled (bpp, theta) ->
      eval_bp bpp player previous_time current_time enemyx enemyy enemy_spawn
        {
          mods_bullet with
          velocity =
            Combo
              [ mods_bullet.velocity; Linear (eval_float theta, fun t -> 0.0) ];
        }
  | Pulse (bpp, period) ->
      let prev_idx =
        int_of_float ((previous_time -. enemy_spawn) /. eval_float period)
      in
      let cur_idx =
        int_of_float ((current_time -. enemy_spawn) /. eval_float period)
      in

      if prev_idx < cur_idx then (*spawn the pattern*)
        eval_bp bpp player previous_time current_time enemyx enemyy enemy_spawn
          mods_bullet
      else []
  | Aimed bpp ->
      let dx = float_of_int Player.(player.x) -. enemyx in
      let dy = enemyy -. float_of_int player.y in
      let angle = atan2 dy dx in

      eval_bp bpp player previous_time current_time enemyx enemyy enemy_spawn
        {
          mods_bullet with
          velocity =
            Combo [ mods_bullet.velocity; Linear (angle, fun t -> 0.0) ];
        }
  | Arc (bpp, n, theta) ->
      List.flatten
        (List.init (eval_int n) (fun i ->
             eval_bp bpp player previous_time current_time enemyx enemyy
               enemy_spawn
               {
                 mods_bullet with
                 velocity =
                   Combo
                     [
                       mods_bullet.velocity;
                       Linear
                         ( -.(eval_float theta /. 2.)
                           +. eval_float theta
                              /. float_of_int (eval_int n)
                              *. float_of_int i,
                           fun t -> 0.0 );
                     ];
               }))
  | Bullet blt ->
      let newb = add_bullets blt mods_bullet in
      [
        {
          newb with
          x = newb.x +. enemyx;
          y = newb.y +. enemyy;
          spawn_time = current_time;
        };
      ]
  | Spin (bpp, omega) ->
      eval_bp bpp player previous_time current_time enemyx enemyy enemy_spawn
        {
          mods_bullet with
          velocity =
            Combo
              [
                mods_bullet.velocity;
                Linear
                  ( eval_float omega *. (current_time -. enemy_spawn),
                    fun t -> 0.0 );
              ];
        }
  | Combo bps ->
      List.flatten
        (List.map
           (fun b ->
             eval_bp b player previous_time current_time enemyx enemyy
               enemy_spawn mods_bullet)
           bps)
  | _ -> []
