module Player = Player
module Bullet = Bullet
module Enemy = Enemy
open Batteries

(* Distinct phases of the game *)
type game_phase =
  | StartScreen
  | Playing
  | GameOver
  | LevelEnd

type t = {
  phase : game_phase;
  player_bullets : Bullet.bullet BatDllist.t;
  enemy_bullets : Bullet.bullet BatDllist.t;
  active_enemies : Enemy.enemy BatDllist.t;
  queued_enemies : Enemy.enemy BatDllist.t;
      (*probably want sorted based on next to be added first*)
  player : Player.player;
  elapsed_time : float;
  score : int;
}

let finish_time = ref None
let enemies_killed_this_frame = ref 0
let score_gained_this_frame = ref 0

let rec queued_enemy_transfer q1 q2 current_time =
  (*batdlllist doesnt support empty lists so we need to find a way to represent
    one*)
  if Enemy.((BatDllist.get q1).spawn_time) > current_time (*or empty*) then
    (q1, q2)
  else
    let q1_front = BatDllist.get q1 in
    queued_enemy_transfer (BatDllist.drop q1)
      (*man i really dont like working with doubly linked lists*)
      (BatDllist.next (BatDllist.prepend q2 q1_front))
      current_time

(* [bullet_hits_enemy bullet enemy] evaluates to true when bullet [bullet]'s
   position falls inside the axis-aligned bounding box of enemy [enemy]'s
   sprite. *)
let bullet_hits_enemy bullet enemy =
  match Bullet.(bullet.bullet_type) with
  | Anchor -> false
  | _ ->
      let half_w = float_of_int (fst Enemy.(enemy.sprite_size)) /. 2.0 in
      let half_h = float_of_int (snd Enemy.(enemy.sprite_size)) /. 2.0 in
      Bullet.(bullet.x) >= Enemy.(enemy.x) -. half_w
      && Bullet.(bullet.x) <= Enemy.(enemy.x) +. half_w
      && Bullet.(bullet.y) >= Enemy.(enemy.y) -. half_h
      && Bullet.(bullet.y) <= Enemy.(enemy.y) +. half_h

(* [update_state current_time game_state] will update the current state from the
   last state based on the amount of time that has passed*)
let update_state (current_time : float) game_state =
  enemies_killed_this_frame := 0;
  score_gained_this_frame := 0;
  (* spawn new enemies and bullets that need to be spawned *)
  (* put the next enemies to spawn from queued enemies into added_enemies*)
  let remaining_enemies, added_enemies =
    queued_enemy_transfer game_state.queued_enemies game_state.active_enemies
      current_time
  in
  let updated_enemies =
    BatDllist.filter_map
      (fun e ->
        if Enemy.(e.spawn_time +. e.ttl) < current_time then None
        else Some Enemy.(update_enemy e current_time))
      added_enemies
  in
  (*spawning bullets requires going through each enemy, and checking what
    bullets they need to spawn*)
  let new_bullets =
    BatDllist.map
      (fun e ->
        Enemy.spawn_newer_bullets e game_state.elapsed_time current_time
          game_state.player
          Bullet_pattern.(e.spawn_pattern))
      updated_enemies
  in

  (*puts all the newly spawned bullets from above into active_bullets, in no
    particlar order*)
  let () =
    BatDllist.iter
      (fun bl ->
        List.iter (fun b -> BatDllist.add game_state.enemy_bullets b) bl)
      new_bullets
  in
  (*goes through active enemy and player bullets, filtering out inactive ones,
    and updating the positions of the ones that are still active*)
  let update_enemy_bullets =
    BatDllist.filter_map
      (fun b ->
        if Bullet.(b.spawn_time +. b.ttl) < current_time then None
        else if Bullet.(b.bullet_type = Anchor) then Some b
        else Some (Bullet.move_bullet b current_time))
      game_state.enemy_bullets
  in
  let update_player_bullets =
    BatDllist.filter_map
      (fun b ->
        if Bullet.(b.spawn_time +. b.ttl) < current_time then None
        else if Bullet.(b.bullet_type = Anchor) then Some b
        else Some (Bullet.move_bullet b current_time))
      game_state.player_bullets
  in

  (* resolve player-bullet vs enemy collisions: a bullet that overlaps an enemy
     is consumed and deals 1 damage to that enemy. *)
  let enemies_array = Array.of_list (BatDllist.to_list updated_enemies) in
  let hits = Array.make (Array.length enemies_array) 0 in
  let surviving_player_bullets =
    BatDllist.filter_map
      (fun b ->
        let hit_idx = ref None in
        Array.iteri
          (fun i e ->
            if !hit_idx = None && bullet_hits_enemy b e then hit_idx := Some i)
          enemies_array;
        match !hit_idx with
        | Some i ->
            hits.(i) <- hits.(i) + 1;
            None
        | None -> Some b)
      update_player_bullets
  in
  let enemy_idx = ref 0 in
  let surviving_enemies =
    BatDllist.filter_map
      (fun e ->
        let idx = !enemy_idx in
        incr enemy_idx;
        let new_health = Enemy.(e.health) - hits.(idx) in
        if new_health <= 0 then begin
          incr enemies_killed_this_frame;
          score_gained_this_frame := !score_gained_this_frame + (e.health * 100);
          None
        end
        else if new_health < Enemy.(e.health) then
          Some Enemy.{ e with health = new_health; was_hit = not e.was_hit }
        else Some Enemy.{ e with health = new_health; was_hit = false })
      updated_enemies
  in

  if
    (BatDllist.get surviving_enemies).sprite_filename = "anchor"
    && (BatDllist.get (BatDllist.prev surviving_enemies)).sprite_filename
       = "anchor"
    && (BatDllist.get remaining_enemies).sprite_filename = "anchor"
  then if !finish_time = None then finish_time := Some current_time else ()
  else ();

  (*TODO add new bullets from enemies*)
  (*let updated_enemies = in*)
  (*going to be the same logic*)
  {
    game_state with
    elapsed_time = current_time;
    player_bullets = surviving_player_bullets;
    enemy_bullets = update_enemy_bullets;
    active_enemies = surviving_enemies;
    queued_enemies = remaining_enemies;
    score = game_state.score + !score_gained_this_frame;
    phase =
      (match !finish_time with
      | None -> game_state.phase
      | Some t -> if current_time -. t > 3. then LevelEnd else game_state.phase);
  }

(* TODO bullets should have sizes too, make sure to create bullet size/radii*)

(* iterate through every active enemy bullet and check if its position falls
   within the player's hitbox*)
let detect_collision game_state =
  let hx, hy = Player.get_hitbox_position game_state.player in
  let collided b =
    ((Bullet.(b.x) -. float_of_int hx) ** 2.0)
    +. ((Bullet.(b.y) -. float_of_int hy) ** 2.0)
    < float_of_int
        (game_state.player.hitbox_radius * game_state.player.hitbox_radius)
  in
  BatDllist.exists (fun b -> collided b) game_state.enemy_bullets

(* similar to detect_collision but for enemy bodies instead of bullets *)
let detect_enemy_body_collision game_state =
  let hx, hy = Player.get_hitbox_position game_state.player in
  let px = float_of_int hx in
  let py = float_of_int hy in
  BatDllist.exists
    (fun e ->
      if Enemy.(e.sprite_filename) = "anchor" then false
      else
        let half_w = float_of_int (fst Enemy.(e.sprite_size)) /. 2.0 in
        let half_h = float_of_int (snd Enemy.(e.sprite_size)) /. 2.0 in
        px >= Enemy.(e.x) -. half_w
        && px <= Enemy.(e.x) +. half_w
        && py >= Enemy.(e.y) -. half_h
        && py <= Enemy.(e.y) +. half_h)
    game_state.active_enemies
