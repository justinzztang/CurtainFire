module Player = Player (*will have a player object*)
module Bullet = Bullet (*will handle bullets*)
module Enemy = Enemy (*will handle enemies*)
module Ast = Parse.Ast (*will need to look at ast*)
open Batteries
open Parse.Util

let debug_flag = ref true

type game_phase =
  | StartScreen
  | Playing
  | GameOver
  | LevelEnd

type t = {
  phase : game_phase;
  player_bullets : (string, Bullet.t Dynarray.t) Hashtbl.t;
  enemy_bullets : (string, Bullet.t Dynarray.t) Hashtbl.t;
  active_enemies : (string, Enemy.t Dynarray.t) Hashtbl.t;
  queued_enemies : Enemy.t list;
  (*TODO: parts are for another day*)
  (*must be sorted*)
  player : Player.t;
  elapsed_frames : int;
  window_x : int;
  window_y : int;
  debug_flag : bool;
}

(** Creates a new binding instead of erroring *)
let hashtbl_safe_find tbl tag default_data =
  if Hashtbl.mem tbl tag then Hashtbl.find tbl tag
  else
    let () = Hashtbl.add tbl tag default_data in
    Hashtbl.find tbl tag

let count_hashtable_stuff tbl =
  Hashtbl.fold (fun key value acc -> acc + Dynarray.length value) tbl 0

let finish_time = ref None (*unused*)

(** take enemies from [queue] and add them into [active] if the spawn time is
    before the current frame*)
let rec transfer_queued_enemies
    (active : (string, Enemy.t Dynarray.t) Hashtbl.t) (queue : Enemy.t list)
    (current_frame : int) =
  match queue with
  | e :: es ->
      if e.object_properties.spawn_frame <= current_frame then
        let () =
          Dynarray.add_last
            (hashtbl_safe_find active e.object_properties.tag
               (Dynarray.create ()))
            e
        in
        transfer_queued_enemies active es current_frame
      else queue
  | [] -> []

(**Evaluate a concrete number node to a float*)
let rec eval_number (num : Ast.number) (env : float ref segmented_list) =
  match num with
  | Infinity -> Float.infinity
  | PI -> Float.pi
  | E -> Float.e
  | TAU -> Float.pi *. 2.
  | Variable id -> (
      match sl_find_opt env id with
      | Some n -> !n
      | None ->
          print_endline
            (string_of_sl env (fun (k, v) -> k ^ "," ^ string_of_float !v));
          failwith ("variable " ^ id ^ " not found"))
  | Int n -> float_of_int n
  | Float x -> x

(**Return a float based on some query involving the state of the game*)
let rec eval_query (q : Ast.state_query) state current_frame
    (properties : Object.properties) env =
  let tag_and_bounds_check tag ind table =
    if not (Hashtbl.mem table tag) then failwith ("tag not found: " ^ tag);
    if ind >= Dynarray.length (Hashtbl.find table tag) then
      failwith
        ("index out of bounds for index: " ^ string_of_int ind ^ " and tag: "
       ^ tag)
  in
  match q with
  | Player_X -> state.player.x
  | Player_Y -> state.player.y
  | Self_X -> properties.x
  | Self_Y -> properties.y
  | Self_theta -> properties.theta
  | Self_speed -> properties.speed (*and more properties*)
  | Self_lifetime -> float_of_int (current_frame - properties.spawn_frame)
  | Parent_X ->
      if properties.parent_properties <> None then
        eval_query Self_X state current_frame
          (Option.get properties.parent_properties)
          env
      else properties.x
  | Parent_Y ->
      if properties.parent_properties <> None then
        eval_query Self_Y state current_frame
          (Option.get properties.parent_properties)
          env
      else properties.y
  | Parent_theta ->
      if properties.parent_properties <> None then
        eval_query Self_theta state current_frame
          (Option.get properties.parent_properties)
          env
      else properties.theta
  | Parent_speed ->
      if properties.parent_properties <> None then
        eval_query Self_speed state current_frame
          (Option.get properties.parent_properties)
          env
      else properties.speed
  | Parent_lifetime ->
      if properties.parent_properties <> None then
        eval_query Self_lifetime state current_frame
          (Option.get properties.parent_properties)
          env
      else float_of_int (current_frame - properties.spawn_frame)
  | Lookup_bullet_X (tag, index) ->
      let ind =
        int_of_float (eval_expression index state current_frame properties env)
      in
      tag_and_bounds_check tag ind state.enemy_bullets;
      eval_query Self_X state current_frame
        (Dynarray.get (Hashtbl.find state.enemy_bullets tag) ind)
          .object_properties env
  | Lookup_bullet_Y (tag, index) ->
      let ind =
        int_of_float (eval_expression index state current_frame properties env)
      in
      tag_and_bounds_check tag ind state.enemy_bullets;
      eval_query Self_Y state current_frame
        (Dynarray.get (Hashtbl.find state.enemy_bullets tag) ind)
          .object_properties env
  | Lookup_bullet_theta (tag, index) ->
      let ind =
        int_of_float (eval_expression index state current_frame properties env)
      in
      tag_and_bounds_check tag ind state.enemy_bullets;
      eval_query Self_theta state current_frame
        (Dynarray.get (Hashtbl.find state.enemy_bullets tag) ind)
          .object_properties env
  | Lookup_bullet_speed (tag, index) ->
      let ind =
        int_of_float (eval_expression index state current_frame properties env)
      in
      tag_and_bounds_check tag ind state.enemy_bullets;
      eval_query Self_speed state current_frame
        (Dynarray.get (Hashtbl.find state.enemy_bullets tag) ind)
          .object_properties env
  | Lookup_bullet_lifetime (tag, index) ->
      let ind =
        int_of_float (eval_expression index state current_frame properties env)
      in
      tag_and_bounds_check tag ind state.enemy_bullets;
      eval_query Self_lifetime state current_frame
        (Dynarray.get (Hashtbl.find state.enemy_bullets tag) ind)
          .object_properties env
  | Lookup_enemy_X (tag, index) ->
      let ind =
        int_of_float (eval_expression index state current_frame properties env)
      in
      tag_and_bounds_check tag ind state.active_enemies;
      eval_query Self_X state current_frame
        (Dynarray.get (Hashtbl.find state.active_enemies tag) ind)
          .object_properties env
  | Lookup_enemy_Y (tag, index) ->
      let ind =
        int_of_float (eval_expression index state current_frame properties env)
      in
      tag_and_bounds_check tag ind state.active_enemies;
      eval_query Self_Y state current_frame
        (Dynarray.get (Hashtbl.find state.active_enemies tag) ind)
          .object_properties env
  | Lookup_enemy_theta (tag, index) ->
      let ind =
        int_of_float (eval_expression index state current_frame properties env)
      in
      tag_and_bounds_check tag ind state.active_enemies;
      eval_query Self_theta state current_frame
        (Dynarray.get (Hashtbl.find state.active_enemies tag) ind)
          .object_properties env
  | Lookup_enemy_speed (tag, index) ->
      let ind =
        int_of_float (eval_expression index state current_frame properties env)
      in
      tag_and_bounds_check tag ind state.active_enemies;
      eval_query Self_speed state current_frame
        (Dynarray.get (Hashtbl.find state.active_enemies tag) ind)
          .object_properties env
  | Lookup_enemy_lifetime (tag, index) ->
      let ind =
        int_of_float (eval_expression index state current_frame properties env)
      in
      tag_and_bounds_check tag ind state.active_enemies;
      eval_query Self_lifetime state current_frame
        (Dynarray.get (Hashtbl.find state.active_enemies tag) ind)
          .object_properties env
  | Closest_enemy_X ->
      let mindist = ref Float.infinity in
      let storex = ref None in
      Hashtbl.iter
        (fun tag el ->
          Dynarray.iter
            (fun (e : Enemy.t) ->
              let dist =
                ((e.object_properties.x -. properties.x) ** 2.)
                +. ((e.object_properties.y -. properties.y) ** 2.)
              in
              if dist < !mindist then mindist := dist;
              storex := Some e.object_properties.x)
            el)
        state.active_enemies;
      if !storex = None then Float.infinity else Option.get !storex
  | Closest_enemy_Y ->
      let mindist = ref Float.infinity in
      let storey = ref None in
      Hashtbl.iter
        (fun tag el ->
          Dynarray.iter
            (fun (e : Enemy.t) ->
              let dist =
                ((e.object_properties.x -. properties.x) ** 2.)
                +. ((e.object_properties.y -. properties.y) ** 2.)
              in
              if dist < !mindist then mindist := dist;
              storey := Some e.object_properties.y)
            el)
        state.active_enemies;
      if !storey = None then Float.infinity else Option.get !storey
  | Current_frame -> float_of_int current_frame
  | Elapsed_frames -> float_of_int state.elapsed_frames
  | Active_enemies -> float_of_int (count_hashtable_stuff state.active_enemies)
  | Active_enemies_tag tag ->
      if Hashtbl.mem state.active_enemies tag then
        float_of_int (Dynarray.length (Hashtbl.find state.active_enemies tag))
      else 0.
  | Active_bullets -> float_of_int (count_hashtable_stuff state.enemy_bullets)
  | Active_bullets_tag tag ->
      if Hashtbl.mem state.enemy_bullets tag then
        float_of_int (Dynarray.length (Hashtbl.find state.enemy_bullets tag))
      else 0.
  | Remaining_enemies -> float_of_int (List.length state.queued_enemies)

(** Evaluate a number_expression node to a float, recursively evaluating
    subexpressions*)
and eval_expression (expr : Ast.number_expression) state current_frame
    properties env =
  match expr with
  | Number e -> eval_number e env
  | RandInt (a, b) ->
      let l = eval_expression a state current_frame properties env in
      let r = eval_expression b state current_frame properties env in
      float_of_int (Random.int (int_of_float (floor r -. ceil l))) +. ceil l
  | RandFloat (a, b) ->
      let l = eval_expression a state current_frame properties env in
      let r = eval_expression b state current_frame properties env in
      Random.float (r -. l) +. l
  | Query e -> eval_query e state current_frame properties env
  | Plus (e1, e2) ->
      eval_expression e1 state current_frame properties env
      +. eval_expression e2 state current_frame properties env
  | Minus (e1, e2) ->
      eval_expression e1 state current_frame properties env
      -. eval_expression e2 state current_frame properties env
  | Times (e1, e2) ->
      eval_expression e1 state current_frame properties env
      *. eval_expression e2 state current_frame properties env
  | Div (e1, e2) ->
      eval_expression e1 state current_frame properties env
      /. eval_expression e2 state current_frame properties env
  | Mod (e1, e2) ->
      float_of_int
        (int_of_float (eval_expression e1 state current_frame properties env)
        mod int_of_float (eval_expression e2 state current_frame properties env)
        )
  | Pow (e1, e2) ->
      eval_expression e1 state current_frame properties env
      ** eval_expression e2 state current_frame properties env
  | Log (b, x) ->
      Float.log (eval_expression x state current_frame properties env)
      /. Float.log (eval_expression b state current_frame properties env)
  | Max (e1, e2) ->
      max
        (eval_expression e1 state current_frame properties env)
        (eval_expression e2 state current_frame properties env)
  | Min (e1, e2) ->
      min
        (eval_expression e1 state current_frame properties env)
        (eval_expression e2 state current_frame properties env)
  | Sin e -> Float.sin (eval_expression e state current_frame properties env)
  | Cos e -> Float.cos (eval_expression e state current_frame properties env)
  | Tan e -> Float.tan (eval_expression e state current_frame properties env)
  | Asin e -> Float.asin (eval_expression e state current_frame properties env)
  | Acos e -> Float.acos (eval_expression e state current_frame properties env)
  | Atan e -> Float.atan (eval_expression e state current_frame properties env)
  | Atan2 (e1, e2) ->
      Float.atan2
        (eval_expression e1 state current_frame properties env)
        (eval_expression e2 state current_frame properties env)
  | Abs e -> Float.abs (eval_expression e state current_frame properties env)
  | Sign e ->
      if eval_expression e state current_frame properties env < 0. then -1.
      else 1.
  | Ceil e -> Float.ceil (eval_expression e state current_frame properties env)
  | Floor e ->
      Float.floor (eval_expression e state current_frame properties env)
  | Sqrt e -> Float.sqrt (eval_expression e state current_frame properties env)
  | Dist ((a, b), (c, d)) ->
      let aa = eval_expression a state current_frame properties env in
      let bb = eval_expression b state current_frame properties env in
      let cc = eval_expression c state current_frame properties env in
      let dd = eval_expression d state current_frame properties env in
      Float.sqrt (((aa -. cc) ** 2.) +. ((bb -. dd) ** 2.))

(**Reset the refs in both Elapsed and DoOnce conditions*)
let rec reset_elapsed_helper (boolean_expression : Ast.boolean_expression) =
  match boolean_expression with
  | Elapsed (t, start) -> start := None
  | DoOnce r -> r := false
  | Not b -> reset_elapsed_helper b
  | Or (a, b) ->
      reset_elapsed_helper a;
      reset_elapsed_helper b
  | And (a, b) ->
      reset_elapsed_helper a;
      reset_elapsed_helper b
  | Xor (a, b) ->
      reset_elapsed_helper a;
      reset_elapsed_helper b
  | _ -> ()

(** Evaluate a boolean_expression node to a bool, recursively evaluating
    subexpressions*)
let rec eval_boolean (c : Ast.boolean_expression) (state : t)
    (current_frame : int) (properties : Object.properties) env =
  match c with
  | Instant -> failwith "not supposed to evaluate Instant"
  | DoOnce b -> failwith "not supposed to evaluate DoOnce"
  | True -> true
  | False -> false
  | Before t ->
      float_of_int current_frame
      < eval_expression t state current_frame properties env
  | After t ->
      float_of_int current_frame
      > eval_expression t state current_frame properties env
  | Elapsed (t, start) ->
      let () = if !start = None then start := Some current_frame else () in
      float_of_int (current_frame - Option.get !start)
      > eval_expression t state current_frame properties env
  | Within (x, y, r) ->
      (eval_expression x state current_frame properties env -. properties.x)
      ** 2.0
      +. (eval_expression y state current_frame properties env -. properties.y)
         ** 2.0
      < eval_expression r state current_frame properties env ** 2.0
  | Not c -> not (eval_boolean c state current_frame properties env)
  | Or (c1, c2) ->
      eval_boolean c1 state current_frame properties env
      || eval_boolean c2 state current_frame properties env
  | And (c1, c2) ->
      eval_boolean c1 state current_frame properties env
      && eval_boolean c2 state current_frame properties env
  | Xor (c1, c2) ->
      eval_boolean c1 state current_frame properties env
      <> eval_boolean c2 state current_frame properties env
  | LT (e1, e2) ->
      eval_expression e1 state current_frame properties env
      < eval_expression e2 state current_frame properties env
  | GT (e1, e2) ->
      eval_expression e1 state current_frame properties env
      > eval_expression e2 state current_frame properties env
  | EQ (e1, e2) ->
      eval_expression e1 state current_frame properties env
      = eval_expression e2 state current_frame properties env

(**Perform an update on the object whose [properties] are passed in as an
   argument*)
let execute_action (action : Ast.action) current_frame state
    (properties : Object.properties) (parent_env : float ref segmented_list) =
  match action with
  | Die ->
      properties.spawn_frame <- -1;
      properties.ttl <- 0
  | Sleep -> () (*Do nothing*)
  | Set_all (tag, x, y, theta, s, tangible, opacity) ->
      properties.tag <- tag;
      properties.x <-
        eval_expression x state current_frame properties parent_env;
      properties.y <-
        eval_expression y state current_frame properties parent_env;
      properties.theta <-
        eval_expression theta state current_frame properties parent_env;
      properties.speed <-
        eval_expression s state current_frame properties parent_env;
      properties.tangible <-
        eval_boolean tangible state current_frame properties parent_env;
      properties.opacity <-
        eval_expression opacity state current_frame properties parent_env
  | Set_tag tag -> properties.tag <- tag
  | Set_X x ->
      properties.x <-
        eval_expression x state current_frame properties parent_env
  | Set_Y y ->
      properties.y <-
        eval_expression y state current_frame properties parent_env
  | Set_XY (x, y) ->
      properties.x <-
        eval_expression x state current_frame properties parent_env;
      properties.y <-
        eval_expression y state current_frame properties parent_env
  | Set_angle theta ->
      properties.theta <-
        eval_expression theta state current_frame properties parent_env
  | Set_speed speed ->
      properties.speed <-
        eval_expression speed state current_frame properties parent_env
  | Set_tangible boole ->
      properties.tangible <-
        eval_boolean boole state current_frame properties parent_env
  | Set_opacity op ->
      properties.opacity <-
        max 0.
          (min 1.
             (eval_expression op state current_frame properties parent_env))
  | Set_velocity (theta, s) ->
      properties.theta <-
        eval_expression theta state current_frame properties parent_env;
      properties.speed <-
        eval_expression s state current_frame properties parent_env
  | Define_variable (name, value) ->
      parent_env.values <-
        StringMap.add name
          (ref
             (eval_expression value state current_frame properties parent_env))
          parent_env.values
  (*here, env is a direct reference the parents env, so this adds the variable
    for everyone in the parent scope*)
  | Update_variable (name, value) -> (
      match sl_find_opt parent_env name with
      | Some v ->
          v := eval_expression value state current_frame properties parent_env
      | None ->
          failwith "tried to update a variable that doesnt exist"
          (*variable doesnt even exist*))

(** Fully reset the refs in a behavior and its sub-behaviors and conditions*)
let rec reset_behavior (behavior : Ast.behavior) =
  match behavior with
  | Nothing -> ()
  | Single (action, condition, parent_env) -> (
      match condition with
      | DoOnce b -> b := false
      | _ -> reset_elapsed_helper condition)
  | If_then_else (b0, b1, b2, bi, own_env) ->
      reset_elapsed_helper b0;
      reset_behavior b1;
      reset_behavior b2;
      bi := None
  | Sequence (bs, ind, own_env) ->
      Array.iter (fun b -> reset_behavior b) bs;
      ind := 0
  | For (b, vname, count, ind, own_env) ->
      reset_behavior b;
      ind := 0;
      own_env.values <- StringMap.add vname (ref 0.) own_env.values
  | While (b, cond, own_env) ->
      reset_behavior b;
      reset_elapsed_helper cond

(** Check if a behavior is done*)
let rec check_behavior_done (behavior : Ast.behavior) current_frame state
    (properties : Object.properties) =
  match behavior with
  | Nothing -> true
  | Single (action, condition, parent_env) -> (
      match condition with
      | DoOnce b -> !b
      | _ ->
          if
            (*Instant, so should evaluate and move on within the same frame*)
            condition = Instant
          then
            let () =
              execute_action action current_frame state properties parent_env
            in
            true
          else
            let result =
              eval_boolean condition state current_frame properties parent_env
            in
            result)
  | If_then_else (b0, b1, b2, bi, own_env) ->
      if !bi = None then false (*havent even checked the switch*)
      else if Option.get !bi then (*if true, check #1*)
        let result = check_behavior_done b1 current_frame state properties in
        result
      else
        let result = check_behavior_done b2 current_frame state properties in
        result
  | Sequence (behaviors, index, own_env) ->
      (*past the end of the array or the last behavior is done*)
      let result =
        !index >= Array.length behaviors
        || !index + 1 = Array.length behaviors
           && check_behavior_done behaviors.(!index) current_frame state
                properties
      in
      if result then true else false
  | For (behavior, vname, count, index, own_env) ->
      let upper =
        eval_expression count state current_frame properties own_env
      in
      let result =
        float_of_int !index >= upper
        || !index + 1 = int_of_float upper
           && check_behavior_done behavior current_frame state properties
      in
      result
  | While (b, cond, own_env) ->
      not (eval_boolean cond state current_frame properties own_env)

(**Create (effectively) a deep copy of a boolean_expression*)
let rec deep_copy_condition (cond : Ast.boolean_expression) :
    Ast.boolean_expression =
  match cond with
  | Not c -> Not (deep_copy_condition c)
  | Or (c1, c2) -> Or (deep_copy_condition c1, deep_copy_condition c2)
  | And (c1, c2) -> And (deep_copy_condition c1, deep_copy_condition c2)
  | Xor (c1, c2) -> Xor (deep_copy_condition c1, deep_copy_condition c2)
  | Elapsed (expr, marker) -> Elapsed (expr, ref !marker)
  | DoOnce r -> DoOnce (ref !r)
  | _ -> cond

(**Create (effectively) a deep copy of an action*)
let copy_action (act : Ast.action) : Ast.action =
  match act with
  | Set_tangible bx -> Set_tangible (deep_copy_condition bx)
  | Set_all (t, xx, yy, ang, sp, tang, opa) ->
      Set_all (t, xx, yy, ang, sp, deep_copy_condition tang, opa)
  | a -> a

(**Create (effectively) a deep copy of a behavior*)
let rec deep_copy_behavior (b : Ast.behavior) parent_env : Ast.behavior =
  match b with
  | Nothing -> Nothing
  | Single (action, cond, penv) ->
      Single (copy_action action, deep_copy_condition cond, parent_env)
  | If_then_else (b0, b1, b2, bi, own_env) ->
      let envcopy = { values = own_env.values; next = Some parent_env } in
      If_then_else
        ( deep_copy_condition b0,
          deep_copy_behavior b1 envcopy,
          deep_copy_behavior b2 envcopy,
          ref !bi,
          envcopy )
  | Sequence (bs, index, own_env) ->
      let envcopy = { values = own_env.values; next = Some parent_env } in
      Sequence
        ( Array.map (fun b -> deep_copy_behavior b envcopy) bs,
          ref !index,
          envcopy )
  | For (b, vname, count, index, own_env) ->
      let envcopy = { values = own_env.values; next = Some parent_env } in
      For (deep_copy_behavior b envcopy, vname, count, ref !index, envcopy)
  | While (b, bx, own_env) ->
      let envcopy = { values = own_env.values; next = Some parent_env } in
      While (deep_copy_behavior b envcopy, deep_copy_condition bx, envcopy)

(**Evaluate a behavior, identifying and executing the current action to be
   performed, which will update the [properties] of the object*)
let rec eval_behavior (behavior : Ast.behavior) current_frame state
    (properties : Object.properties) (parent_env : float ref segmented_list) =
  if check_behavior_done behavior current_frame state properties then ()
  else
    match behavior with
    | Nothing -> ()
    | Single (action, condition, parent_env) ->
        (match condition with
        | DoOnce b -> b := true
        | _ -> ());
        (*if we are executing this, that means it was false*)
        execute_action action current_frame state properties parent_env
        (*must be able to modify the current env, both updating kv pairs and
          creating new ones*)
    | If_then_else (b0, b1, b2, bi, own_env) ->
        if !bi = None then
          let () =
            bi := Some (eval_boolean b0 state current_frame properties own_env)
          in
          eval_behavior behavior current_frame state properties
            parent_env (*we are retrying from the beginning, same parent env*)
        else if Option.get !bi then
          eval_behavior b1 current_frame state properties own_env
          (*must be able to update kv pairs in the current env, but any new ones
            are strictly in the subsequence copy*)
        else eval_behavior b2 current_frame state properties own_env
          (*must be able to update kv pairs in the current env, but any new ones
            are strictly in the subsequence copy*)
    | Sequence (behaviors, index, own_env) ->
        if check_behavior_done behaviors.(!index) current_frame state properties
        then (
          reset_behavior behaviors.(!index);
          let () = incr index in
          eval_behavior behavior current_frame state properties parent_env
          (* a restart *))
        else
          eval_behavior behaviors.(!index) current_frame state properties
            own_env
          (*must be able to update kv pairs in the current env, but any new ones
            are strictly in the subsequence copy*)
    | While (b, cond, own_env) ->
        if check_behavior_done b current_frame state properties then
          reset_behavior b;
        eval_behavior b current_frame state properties own_env
    | For (b, vname, count, index, own_env) ->
        if check_behavior_done b current_frame state properties then (
          reset_behavior b;
          let () = incr index in
          eval_behavior behavior current_frame state properties parent_env
          (*must be able to update kv pairs in the current env, but any new ones
            are strictly in the subsequence copy*))
        else
          eval_behavior b current_frame state properties
            {
              own_env with
              values =
                StringMap.add vname (ref (float_of_int !index)) own_env.values;
            }
(*must be able to update kv pairs in the current env, but any new ones are
  strictly in the subsequence copy*)

(**Update a bullet according to its movement properties and behavior*)
let move_bullet (bullet : Bullet.t) current_frame state =
  let () =
    eval_behavior bullet.behavior current_frame state bullet.object_properties
      bullet.parent_env
  in
  (*we know what the angle and speed is now*)
  let dx, dy =
    ( cos bullet.object_properties.theta *. bullet.object_properties.speed,
      sin bullet.object_properties.theta *. bullet.object_properties.speed )
  in
  match bullet.bullet_type with
  | Trail (n, intv, memory) ->
      let newmem =
        BatDeque.rear
          (BatDeque.cons
             (bullet.object_properties.x +. dx, bullet.object_properties.y +. dy)
             memory)
      in
      let newmem = fst (Option.get newmem) in
      {
        bullet with
        bullet_type = Trail (n, intv, newmem);
        object_properties =
          {
            bullet.object_properties with
            x = bullet.object_properties.x +. dx;
            y = bullet.object_properties.y +. dy;
            (*theta and speed already updated*)
          };
      }
  | _ ->
      {
        bullet with
        object_properties =
          {
            bullet.object_properties with
            x = bullet.object_properties.x +. dx;
            y = bullet.object_properties.y +. dy;
            (*theta and speed already updated*)
          };
      }

(**Update an enemy according to its movement properties and behavior*)
let update_enemy (enemy : Enemy.t) current_frame state =
  let init_env = { values = StringMap.empty; next = None } in
  let () =
    eval_behavior enemy.behavior current_frame state enemy.object_properties
      { values = StringMap.empty; next = Some init_env }
  in
  let dx, dy =
    ( cos enemy.object_properties.theta *. enemy.object_properties.speed,
      sin enemy.object_properties.theta *. enemy.object_properties.speed )
  in
  {
    enemy with
    object_properties =
      {
        enemy.object_properties with
        x = enemy.object_properties.x +. dx;
        y = enemy.object_properties.y +. dy;
        (*theta and speed already updated*)
      };
  }

(**Reset all the refs in a pattern and its sub-patterns and conditions*)
let rec reset_pattern (p : Ast.bullet_pattern) =
  match p with
  | Definition (_, _, _) -> ()
  | Update (_, _, _) -> ()
  | Nothing c -> (
      match c with
      | DoOnce b -> b := false
      | _ -> reset_elapsed_helper c)
  | Bullet (t, x, y, s, tt, ttl, color, opacity, b, bh, r, a, cond, penv) -> (
      match cond with
      | DoOnce b -> b := false
      | _ -> reset_elapsed_helper cond)
  | Conditional (bp, cond, own_env) ->
      reset_elapsed_helper cond;
      reset_pattern bp
  | If_then_else (switch, p1, p2, bi, own_env) ->
      reset_elapsed_helper switch;
      reset_pattern p1;
      reset_pattern p2;
      bi := None
  | Timed (st, et, p, env) -> reset_pattern p
  | Modify_tag (p, t, env) -> reset_pattern p
  | Modify_active (p, v, env) ->
      reset_pattern p;
      reset_elapsed_helper v
  | Modify (p, mods, env) -> reset_pattern p
  | Iterate (vname, count, bp, bp_copies, own_env) ->
      if !bp_copies <> None then
        List.iter (fun (p, e) -> reset_pattern p) (Option.get !bp_copies)
  | Combo (bps, own_env) -> List.iter reset_pattern bps
  | Sequence (bps, index, own_env) ->
      Array.iter reset_pattern bps;
      index := 0
  | For (bp, vname, count, index, own_env) ->
      reset_pattern bp;
      index := 0;
      own_env.values <- StringMap.add vname (ref 0.) own_env.values
  | While (bp, cond, own_env) ->
      reset_pattern bp;
      reset_elapsed_helper cond
  | DEBUG -> ()

(**Check if a pattern is done*)
let rec check_pattern_done (enemy : Enemy.t) current_frame state
    (pattern : Ast.bullet_pattern) (parent_env : float ref segmented_list) =
  match pattern with
  | Definition (name, value, penv) ->
      let () =
        penv.values <-
          StringMap.add name
            (ref
               (eval_expression value state current_frame
                  enemy.object_properties penv))
            penv.values
      in
      true
  | Update (name, value, penv) ->
      let () =
        match sl_find_opt penv name with
        | Some v ->
            v :=
              eval_expression value state current_frame enemy.object_properties
                penv
        | None ->
            failwith
              "tried to update a variable that doesnt exist in bullet pattern"
      in
      true
  | Nothing cond -> (
      match cond with
      | DoOnce b ->
          !b
          (*if we're checking, weve moved on since the last time it executed
            since DoOnce is only in "terminal" actions*)
      | _ ->
          eval_boolean cond state current_frame enemy.object_properties
            parent_env)
  | Bullet
      ( tag,
        x,
        y,
        speed,
        theta,
        ttl,
        color,
        opacity,
        btype,
        behavior,
        radius,
        active,
        condition,
        parent_env ) -> (
      match condition with
      | DoOnce b -> !b
      | _ ->
          eval_boolean condition state current_frame enemy.object_properties
            parent_env)
  | Conditional (bp, cond, own_env) ->
      check_pattern_done enemy current_frame state bp own_env
  | If_then_else (switch, bp1, bp2, bi, own_env) ->
      if !bi = None then false
      else if Option.get !bi then
        check_pattern_done enemy current_frame state bp1 own_env
      else check_pattern_done enemy current_frame state bp2 own_env
  | Timed (st, et, bp, own_env) ->
      float_of_int current_frame
      > eval_expression et state current_frame enemy.object_properties own_env
  | Modify_tag (bp, tag, own_env) ->
      check_pattern_done enemy current_frame state bp own_env
  | Modify_active (bp, bx, own_env) ->
      check_pattern_done enemy current_frame state bp own_env
  | Modify (bp, mods, own_env) ->
      check_pattern_done enemy current_frame state bp own_env
  | Iterate (vname, count, bp, bp_copies, own_env) ->
      if !bp_copies = None then false
      else
        List.fold_left
          (fun acc elt -> acc && elt)
          true
          (List.map
             (fun (bp, env) ->
               check_pattern_done enemy current_frame state bp env)
             (Option.get !bp_copies))
  | Combo (bps, own_env) ->
      let result =
        List.fold_left
          (fun acc elt -> acc && elt)
          true
          (List.map
             (fun p ->
               match (p : Ast.bullet_pattern) with
               | Definition (n, v, p) ->
                   true (*dont check again or else it will redefine*)
               | Update (n, v, p) -> true
               | _ -> check_pattern_done enemy current_frame state p own_env)
             bps)
      in
      result
  | Sequence (bps, index, own_env) ->
      let result =
        !index >= Array.length bps
        || !index + 1 = Array.length bps
           && check_pattern_done enemy current_frame state bps.(!index) own_env
      in
      result
  | For (bp, vname, count, index, own_env) ->
      let upper =
        eval_expression count state current_frame enemy.object_properties
          own_env
      in
      let result =
        float_of_int !index >= upper
        || !index + 1 = int_of_float upper
           && check_pattern_done enemy current_frame state bp own_env
      in
      result
  | While (bp, cond, own_env) ->
      not
        (eval_boolean cond state current_frame enemy.object_properties own_env)
  | DEBUG -> false

(**Create (effectively) a deep copy of a bullet_pattern*)
let rec deep_copy_bp (p : Ast.bullet_pattern)
    (parent_env : float ref segmented_list) : Ast.bullet_pattern =
  match p with
  | Definition (name, value, penv) -> Definition (name, value, parent_env)
  | Update (name, value, penv) -> Update (name, value, parent_env)
  | Nothing bx -> Nothing bx
  | Bullet
      ( tag,
        x,
        y,
        speed,
        theta,
        ttl,
        color,
        opacity,
        btype,
        behavior,
        radius,
        active,
        condition,
        penv ) ->
      Bullet
        ( tag,
          x,
          y,
          speed,
          theta,
          ttl,
          color,
          opacity,
          btype,
          deep_copy_behavior behavior parent_env,
          radius,
          active,
          deep_copy_condition condition,
          parent_env )
  | Conditional (bp, bx, own_env) ->
      let envcopy = { values = own_env.values; next = Some parent_env } in
      Conditional (deep_copy_bp bp envcopy, deep_copy_condition bx, envcopy)
  | If_then_else (b0, b1, b2, bi, own_env) ->
      let envcopy = { values = own_env.values; next = Some parent_env } in
      If_then_else
        ( deep_copy_condition b0,
          deep_copy_bp b1 envcopy,
          deep_copy_bp b2 envcopy,
          ref !bi,
          envcopy )
      (*need to make a completely new switch marker, since updating the original
        node would update this*)
  | Timed (st, et, bp, own_env) ->
      let envcopy = { values = own_env.values; next = Some parent_env } in
      Timed (st, et, deep_copy_bp bp envcopy, envcopy)
  | Modify_tag (bp, tag, own_env) ->
      let envcopy = { values = own_env.values; next = Some parent_env } in
      Modify_tag (deep_copy_bp bp envcopy, tag, envcopy)
  | Modify_active (bp, bx, own_env) ->
      let envcopy = { values = own_env.values; next = Some parent_env } in
      Modify_active (deep_copy_bp bp envcopy, deep_copy_condition bx, envcopy)
  | Modify (bp, mods, own_env) ->
      let envcopy = { values = own_env.values; next = Some parent_env } in
      Modify (deep_copy_bp bp envcopy, mods, envcopy)
  | Iterate (vname, count, bp, bp_copies, own_env) ->
      let envcopy = { values = own_env.values; next = Some parent_env } in
      Iterate (vname, count, deep_copy_bp bp envcopy, ref !bp_copies, envcopy)
  | Combo (bps, own_env) ->
      let envcopy = { values = own_env.values; next = Some parent_env } in
      Combo (List.map (fun bp -> deep_copy_bp bp envcopy) bps, envcopy)
  | Sequence (bps, index, own_env) ->
      let envcopy = { values = own_env.values; next = Some parent_env } in
      Sequence
        (Array.map (fun bp -> deep_copy_bp bp envcopy) bps, ref !index, envcopy)
      (*make a new index, so other copies dont update this one*)
  | For (bp, vname, count, index, own_env) ->
      let envcopy = { values = own_env.values; next = Some parent_env } in
      For (deep_copy_bp bp envcopy, vname, count, ref !index, envcopy)
      (*copy*)
  | While (bp, cond, own_env) ->
      let envcopy = { values = own_env.values; next = Some parent_env } in
      While (deep_copy_bp bp envcopy, deep_copy_condition cond, envcopy)
  | _ -> p (*none of the others have interesting copy behaviors*)

(*whatever it works*)
type mod_value_list = {
  x : float;
  y : float;
  speed : float;
  theta : float;
  ttl : float;
  op : float;
}

let rec apply_mods_function expr (vl : mod_value_list) state current_frame
    properties parent_env : float =
  let env_copy =
    {
      values =
        StringMap.empty
        |> StringMap.add "@X" (ref vl.x)
        |> StringMap.add "@Y" (ref vl.y)
        |> StringMap.add "@S" (ref vl.speed)
        |> StringMap.add "@A" (ref vl.theta)
        |> StringMap.add "@TTL" (ref vl.ttl)
        |> StringMap.add "@O" (ref vl.op);
      next = Some parent_env;
    }
  in
  eval_expression expr state current_frame properties env_copy

(**Evaluate a bullet pattern, identifying and executing the current action to be
   performed, which will return a list of bullets to be spawned*)
let rec evaluate_pattern enemy previous_time current_frame state
    (pattern : Ast.bullet_pattern) (parent_env : float ref segmented_list)
    (modlist : Ast.mod_list list) : Bullet.t tree_list =
  if check_pattern_done enemy current_frame state pattern parent_env then Empty
  else
    match pattern with
    | Definition (_, _, _) -> Empty (*this just jumps into check pattern done*)
    | Update (_, _, _) -> Empty (*this just jumps into check pattern done*)
    | Nothing condition -> Empty
    | Bullet
        ( tag,
          x,
          y,
          speed,
          theta,
          ttl,
          color,
          opacity,
          btype,
          behavior,
          radius,
          tangible,
          condition,
          parent_env ) ->
        (match condition with
        | DoOnce b -> b := true
        | _ -> ());

        (*if we are executing this, that means it was false*)
        let new_tag = ref None in
        let new_x =
          ref
            (eval_expression x state current_frame enemy.object_properties
               parent_env)
        in
        let new_y =
          ref
            (eval_expression y state current_frame enemy.object_properties
               parent_env)
        in
        let new_s =
          ref
            (eval_expression speed state current_frame enemy.object_properties
               parent_env)
        in
        let new_t =
          ref
            (eval_expression theta state current_frame enemy.object_properties
               parent_env)
        in
        let new_ttl =
          ref
            (eval_expression ttl state current_frame enemy.object_properties
               parent_env)
        in
        let new_op =
          ref
            (eval_expression opacity state current_frame enemy.object_properties
               parent_env)
        in
        let new_tangible = ref None in

        (*mods are applied earliest first, so outer layers of Modify are assumed
          to have been applied by the time you get to an inner layer*)
        let rec apply_mods (ml : Ast.mod_list list) =
          match ml with
          | [] -> ()
          | m :: ms ->
              let temp_x = ref !new_x in
              let temp_y = ref !new_y in
              let temp_s = ref !new_s in
              let temp_t = ref !new_t in
              let temp_ttl = ref !new_ttl in
              let temp_op = ref !new_op in

              List.iter
                (fun (prop, expr) ->
                  match prop with
                  | Ast.X ->
                      temp_x :=
                        apply_mods_function expr
                          {
                            x = !new_x;
                            y = !new_y;
                            speed = !new_s;
                            theta = !new_t;
                            ttl = !new_ttl;
                            op = !new_op;
                          }
                          state current_frame enemy.object_properties parent_env
                  | Ast.Y ->
                      temp_y :=
                        apply_mods_function expr
                          {
                            x = !new_x;
                            y = !new_y;
                            speed = !new_s;
                            theta = !new_t;
                            ttl = !new_ttl;
                            op = !new_op;
                          }
                          state current_frame enemy.object_properties parent_env
                  | Ast.Speed ->
                      temp_s :=
                        apply_mods_function expr
                          {
                            x = !new_x;
                            y = !new_y;
                            speed = !new_s;
                            theta = !new_t;
                            ttl = !new_ttl;
                            op = !new_op;
                          }
                          state current_frame enemy.object_properties parent_env
                  | Ast.Theta ->
                      temp_t :=
                        apply_mods_function expr
                          {
                            x = !new_x;
                            y = !new_y;
                            speed = !new_s;
                            theta = !new_t;
                            ttl = !new_ttl;
                            op = !new_op;
                          }
                          state current_frame enemy.object_properties parent_env
                  | Ast.TTL ->
                      temp_ttl :=
                        apply_mods_function expr
                          {
                            x = !new_x;
                            y = !new_y;
                            speed = !new_s;
                            theta = !new_t;
                            ttl = !new_ttl;
                            op = !new_op;
                          }
                          state current_frame enemy.object_properties parent_env
                  | Ast.Opacity ->
                      temp_op :=
                        apply_mods_function expr
                          {
                            x = !new_x;
                            y = !new_y;
                            speed = !new_s;
                            theta = !new_t;
                            ttl = !new_ttl;
                            op = !new_op;
                          }
                          state current_frame enemy.object_properties parent_env
                  | _ ->
                      failwith "tag or active should not have an apply function")
                m.mod_number_property;
              if m.mod_tag = None then () else new_tag := m.mod_tag;
              new_x := !temp_x;
              new_y := !temp_y;
              new_s := !temp_s;
              new_t := !temp_t;
              new_ttl := !temp_ttl;
              new_op := !temp_op;
              if m.mod_tangible = None then ()
              else new_tangible := m.mod_tangible;
              apply_mods ms
        in
        apply_mods (List.rev modlist);

        let rec init_deque n v acc =
          if n > 0 then init_deque (n - 1) v (BatDeque.cons v acc) else acc
        in

        let bt =
          match btype with
          | Circle -> Bullet.(Circle)
          | Arrow -> Bullet.(Arrow)
          | Knife -> Bullet.(Knife)
          | Laser leng ->
              Bullet.(
                Laser
                  (eval_expression leng state current_frame
                     enemy.object_properties parent_env))
          | Trail (reps, intv) ->
              let reps =
                int_of_float
                  (eval_expression reps state current_frame
                     enemy.object_properties parent_env)
              in
              let intv =
                int_of_float
                  (eval_expression intv state current_frame
                     enemy.object_properties parent_env)
              in
              Bullet.(
                Trail
                  ( reps,
                    intv,
                    init_deque (reps * intv) (!new_x, !new_y) BatDeque.empty ))
        in

        let color =
          match color with
          | White -> Bullet.(White)
          | Red -> Bullet.(Red)
          | Orange -> Bullet.(Orange)
          | Yellow -> Bullet.(Yellow)
          | Green -> Bullet.(Green)
          | Cyan -> Bullet.(Cyan)
          | Blue -> Bullet.(Blue)
          | Purple -> Bullet.(Purple)
        in

        Leaf
          Bullet.(
            create_bullet
              (if !new_tag = None then tag else Option.get !new_tag)
              !new_x !new_y !new_s !new_t current_frame (int_of_float !new_ttl)
              color bt
              (deep_copy_behavior behavior parent_env)
              (eval_expression radius state current_frame
                 enemy.object_properties parent_env)
              (if !new_tangible = None then
                 eval_boolean tangible state current_frame
                   enemy.object_properties parent_env
               else
                 eval_boolean (Option.get !new_tangible) state current_frame
                   enemy.object_properties parent_env)
              !new_op (Some enemy.object_properties) parent_env)
    | Conditional (bp, cond, own_env) ->
        if eval_boolean cond state current_frame enemy.object_properties own_env
        then
          evaluate_pattern enemy previous_time current_frame state bp own_env
            modlist
        else Empty (*only execute if condition met*)
    | If_then_else (switch, bp1, bp2, bi, own_env) ->
        if !bi = None then
          (*if bi not set, set it, then step into the right behavior*)
          let () =
            bi :=
              Some
                (eval_boolean switch state current_frame enemy.object_properties
                   own_env)
          in
          evaluate_pattern enemy previous_time current_frame state pattern
            parent_env
            modlist (*we are retrying from the beginning, same parent env*)
        else if Option.get !bi then
          evaluate_pattern enemy previous_time current_frame state bp1 own_env
            modlist
          (*must be able to update kv pairs in the current env, but any new ones
            are strictly in the subsequence copy*)
        else
          evaluate_pattern enemy previous_time current_frame state bp2 own_env
            modlist
    | Timed (st, et, bp, own_env) ->
        if
          float_of_int current_frame
          <= eval_expression et state current_frame enemy.object_properties
               own_env
          && float_of_int current_frame
             >= eval_expression st state current_frame enemy.object_properties
                  own_env
        then
          evaluate_pattern enemy previous_time current_frame state bp own_env
            modlist
        else Empty (*if in range, execute inside*)
    | Modify_tag (bp, tag, own_env) ->
        evaluate_pattern enemy previous_time current_frame state bp own_env
          ({ mod_tag = Some tag; mod_number_property = []; mod_tangible = None }
          :: modlist)
    | Modify_active (bp, bx, own_env) ->
        (*execute inside with new mod*)
        evaluate_pattern enemy previous_time current_frame state bp own_env
          ({ mod_tag = None; mod_number_property = []; mod_tangible = Some bx }
          :: modlist)
    | Modify (bp, mods, own_env) ->
        (*execute inside with new mod*)
        let new_modlist = mods :: modlist in
        evaluate_pattern enemy previous_time current_frame state bp own_env
          new_modlist
    | Iterate (vname, count, bp, bp_copies, own_env) ->
        let cnt =
          int_of_float
            (eval_expression count state current_frame enemy.object_properties
               own_env)
        in
        if !bp_copies = None || List.length (Option.get !bp_copies) <> cnt then
          bp_copies :=
            Some
              (List.init cnt (fun i ->
                   let indexvarenv =
                     {
                       own_env with
                       values =
                         StringMap.add vname
                           (ref (float_of_int i))
                           own_env.values;
                     }
                   in
                   (deep_copy_bp bp indexvarenv, indexvarenv)));
        Join
          (List.map
             (fun (bpp, env) ->
               evaluate_pattern enemy previous_time current_frame state bpp env
                 modlist)
             (Option.get !bp_copies))
    | Combo (bps, own_env) ->
        Join
          (List.map
             (fun bp ->
               evaluate_pattern enemy previous_time current_frame state bp
                 own_env modlist)
             bps)
        (*flatten the result of many*)
    | Sequence (bps, index, own_env) ->
        if check_pattern_done enemy current_frame state bps.(!index) own_env
        then (
          reset_pattern bps.(!index);
          let () = incr index in
          evaluate_pattern enemy previous_time current_frame state pattern
            parent_env modlist
          (* a restart *))
        else
          evaluate_pattern enemy previous_time current_frame state bps.(!index)
            own_env modlist
    | For (bp, vname, count, index, own_env) ->
        if check_pattern_done enemy current_frame state bp own_env then (
          reset_pattern bp;
          let () =
            incr index;
            own_env.values <-
              StringMap.add vname (ref (float_of_int !index)) own_env.values
          in
          evaluate_pattern enemy previous_time current_frame state pattern
            parent_env modlist)
        else
          evaluate_pattern enemy previous_time current_frame state bp own_env
            modlist
    | While (p, cond, own_env) ->
        if check_pattern_done enemy current_frame state p own_env then
          reset_pattern p;
        evaluate_pattern enemy previous_time current_frame state p own_env
          modlist
    | DEBUG -> Empty

(** [update_state current_time game_state] will update the current state from
    the last state based on the amount of time that has passed*)
let update_state (current_frame : int) game_state =
  debug_flag := game_state.debug_flag;
  let remaining_enemies =
    transfer_queued_enemies game_state.active_enemies game_state.queued_enemies
      current_frame
  in

  (*move enemies or despawn them*)
  let () =
    Hashtbl.filter_map_inplace
      (fun tag el ->
        Some
          (Dynarray.filter_map
             (fun e ->
               if
                 Enemy.(
                   e.object_properties.spawn_frame + e.object_properties.ttl)
                 < current_frame
                 || Enemy.(e.health < 1)
               then None
               else if e.object_properties.tag <> tag then (
                 Dynarray.add_last
                   (hashtbl_safe_find game_state.active_enemies
                      e.object_properties.tag (Dynarray.create ()))
                   (update_enemy e current_frame game_state);
                 None)
               else Some (update_enemy e current_frame game_state))
             el))
      game_state.active_enemies
  in
  (*spawning bullets requires going through each enemy, and checking what
    bullets they need to spawn*)

  let () =
    Hashtbl.iter
      (fun tag el ->
        Dynarray.iter
          (fun e ->
            let new_bullets =
              evaluate_pattern e game_state.elapsed_frames current_frame
                game_state
                Enemy.(e.spawn_pattern)
                { values = StringMap.empty; next = None }
                [
                  Ast.
                    {
                      mod_tag = None;
                      mod_number_property = [];
                      mod_tangible = None;
                    };
                ]
            in
            List.iter
              (fun b ->
                Dynarray.add_last
                  (hashtbl_safe_find game_state.enemy_bullets
                     Bullet.(b.object_properties).tag (Dynarray.create ()))
                  b)
              (tree_to_list new_bullets))
          el)
      game_state.active_enemies
  in
  (*goes through active enemy and player bullets, filtering out inactive ones,
    and updating the positions of the ones that are still active*)
  let () =
    Hashtbl.filter_map_inplace
      (fun tag bl ->
        Some
          (Dynarray.filter_map
             (fun b ->
               if
                 Bullet.(
                   b.object_properties.spawn_frame + b.object_properties.ttl)
                 < current_frame
               then None
               else Some (move_bullet b current_frame game_state))
             bl))
      game_state.enemy_bullets
  in
  let () =
    Hashtbl.filter_map_inplace
      (fun tag bl ->
        Some
          (Dynarray.filter_map
             (fun b ->
               if
                 Bullet.(
                   b.object_properties.spawn_frame + b.object_properties.ttl)
                 < current_frame
               then None
               else Some (move_bullet b current_frame game_state))
             bl))
      game_state.player_bullets
  in

  let dist_from_line_segment_squared x1 y1 x2 y2 px py =
    (*project p onto the line segment*)
    let l2 = ((x1 -. x2) ** 2.) +. ((y1 -. y2) ** 2.) in
    if l2 = 0. then ((x1 -. px) ** 2.) +. ((y1 -. py) ** 2.)
    else
      let dotpl = ((px -. x1) *. (x2 -. x1)) +. ((py -. y1) *. (y2 -. y1)) in
      let t = max 0. (min 1. (dotpl /. l2)) in
      let projx = x1 +. (t *. (x2 -. x1)) in
      let projy = y1 +. (t *. (y2 -. y1)) in
      ((projx -. px) ** 2.) +. ((projy -. py) ** 2.)
  in

  let detect_player_collision state =
    let hx, hy = Player.get_hitbox_position state.player in
    let collided (b : Bullet.t) =
      if b.object_properties.tangible = false then false
      else
        match b.bullet_type with
        | Circle | Knife | Arrow ->
            ((b.object_properties.x -. hx) ** 2.)
            +. ((b.object_properties.y -. hy) ** 2.)
            < (state.player.hitbox_radius +. b.hitbox_radius) ** 2.
        | Laser len ->
            let endx =
              b.object_properties.x +. (len *. cos b.object_properties.theta)
            in
            let endy =
              b.object_properties.y +. (len *. sin b.object_properties.theta)
            in
            dist_from_line_segment_squared b.object_properties.x
              b.object_properties.y endx endy hx hy
            < (state.player.hitbox_radius +. b.hitbox_radius) ** 2.
        | Trail (reps, intv, memory) ->
            (*memory[0] is right now, memory[1] is 1 frame ago*)
            let prevx = ref None in
            let prevy = ref None in
            let ans = ref false in
            BatDeque.iteri
              (fun i (x, y) ->
                if i mod intv <> 0 then ()
                else if !prevx = None then (
                  ans :=
                    !ans
                    || dist_from_line_segment_squared x y x y hx hy
                       < (state.player.hitbox_radius +. b.hitbox_radius) ** 2.;
                  prevx := Some x;
                  prevy := Some y)
                else (
                  ans :=
                    !ans
                    || dist_from_line_segment_squared x y (Option.get !prevx)
                         (Option.get !prevy) hx hy
                       < (state.player.hitbox_radius +. b.hitbox_radius) ** 2.;
                  prevx := Some x;
                  prevy := Some y))
              memory;
            !ans
    in
    Hashtbl.fold
      (fun tag bl acc -> acc || Dynarray.exists collided bl)
      state.enemy_bullets false
  in

  let bullet_hits_enemy state (bullet : Bullet.t) =
    let dat = ref None in

    Hashtbl.iter
      (fun tag el ->
        Dynarray.iter
          (fun (e : Enemy.t) ->
            let hw = float_of_int (fst e.sprite_size) /. 2. in
            let hh = float_of_int (snd e.sprite_size) /. 2. in
            if
              bullet.object_properties.x
              >= e.object_properties.x -. hw -. bullet.hitbox_radius
              && bullet.object_properties.x
                 <= e.object_properties.x +. hw +. bullet.hitbox_radius
              && bullet.object_properties.y
                 >= e.object_properties.y -. hh -. bullet.hitbox_radius
              && bullet.object_properties.y
                 <= e.object_properties.y +. hh +. bullet.hitbox_radius
            then if !dat = None then dat := Some e)
          el)
      state.active_enemies;
    !dat
  in

  if detect_player_collision game_state && not !debug_flag then
    Hashtbl.clear game_state.enemy_bullets;

  (*detect player bullets colliding with enemies*)
  let () =
    Hashtbl.filter_map_inplace
      (fun tag bl ->
        Some
          (Dynarray.filter_map
             (fun b ->
               let enemyhit = bullet_hits_enemy game_state b in
               if enemyhit = None then Some b
               else (
                 (Option.get enemyhit).health <-
                   (Option.get enemyhit).health - 1;
                 None))
             bl))
      game_state.player_bullets
  in

  {
    game_state with
    elapsed_frames = current_frame;
    player_bullets = game_state.player_bullets;
    enemy_bullets = game_state.enemy_bullets;
    active_enemies = game_state.active_enemies;
    queued_enemies = remaining_enemies;
    phase =
      (match !finish_time with
      | None -> game_state.phase
      | Some t -> if current_frame - t > 3 then LevelEnd else game_state.phase);
  }

let get_active_enemies state =
  let elist = ref [] in
  let () =
    Hashtbl.iter
      (fun tag el -> Dynarray.iter (fun e -> elist := e :: !elist) el)
      state.active_enemies
  in
  !elist

let get_enemy_bullets state =
  let blist = ref [] in
  let () =
    Hashtbl.iter
      (fun tag bl -> Dynarray.iter (fun b -> blist := b :: !blist) bl)
      state.enemy_bullets
  in
  !blist

let apply_function_to_enemy_bullets state f =
  Hashtbl.iter
    (fun tag bl -> Dynarray.iter (fun b -> f b) bl)
    state.enemy_bullets

let get_player_bullets state =
  let blist = ref [] in
  let () =
    Hashtbl.iter
      (fun tag bl -> Dynarray.iter (fun b -> blist := b :: !blist) bl)
      state.player_bullets
  in
  !blist

let apply_function_to_player_bullets state f =
  Hashtbl.iter
    (fun tag bl -> Dynarray.iter (fun b -> f b) bl)
    state.player_bullets
