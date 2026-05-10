type velocity =
  | Custom of (float -> float) * (float -> float)
  (*angle function, speed function*)
  | CustomDXDY of (float -> float) * (float -> float) (*dx func, dy func*)
  | Linear of float * (float -> float) (*fixed angle, speed function*)
  | Point of (float * float) * (float -> float)
    (*point to calculate angle, speed function*)
  | Sequence of (velocity * float) list (*[velocity, time limit]*)
  | Orbit of (float * float) * (float -> float) * (float -> float)
  | Combo of velocity list
(*pivot point, angular velocity function, speed away from pivot function*)

let rec eval_velocity velocity current_time x y =
  match velocity with
  | Linear (theta, vt) -> (-.theta, vt current_time)
  | Point ((t_x, t_y), vt) ->
      let dx = t_x -. x in
      let dy = y -. t_x in
      let angle = atan2 dy dx in
      (-.angle, vt current_time)
  | Sequence ((vh, cond) :: vt) ->
      if current_time > cond then eval_velocity (Sequence vt) current_time x y
      else eval_velocity vh current_time x y
  | Sequence [] -> (0.0, 0.0)
  | Orbit ((px, py), wt, vt) ->
      let dx = if x -. px = 0. then 0.001 else x -. px in
      let dy = if y -. py = 0. then 0.001 else y -. py in
      let dist = sqrt ((dx ** 2.) +. (dy ** 2.)) in
      let newvt t = sqrt ((vt t ** 2.) +. ((dist *. wt t) ** 2.)) in
      let newwt t =
        atan2 dx dy +. atan2 (wt t *. dist) (vt t) -. (Float.pi /. 2.)
      in
      (-.newwt current_time, newvt current_time)
  | Custom (wt, vt) -> (-.wt current_time, vt current_time)
  | CustomDXDY (xt, yt) ->
      ( atan2 (-.yt current_time) (xt current_time),
        sqrt ((yt current_time ** 2.) +. (xt current_time ** 2.)) )
  | Combo vl ->
      List.fold_left
        (fun (a, b) (x, y) ->
          (*print_endline (string_of_float x ^ " X " ^ string_of_float y);*)
          (a +. x, b +. y))
        (0., 0.)
        (List.map (fun v -> eval_velocity v current_time x y) vl)

let rec string_of_velo velocity current_time x y =
  match velocity with
  | Combo (vh :: []) -> string_of_velo vh current_time x y
  | Combo (vh :: vt) ->
      "Combo( ["
      ^ string_of_velo vh current_time x y
      ^ " ; "
      ^ string_of_velo (Combo vt) current_time x y
      ^ "] )"
  | _ ->
      let dx, dy = eval_velocity velocity current_time x y in
      string_of_float dx ^ " x " ^ string_of_float dy
