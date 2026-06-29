(** mainly position properties every object wants to store*)
type properties = {
  mutable tag : string;
  (* we need subpixel precision to make things actually move right*)
  mutable x : float;
  mutable y : float;
  mutable speed : float;
  mutable theta : float;
  spawn_x : float;
  spawn_y : float;
  spawn_speed : float;
  spawn_theta : float;
  mutable spawn_frame : int;
  mutable ttl : int; (* time-to-live; how long its active *)
  mutable tangible : bool;
  mutable opacity : float;
  parent_properties : properties option;
}
