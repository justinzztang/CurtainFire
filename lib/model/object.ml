type properties = {
  mutable tag : string;
  mutable x : float;
  mutable y : float;
  mutable speed : float;
  mutable theta : float;
  spawn_x : float;
  spawn_y : float;
  spawn_speed : float;
  spawn_theta : float;
  mutable spawn_frame : int;
  mutable ttl : int;
  mutable tangible : bool;
  mutable opacity : float;
  parent_properties : properties option;
}
