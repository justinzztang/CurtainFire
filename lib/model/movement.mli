(** Abstraction Function: A value [v : velocity] represents a movement pattern
    via velocity that determines how an object (e.g., an enemy) moves over time.

    - [Custom] represents velocity with angle and speed over time determined by
      custom functions for more particular movement

    - [CustomDXDY] represents custom motion with respect to functions
      determining change in x and y position over time

    - [Linear] represents straight-line motion with constant direction and
      speed, as determined by external parameters (e.g., angle and speed stored
      in the object using this path).

    - [Point] calculates an angle to travel in given a point in space that the
      object is approaching together with a speed function

    - [Sequence] allows multiple velocity types back-to-back, each with a time
      limit

    - [Orbit] moves a certain velocity away from an orbig point at a given
      angular velocity around the orbit point

    - [Combo] adds up all velocity functions stored in it and moves according to
      the resultant velocity

    Representation Invariant:
    - No additional invariants; all values of type [path] are valid. *)
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

(** [eval_velocity v t x y] returns the [(angle, speed)] pair that the velocity
    [v] produces at time [t] for an object currently at position [(x, y)]. The
    angle is in radians measured in screen coordinates (positive y down), and
    the speed is in pixels per frame. *)
val eval_velocity : velocity -> float -> float -> float -> float * float

(** [string_of_velo v t x y] returns a human-readable string describing the
    angle and speed produced by [v] at time [t] from position [(x, y)]. Useful
    for debugging movement patterns. *)
val string_of_velo : velocity -> float -> float -> float -> string
