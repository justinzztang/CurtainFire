open Util

type number =
  | Infinity (*Float.infinity*)
  | PI (*Float.pi*)
  | E (*Float.e*)
  | TAU
  | Variable of
      string (*Search the env for the key, and return the numerical value*)
  | Int of int
  | Float of float

type state_query =
  | Player_X
  | Player_Y
  | Self_X
  | Self_Y
  | Self_theta
  | Self_speed
  | Self_lifetime
  | Parent_X
  | Parent_Y
  | Parent_theta
  | Parent_speed
  | Parent_lifetime
  | Lookup_bullet_X of string * number_expression
  | Lookup_bullet_Y of string * number_expression
  | Lookup_bullet_theta of string * number_expression
  | Lookup_bullet_speed of string * number_expression
  | Lookup_bullet_lifetime of string * number_expression
  | Lookup_enemy_X of string * number_expression
  | Lookup_enemy_Y of string * number_expression
  | Lookup_enemy_theta of string * number_expression
  | Lookup_enemy_speed of string * number_expression
  | Lookup_enemy_lifetime of string * number_expression
  | Closest_enemy_X
  | Closest_enemy_Y
  | Current_frame
  | Elapsed_frames
  | Active_bullets
  | Active_bullets_tag of string
  | Active_enemies
  | Active_enemies_tag of string
  | Remaining_enemies

and number_expression =
  | Number of number
  | RandInt of number_expression * number_expression
  | RandFloat of number_expression * number_expression
  | Query of state_query
  | Plus of number_expression * number_expression
  | Minus of number_expression * number_expression
  | Times of number_expression * number_expression
  | Div of number_expression * number_expression
  | Mod of number_expression * number_expression
  | Pow of number_expression * number_expression
  | Log of number_expression * number_expression
  | Max of number_expression * number_expression
  | Min of number_expression * number_expression
  | Abs of number_expression
  | Sign of number_expression
  | Sin of number_expression
  | Cos of number_expression
  | Tan of number_expression
  | Asin of number_expression
  | Acos of number_expression
  | Atan of number_expression
  | Atan2 of number_expression * number_expression
  | Ceil of number_expression
  | Floor of number_expression
  | Sqrt of number_expression
  | Dist of
      (number_expression * number_expression)
      * (number_expression * number_expression)

type number_function = Function of string list * number_expression

type boolean_expression =
  | Instant
    (*should not evaluate to anything, only put this in single actions n stuff*)
  | DoOnce of bool ref
    (*should not evaluate to anything, only put this in single actions n stuff*)
  | True
  | False
  | Before of number_expression
  | After of number_expression
  | Elapsed of
      (*weird, but theoretically no harm with just making a private variable*)
      number_expression
      * int option ref (*ends after x frames from start*)
  | Within of
      number_expression
      * number_expression
      * number_expression (*if an object is close to a certain point*)
  | Not of boolean_expression
  | Or of boolean_expression * boolean_expression
  | And of boolean_expression * boolean_expression
  | Xor of boolean_expression * boolean_expression
  | LT of number_expression * number_expression
  | GT of number_expression * number_expression
  | EQ of number_expression * number_expression

type action =
  | Die
  | Sleep
  (*tag, coords, velocity, visible, opacity.*)
  | Set_all of
      string
      * number_expression
      * number_expression
      * number_expression
      * number_expression
      * boolean_expression
      * number_expression
  | Set_tag of string
  | Set_X of number_expression
  | Set_Y of number_expression
  | Set_XY of number_expression * number_expression
  | Set_angle of number_expression
  | Set_speed of number_expression
  | Set_tangible of boolean_expression
  | Set_opacity of number_expression
  | Set_velocity of number_expression * number_expression
  | Define_variable of string * number_expression
  | Update_variable of string * number_expression

type behavior =
  | Nothing (*doesnt need env*)
  | Single of action * boolean_expression * float ref segmented_list
    (*needs a reference to parent env*)
  | If_then_else of
      boolean_expression
      * behavior
      * behavior
      * bool option ref
      * float ref segmented_list
  | Sequence of behavior array * int ref * float ref segmented_list
  | For of
      behavior * string * number_expression * int ref * float ref segmented_list
  | While of behavior * boolean_expression * float ref segmented_list

type bullet_type =
  | Circle
  | Arrow
  | Knife
  | Laser of number_expression (*length of laser*)
  | Trail of number_expression * number_expression (*repetitions and interval*)

type bullet_color =
  | White
  | Red
  | Orange
  | Yellow
  | Green
  | Cyan
  | Blue
  | Purple

type bullet_property =
  | Tag
  | X
  | Y
  | Speed
  | Theta
  | TTL
  | Tangible
  | Opacity

type mod_list = {
  mod_tag : string option; (*constantly gets overwritten, shows newest tag*)
  mod_number_property : (bullet_property * number_expression) list;
      (*applies all functions "left to right"*)
  mod_tangible : boolean_expression option; (*also overwrites stuff*)
}

type bullet_pattern =
  | Definition of string * number_expression * float ref segmented_list
  | Update of string * number_expression * float ref segmented_list
  | Nothing of boolean_expression
  (*make a bullet*)
  | Bullet of
      string
      * number_expression
      * number_expression
      * number_expression
      * number_expression
      * number_expression
      * bullet_color
      * number_expression
      * bullet_type
      * behavior
      * number_expression
      * boolean_expression
      * boolean_expression
      * float ref segmented_list
  | Conditional of
      bullet_pattern * boolean_expression * float ref segmented_list
  | If_then_else of
      boolean_expression
      * bullet_pattern
      * bullet_pattern
      * bool option ref
      * float ref segmented_list
  | Timed of
      number_expression
      * number_expression
      * bullet_pattern
      * float ref segmented_list
  | Modify_tag of bullet_pattern * string * float ref segmented_list
  | Modify_active of
      bullet_pattern * boolean_expression * float ref segmented_list
  | Modify of bullet_pattern * mod_list * float ref segmented_list
  | Iterate of
      string
      * number_expression
      * bullet_pattern
      * (bullet_pattern * float ref segmented_list) list option ref
      * float ref segmented_list
  | Combo of bullet_pattern list * float ref segmented_list
  | Sequence of bullet_pattern array * int ref * float ref segmented_list
  | For of
      bullet_pattern
      * string
      * number_expression
      * int ref
      * float ref segmented_list
  | While of bullet_pattern * boolean_expression * float ref segmented_list
  | DEBUG
