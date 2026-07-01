# Spawn Patterns

### Nothing
`Nothing Until <condition>;`: Does exactly what you think it does. Lasts until the condition is met.

### Bullet

`Bullet <color> <bullet_type> <speed> <radius> <ttl> Until <condition>;`

Or
`Bullet <color> <bullet_type> <speed> <angle> <radius> <ttl> Until <condition>;`

Or
`Bullet <color> <bullet_type> <speed> <angle> <radius> <ttl> <behavior> Until <condition>;`

Or
`Bullet <x> <y> <color> <bullet_type> <speed> <angle> <radius> <ttl> Until <condition>;`

Or
`Bullet <x> <y> <color> <bullet_type> <speed> <angle> <radius> <ttl> <behavior> Until <condition>;`

Or
`Bullet <tag> <x> <y> <color> <bullet_type> <speed> <angle> <radius> <ttl> <tangible> <opacity> <behavior> Until <condition>;`

Spawns a bullet with the given properties if the end condition is false. Here, `tangible` refers to if collision checking is enabled.

The bullet type is either Circle (circular graphic), Arrow (directional graphic), Laser <length>, or Trail <repetitions> <time_interval> (remembers the last few positions with `time_interval` frames between each of them).

### Define & Update
`Define <id> = <value>;`: Defines a variable with the given id and numerical value. Note that variable ids begin with '$' like: `$id`. Variables are in scope to every nested pattern.

`Update <id> = <value>;`: Updates a variable with the given id and numerical value. Will cause an error if no variable with the id is found.

### Conditional & If

`Conditional <condition> <pattern>;`: Only evaluates the inner pattern if the boolean condition is true.

`If <condition> then <pattern1> else <pattern2>;`: If the boolean condition is true, step into the first pattern, otherwise step into the second. Note that the condition is evaluated only once, and the first chosen pattern is evaluated until it ends.

### Timed

`Timed <start_time> <end_time> <pattern>;`: Only evaluates the inner pattern if the current time is between the start and end time.

### Modify

`Modify <mod1> <mod2> ... of <pattern>;`: Applies each modification function to bullets spawned by the inner pattern, on top of any previous modifications. Modification functions can be thought of as a function of the bullet's X, Y, speed, angle, TTL, and opacity, with the syntax: `@<property> = <number_expression>`.

Examples: `@X = @X + 32` (bullets are spawned 32 pixels to the right), `@A = @X + @Y` (bullets have their angles set to X + Y)

The two exceptions are tag and tangible. Tag is set to a string, overwriting previous tag modifications like: `@TAG = "newtag"`, and tangible is set to a boolean expression, overwriting previous tangible modifications like: `@TANGIBLE = False OR True`.

### Iterate

`Iterate <id> upto <n> <pattern>;`: Creates `n` copies of the inner pattern, passing down the current index to the pattern each time.

Example: `Iterate $i upto 10 Bullet Blue Circle $i 10 60 Until Once;;` creates 10 bullets with speeds 0, 1, 2, ... 

### Combine & Sequence

`Combine[ <pattern1> <pattern2> <pattern3> ... ];`: Evaluates each pattern before the next frame, from left to right. 

`Sequence[ <pattern1> <pattern2> <pattern3> ... ];`: Evaluates the patterns in sequence, moving to the next pattern once the current pattern is finished.

### For & While

`For <id> upto <n> <pattern>;`: For loop, passes the value of id to the inner pattern each step of evaluation. Note: the value of id updates globally each step.

`While <condition> <pattern>;`: While loop, evaluates the inner pattern only if the condition is true.

### Angled & Aimed

`Angled <angle> <pattern>;`: Angles the inner pattern, as if the provided angle is the new zero for the inner pattern.

`Aimed <pattern>;`: Angles the inner pattern directly towards the player position.

### Pulse

`Pulse <interval> <pattern>;`: Sets a delay between each evaluation of the inner pattern. 

### Arc & Spin

`Arc <n> <start_angle> <end_angle> <distance> <pattern>;` Creates `n` evenly spaced inner patterns `distance` away from the spawnpoint, from the start angle to the end angle.

`Spin (<pivot_x>, <pivot_y>) <angular_speed> <pattern>;` Rotates the inner pattern around the pivot point at the given angular speed.

# Behaviors

Every instruction is followed by `Until <condition>;`, which tells it when to end.

### If, Sequence, For, & While

Same syntax and behavior as their pattern equivalents, just replace patterns with behaviors.

### Die & Sleep

`Die` Despawns the object.

`Sleep` Updates nothing.

### Set

`Set_all <tag> <x> <y> <speed> <angle> <tangible> <opacity>` Updates every property of the object. Tangible refers to if collision checking is on.

Other "Set" instructions are: `Set_X, Set_Y, Set_XY, Set_angle, Set_speed, Set_tangible, Set_opacity, Set_velocity <speed> <angle>`.

### Define & Update

`Define <id> = <value>;`

`Update <id> = <value>;`

Same as in patterns. Variables are in scope to every nested behavior.

### Stop

`Stop` fully stops movement, setting the speed to 0.

### Custom & Custom_rect

`Custom <speed> <angle>` sets the object's speed and angle properties to a given numerical expression.

`Custom <dx> <dy>` sets the object's horizontal and vertical speeds.

### Forwards

`Forwards <speed>` sets the object's speed, leaving the angle unchanged.

### Point, Orbit, & Gravitate

`Point (<x>, <y>) <speed>` moves the object towards the given point at the given speed.

`Orbit (<pivot_x>, <pivot_y>) <radial_speed> <angular_speed>` moves the object around the pivot point at the given angular speed, and away from the pivot point at the given radial speed.

`Gravitate (<x>, <y>) <force>` applies acceleration to the object that pulls it towards the point faster as it gets closer. Not physically accurate.

### Drift

`Drift <friction>` gradually slows down the object based on the given friction.

### Steer & Turn

`Steer <speed> <angular_speed>` moves the object forward at the given speed, and turns the angle at the given angular speed

`Turn <angle>` turns the angle by the given amount.


### Angled & Angled_rel

`Angled <speed> <angle>` moves the object at the given angle, at the given speed.

`Angled_rel <speed> <angle>` moves the object at the given angle relative to its initial angle, at the given speed.

# Boolean Expressions

### Instant & Once

`Instant` is intended to be a condition that instantly evaluates some instruction, then moves to the next instruction without waiting another frame.

`Once` is intended to be a condition that instantly evaluates some instruction, then stops evaluation before the next instruction is executed.

Neither should be treated as a genuine boolean value.

### Before & After

`Before <time>` evaluates true if the current time is before the given time.

`After <time>` evaluates true if the current time is after the given time.

### Elapsed

`Elapsed <time>` evaluates true if the given amount of time has passed since first checking this condition.

### Within

`Within (<x>,<y>) <r>` evaluates true if the object is within a certain radius of a point.

# State Queries

Special variables whose names begin with `%` are used to query information about the game state. 

### Lookups

Since game objects are stored in arrays that are accessed through using their tags as keys, queries such as `%LOOKUP_BULLET_X` have the syntax `<query> <tag> <index>` to tell the engine to look up the property of the object with that given tag at the given index of the engine's list of objects.

For example: If we used Python dicts of arrays to store bullets, `%LOOKUP_BULLET_X "tag" 0` will get the properties of enemy_bullet_map["tag"][0].

# Full Grammar

Anything that didn't have its own description is probably a common, already documented function.

The full EBNF grammar for the language is below:

```
<spawn_pattern> ::= "Nothing" "Until" <boolean_expr> ";"
				| <bullet_spawn> "Until" <boolean_expr> ";"
                | "Define" <variable_id> "=" <number_expr> ";"
                | "Update" <variable_id> "=" <number_expr> ";"
                | "Conditional" <boolean_expr> <spawn_pattern> ";"
				| "If" <boolean_expr> "then" <spawn_pattern> "else" <spawn_pattern> ";" 
                | "Timed" <number_expr> <number_expr> <spawn_pattern> ";"
                | "Modify" (<modification>)+ "of" <spawn_pattern> ";"
                | "Iterate" <variable_id> "upto" <number_expr> <spawn_pattern> ";"
                | "Combine" "[" <spawn_pattern> ( <spawn_pattern>)* "]" ";"  
                | "Sequence" "[" <spawn_pattern> ( <spawn_pattern>)* "]" ";"  
				| "For" <variable_id> "upto" <number_expr> <spawn_pattern> ";"
				| "While" <boolean_expr> <spawn_pattern> ";"
                | "Angled" <number_expr> <spawn_pattern> ";"
                | "Pulse" <number_expr> <spawn_pattern> ";"
                | "Arc" <number_expr> <number_expr> <number_expr> <number_expr> <spawn_pattern> ";"
                | "Aimed" <spawn_pattern> ";"
                | "Spin" "(" <number_expr> "," <number_expr> ")" <number_expr> <spawn_pattern> ";"
				
<bullet_spawn> ::= "Bullet" <color> <bullet_type> <number_expr> <number_expr> <number_expr>
				| "Bullet" <color> <bullet_type> <number_expr> <number_expr> <number_expr> <number_expr>
                | "Bullet" <color> <bullet_type> <number_expr> <number_expr> <number_expr> <number_expr> <behavior>
                | "Bullet" <number_expr> <number_expr> <color> <bullet_type> <number_expr> <number_expr> <number_expr> <number_expr> 
                | "Bullet" <number_expr> <number_expr> <color> <bullet_type> <number_expr> <number_expr> <number_expr> <number_expr>  <behavior>
                | "Bullet" <string> <number_expr> <number_expr> <color> <bullet_type> <number_expr> <number_expr> <number_expr> <number_expr> <boolean_expr> <number_expr> <behavior>

<modification> ::= (<bullet_query> "=" <number_expr> | "@TAG" "=" <string> | "@TANGIBLE" "=" <boolean_expr>)

<color> ::= "Red" | "Orange" | "Yellow" | "Green" | "Cyan" | "Blue" | "Purple"

<bullet_type> ::= "Circle" | "Arrow" | "Knife" | "Laser" <number_expr> | "Trail" <number_expr> <number_expr>

<behavior> ::= <single_item> ";" 
				| "If" <boolean_expr> "then" <behavior> "else" <behavior> ";" 
                | "Sequence" "[" <behavior> (<behavior>)* "]" ";" 
                | "For" <variable_id> "upto" <number_expr> <behavior> ";"
				| "While" <boolean_expr> <behavior> ";"
<single_item> ::= <instruction> | <movement>
<instruction> ::= "Die"
				| "Sleep" "Until" <boolean_expr>
				| "Set_all" <string> <number_expr> <number_expr> <number_expr> <number_expr> <boolean_expr> <number_expr> "Until" <boolean_expr>
                | "Set_X" <number_expr> "Until" <boolean_expr>
				| "Set_Y" <number_expr> "Until" <boolean_expr>
				| "Set_XY" <number_expr> <number_expr> "Until" <boolean_expr>
				| "Set_angle" <number_expr> "Until" <boolean_expr>
				| "Set_speed" <number_expr> "Until" <boolean_expr>
				| "Set_tangible" <boolean_expr> "Until" <boolean_expr>
				| "Set_opacity" <number_expr> "Until" <boolean_expr>
				| "Set_velocity" <number_expr> <number_expr> "Until" <boolean_expr>
                | "Define" <variable_id> "=" <number_expr> 
                | "Update" <variable_id> "=" <number_expr> 
<movement> ::= "Stop" "Until" <boolean_expr>
				| "Custom" <number_expr> <number_expr> "Until" <boolean_expr>
                | "Custom_rect" <number_expr> <number_expr> "Until" <boolean_expr>
                | "Forwards" <number_expr> "Until" <boolean_expr>
                | "Point" "(" <number_expr> "," <number_expr> ")" <number_expr> "Until" <boolean_expr>
				| "Orbit" "(" <number_expr> "," <number_expr> ")" <number_expr> <number_expr> "Until" <boolean_expr>
				| "Gravitate" "(" <number_expr> "," <number_expr> ")" <number_expr> "Until" <boolean_expr>
				| "Drift" <number_expr> "Until" <boolean_expr>
				| "Steer" <number_expr> <number_expr> "Until" <boolean_expr>
				| "Angled" <number_expr> <number_expr> "Until" <boolean_expr> 
				| "Angled_rel" <number_expr> <number_expr> "Until" <boolean_expr>
				| "Turn" <number_expr> "Until" <boolean_expr>


<boolean_expr> ::= <bool_xor_term> ("XOR" <bool_xor_term>)*
<bool_xor_term> ::= <boolean_term> ("OR" <boolean_term>)*
<boolean_term> ::= <boolean_factor> ("AND" <boolean_factor>)*
<boolean_factor> ::= "Instant" | "Once" | "True" | "False" 
				| "Before" <number_expr> 
                | "After" <number_expr> 
                | "Elapsed" <number_expr> 
                | "Within" "(" <number_expr> "," <number_expr> ")" <number_expr> 
                | "NOT" <boolean_factor> 
                | <number_expr> "<" <number_expr> 
                | <number_expr> "==" <number_expr> 
                | <number_expr> ">" <number_expr> 
                | "(" <boolean_expr> ")"

<number_expr> ::= <term> (<addop> <term>)*
<term> ::= <factor> (<mulop> <factor>)*
<factor> ::= <addop>? <power>
<power> ::= <primary> ("^" <power>)?
<primary> ::= "INF" | "PI" | "E" | "TAU" 
				| <num_literal> | <variable_id> | <state_query> | <bullet_query>
				| "(" <number_expr> ")" 
                | <unary_func> "(" <number_expr> ")" 
                | <binary_func> "(" <number_expr> "," <number_expr> ")" 
				| <primary_extras>
<unary_func> ::= "log" | "ln" | "sin" | "cos" | "tan" | "asin" | "acos" | "atan" 
				| "ceil" | "floor" | "sqrt" | "abs" | "sign"
<binary_func> ::= "log" | "min" | "max" | "randFloat" | "randInt" | "atan2"
<primary_extras> ::= "dist" "(" <number_expr> "," <number_expr> ")" "(" <number_expr> "," <number_expr> ")"
<addop> ::= "+" | "-"
<mulop> ::= "*" | "/" | "mod" 

<state_query> ::= "%PLAYER_X" | "%PLAYER_Y" 
				| "%SELF_X" | "%SELF_Y" | "%SELF_ANGLE" | "%SELF_SPEED" | "%SELF_LIFETIME"
				| "%PARENT_X" | "%PARENT_Y" | "%PARENT_ANGLE" | "%PARENT_SPEED" | "%PARENT_LIFETIME"
				| "%CLOSEST_ENEMY_X" | "%CLOSEST_ENEMY_Y"
				| "%LOOKUP_BULLET_X" <string> <number_expr> | "%LOOKUP_BULLET_Y" <string> <number_expr> (*objects are stored in (tag, array) maps, latest spawn last, so lookup map[tag][index]*)
				| "%LOOKUP_BULLET_ANGLE" <string> <number_expr> | "%LOOKUP_BULLET_SPEED" <string> <number_expr>
				| "%LOOKUP_BULLET_LIFETIME" <string> <number_expr>  
				| "%LOOKUP_ENEMY_X" <string> <number_expr> | "%LOOKUP_ENEMY_Y" <string> <number_expr>
				| "%LOOKUP_ENEMY_ANGLE" <string> <number_expr> | "%LOOKUP_ENEMY_SPEED" <string> <number_expr>
				| "%LOOKUP_ENEMY_LIFETIME" <string> <number_expr> 
				| "%CURRENT_FRAME" | "%ELAPSED_FRAMES" | "%ALIVE_ENEMIES_COUNT" | "%ALIVE_BULLETS_COUNT"
				| "%ALIVE_BULLETS_COUNT_TAG" <string> | "%ALIVE_ENEMIES_COUNT_TAG" <string> | "%REMAINING_ENEMIES_COUNT"

<bullet_query> ::= "@X" | "@Y" | "@S" | "@A" | "@TTL" | "@O" (*using this outside of modify will cause an error*)

<variable_id> ::= "$" ([a-z] | [A-Z] | [0-9] | "_")+
```
