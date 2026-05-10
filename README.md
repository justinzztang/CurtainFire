# Bullet Hell Engine
This project is an OCaml backend that implements both a game engine and several helper modules that allow for the efficient creation of shoot-em-up (also known as bullet hell) levels. A demo level can be found at bin/level/demo_stage.ml

# Custom Levels
Just like demo_stage.ml, it is possible to create custom levels using the provided modules and helper functions. Then, in main.ml, import the level's module (the demo level's module is called "DS") and call init_level in the same manner as DS (we didn't have time to make this nice :p ). 

*Audio is hardcoded into main.ml right now

# Contributors
Daniel Lin 
Joshua Rafaeil 
Gokulanath Mahesh Kumar 
Justin Tang 
Will Siwinski 