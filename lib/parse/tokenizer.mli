type token =
  | NOTHING
  | UNTIL
  | SEMICOLON
  | DEFINE
  | EQ
  | UPDATE
  | CONDITIONAL
  | OF
  | IF
  | THEN
  | ELSE
  | TIMED
  | MODIFY_TAG
  | MODIFY
  | FUNC
  | LPAREN
  | RPAREN
  | MODIFY_ACTIVE
  | ITERATE
  | UPTO
  | COMBINE
  | LBRACKET
  | COMMA
  | RBRACKET
  | SEQUENCE
  | FOR
  | WHILE
  | LOOP
  | ANGLED
  | PULSE
  | ARC
  | AIMED
  | SPIN
  | PARAMETRIC
  | BULLET
  | WHITE
  | RED
  | ORANGE
  | YELLOW
  | GREEN
  | CYAN
  | BLUE
  | PURPLE
  | CIRCLE_BULLET
  | ARROW_BULLET
  | KNIFE_BULLET
  | LASER_BULLET
  | TRAIL_BULLET
  | SLEEP
  | DIE
  | SET_ALL
  | SET_X
  | SET_Y
  | SET_XY
  | SET_ANGLE
  | SET_SPEED
  | SET_TANGIBLE
  | SET_OPACITY
  | SET_VELOCITY
  | STOP
  | CUSTOM
  | CUSTOM_RECT
  | FORWARDS
  | POINT
  | ORBIT
  | GRAVITATE
  | DRIFT
  | STEER
  | ANGLED_REL
  | TURN
  | XOR
  | OR
  | AND
  | INSTANT
  | ONCE
  | TRUE
  | FALSE
  | BEFORE
  | AFTER
  | ELAPSED
  | WITHIN
  | NOT
  | LT
  | EQEQ
  | GT
  | CARAT
  | INF
  | PI
  | E
  | TAU
  | LOG
  | LN
  | SIN
  | COS
  | TAN
  | ASIN
  | ACOS
  | ATAN
  | CEIL
  | FLOOR
  | SQRT
  | ABS
  | SIGN
  | MIN
  | MAX
  | RANDFLOAT
  | RANDINT
  | ATAN2
  | DIST
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | NUMBER of float
  | DOLLAR
  | STRING of string
  | VARNAME of string
  | QUERY of string
  | EOF

type tokenizer = {
  src : string;
  mutable index : int;
}

(** Return the next token in a tokenizer*)
val next_token : tokenizer -> token

(** Convert a string into a list of tokens*)
val tokenize : string -> token list

(** Get the name of a token*)
val string_of_token : token -> string
