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

let keyword_to_token k =
  match k with
  | "nothing" -> Some NOTHING
  | "until" -> Some UNTIL
  | "define" -> Some DEFINE
  | "update" -> Some UPDATE
  | "conditional" -> Some CONDITIONAL
  | "of" -> Some OF
  | "if" -> Some IF
  | "then" -> Some THEN
  | "else" -> Some ELSE
  | "timed" -> Some TIMED
  | "modify_tag" -> Some MODIFY_TAG
  | "modify" -> Some MODIFY
  | "func" -> Some FUNC
  | "modify_active" -> Some MODIFY_ACTIVE
  | "iterate" -> Some ITERATE
  | "upto" -> Some UPTO
  | "combine" -> Some COMBINE
  | "sequence" -> Some SEQUENCE
  | "loop" -> Some LOOP
  | "for" -> Some FOR
  | "while" -> Some WHILE
  | "angled" -> Some ANGLED
  | "pulse" -> Some PULSE
  | "arc" -> Some ARC
  | "aimed" -> Some AIMED
  | "spin" -> Some SPIN
  | "parametric" -> Some PARAMETRIC
  | "bullet" -> Some BULLET
  | "white" -> Some WHITE
  | "red" -> Some RED
  | "orange" -> Some ORANGE
  | "yellow" -> Some YELLOW
  | "green" -> Some GREEN
  | "cyan" -> Some CYAN
  | "blue" -> Some BLUE
  | "purple" -> Some PURPLE
  | "circle" -> Some CIRCLE_BULLET
  | "arrow" -> Some ARROW_BULLET
  | "knife" -> Some KNIFE_BULLET
  | "laser" -> Some LASER_BULLET
  | "trail" -> Some TRAIL_BULLET
  | "sleep" -> Some SLEEP
  | "die" -> Some DIE
  | "set_all" -> Some SET_ALL
  | "set_x" -> Some SET_X
  | "set_y" -> Some SET_Y
  | "set_xy" -> Some SET_XY
  | "set_angle" -> Some SET_ANGLE
  | "set_speed" -> Some SET_SPEED
  | "set_tangible" -> Some SET_TANGIBLE
  | "set_opacity" -> Some SET_OPACITY
  | "set_velocity" -> Some SET_VELOCITY
  | "stop" -> Some STOP
  | "custom" -> Some CUSTOM
  | "custom_rect" -> Some CUSTOM_RECT
  | "forwards" -> Some FORWARDS
  | "point" -> Some POINT
  | "orbit" -> Some ORBIT
  | "gravitate" -> Some GRAVITATE
  | "drift" -> Some DRIFT
  | "steer" -> Some STEER
  | "angled_rel" -> Some ANGLED_REL
  | "turn" -> Some TURN
  | "xor" -> Some XOR
  | "or" -> Some OR
  | "and" -> Some AND
  | "instant" -> Some INSTANT
  | "once" -> Some ONCE
  | "true" -> Some TRUE
  | "false" -> Some FALSE
  | "before" -> Some BEFORE
  | "after" -> Some AFTER
  | "elapsed" -> Some ELAPSED
  | "within" -> Some WITHIN
  | "not" -> Some NOT
  | "pi" -> Some PI
  | "inf" -> Some INF
  | "e" -> Some E
  | "tau" -> Some TAU
  | "log" -> Some LOG
  | "ln" -> Some LN
  | "sin" -> Some SIN
  | "cos" -> Some COS
  | "tan" -> Some TAN
  | "asin" -> Some ASIN
  | "acos" -> Some ACOS
  | "atan" -> Some ATAN
  | "ceil" -> Some CEIL
  | "floor" -> Some FLOOR
  | "sqrt" -> Some SQRT
  | "abs" -> Some ABS
  | "sign" -> Some SIGN
  | "min" -> Some MIN
  | "max" -> Some MAX
  | "randfloat" -> Some RANDFLOAT
  | "randint" -> Some RANDINT
  | "atan2" -> Some ATAN2
  | "dist" -> Some DIST
  | "mod" -> Some MOD
  | _ -> None

type tokenizer = {
  src : string;
  mutable index : int;
}

let peek t n =
  if t.index + n >= String.length t.src then '\x00' else t.src.[t.index + n]

let rec peek_n t n acc =
  if n > 0 then peek_n t (n - 1) (String.make 1 (peek t (n - 1)) ^ acc) else acc

let consume t =
  let c = peek t 0 in
  t.index <- t.index + 1;
  c

let is_digit c = c >= '0' && c <= '9'
let is_alphabet c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_word_char c = is_alphabet c || is_digit c || c = '_'
let is_whitespace c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

let read_number t =
  let start = t.index in
  while (not (t.index >= String.length t.src)) && is_digit (peek t 0) do
    ignore (consume t)
  done;
  if peek t 0 = '.' then ignore (consume t);
  while (not (t.index >= String.length t.src)) && is_digit (peek t 0) do
    ignore (consume t)
  done;
  let endi = t.index in
  NUMBER (float_of_string (String.sub t.src start (endi - start)))

let read_string t =
  ignore (consume t);
  (*skip past quote *)
  let rec reader (acc : string) =
    if t.index >= String.length t.src then failwith "unterminated string"
    else
      let c = consume t in
      match c with
      | '"' -> acc (*end of string*)
      | '\\' ->
          let e = consume t in
          (*escape chars*)
          let newacc =
            acc
            ^ String.make 1
                (match e with
                | 'n' -> '\n'
                | 't' -> '\t'
                | '\\' -> '\\'
                | '"' -> '"'
                | x -> x)
          in
          reader newacc
      | x -> reader (acc ^ String.make 1 x)
  in
  reader ""

let read_keyword t =
  let start = t.index in
  while (not (t.index >= String.length t.src)) && is_word_char (peek t 0) do
    ignore (consume t)
  done;
  let endi = t.index in
  String.sub t.src start (endi - start)

let rec next_token t =
  while is_whitespace (peek t 0) do
    ignore (consume t)
  done;
  if peek_n t 2 "" = "//" then (
    while peek t 0 <> '\n' && peek t 0 <> '\r' do
      ignore (consume t)
    done;
    next_token t)
  else if peek_n t 2 "" = "==" then (
    ignore (consume t);
    ignore (consume t);
    EQEQ)
  else if
    (* == before = *)
    t.index >= String.length t.src
  then EOF
  else
    let next_char = peek t 0 in
    match next_char with
    | ';' ->
        ignore (consume t);
        SEMICOLON
    | '=' ->
        ignore (consume t);
        EQ
    | '(' ->
        ignore (consume t);
        LPAREN
    | ')' ->
        ignore (consume t);
        RPAREN
    | '[' ->
        ignore (consume t);
        LBRACKET
    | ']' ->
        ignore (consume t);
        RBRACKET
    | ',' ->
        ignore (consume t);
        COMMA
    | '<' ->
        ignore (consume t);
        LT
    | '>' ->
        ignore (consume t);
        GT
    | '^' ->
        ignore (consume t);
        CARAT
    | '+' ->
        ignore (consume t);
        PLUS
    | '-' ->
        ignore (consume t);
        MINUS
    | '*' ->
        ignore (consume t);
        TIMES
    | '/' ->
        ignore (consume t);
        DIV
    | '"' -> STRING (read_string t)
    | '$' ->
        ignore (consume t);
        VARNAME (read_keyword t)
    | '@' ->
        ignore (consume t);
        VARNAME ("@" ^ read_keyword t)
    | '%' ->
        ignore (consume t);
        QUERY (read_keyword t)
    | next_char when is_digit next_char -> read_number t
    | next_char when is_alphabet next_char -> (
        let tk = String.lowercase_ascii (read_keyword t) in
        match keyword_to_token tk with
        | None -> failwith ("empty option while tokenizing: " ^ tk)
        | Some k -> k)
    | next_char -> failwith "didnt find a token for this character"

let tokenize s =
  let tt = { src = s; index = 0 } in
  let rec gothrough acc =
    match next_token tt with
    | EOF -> List.rev (EOF :: acc)
    | x -> gothrough (x :: acc)
  in
  gothrough []

let string_of_token t =
  match t with
  | NOTHING -> "NOTHING"
  | UNTIL -> "UNTIL"
  | SEMICOLON -> "SEMICOLON"
  | DEFINE -> "DEFINE"
  | EQ -> "EQ"
  | UPDATE -> "UPDATE"
  | CONDITIONAL -> "CONDITIONAL"
  | OF -> "OF"
  | IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | TIMED -> "TIMED"
  | MODIFY_TAG -> "MODIFY_TAG"
  | MODIFY -> "MODIFY"
  | FUNC -> "FUNC"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | MODIFY_ACTIVE -> "MODIFY_ACTIVE"
  | ITERATE -> "ITERATE"
  | UPTO -> "UPTO"
  | COMBINE -> "COMBINE"
  | LBRACKET -> "LBRACKET"
  | COMMA -> "COMMA"
  | RBRACKET -> "RBRACKET"
  | SEQUENCE -> "SEQUENCE"
  | FOR -> "FOR"
  | WHILE -> "WHILE"
  | LOOP -> "LOOP"
  | ANGLED -> "ANGLED"
  | PULSE -> "PULSE"
  | ARC -> "ARC"
  | AIMED -> "AIMED"
  | SPIN -> "SPIN"
  | PARAMETRIC -> "PARAMETRIC"
  | BULLET -> "BULLET"
  | WHITE -> "WHITE"
  | RED -> "RED"
  | ORANGE -> "ORANGE"
  | YELLOW -> "YELLOW"
  | GREEN -> "GREEN"
  | CYAN -> "CYAN"
  | BLUE -> "BLUE"
  | PURPLE -> "PURPLE"
  | CIRCLE_BULLET -> "CIRCLE_BULLET"
  | ARROW_BULLET -> "ARROW_BULLET"
  | KNIFE_BULLET -> "KNIFE_BULLET"
  | LASER_BULLET -> "LASER_BULLET"
  | TRAIL_BULLET -> "TRAIL_BULLET"
  | SLEEP -> "SLEEP"
  | DIE -> "DIE"
  | SET_ALL -> "SET_ALL"
  | SET_X -> "SET_X"
  | SET_Y -> "SET_Y"
  | SET_XY -> "SET_XY"
  | SET_ANGLE -> "SET_ANGLE"
  | SET_SPEED -> "SET_SPEED"
  | SET_TANGIBLE -> "SET_TANGIBLE"
  | SET_OPACITY -> "SET_OPACITY"
  | SET_VELOCITY -> "SET_VELOCITY"
  | STOP -> "STOP"
  | CUSTOM -> "CUSTOM"
  | CUSTOM_RECT -> "CUSTOM_RECT"
  | FORWARDS -> "FORWARDS"
  | POINT -> "POINT"
  | ORBIT -> "ORBIT"
  | GRAVITATE -> "GRAVITATE"
  | DRIFT -> "DRIFT"
  | STEER -> "STEER"
  | ANGLED_REL -> "ANGLED_REL"
  | TURN -> "TURN"
  | XOR -> "XOR"
  | OR -> "OR"
  | AND -> "AND"
  | INSTANT -> "INSTANT"
  | ONCE -> "ONCE"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | BEFORE -> "BEFORE"
  | AFTER -> "AFTER"
  | ELAPSED -> "ELAPSED"
  | WITHIN -> "WITHIN"
  | NOT -> "NOT"
  | LT -> "LT"
  | EQEQ -> "EQEQ"
  | GT -> "GT"
  | CARAT -> "CARAT"
  | INF -> "INF"
  | PI -> "PI"
  | E -> "E"
  | TAU -> "TAU"
  | LOG -> "LOG"
  | LN -> "LN"
  | SIN -> "SIN"
  | COS -> "COS"
  | TAN -> "TAN"
  | ASIN -> "ASIN"
  | ACOS -> "ACOS"
  | ATAN -> "ATAN"
  | CEIL -> "CEIL"
  | FLOOR -> "FLOOR"
  | SQRT -> "SQRT"
  | ABS -> "ABS"
  | SIGN -> "SIGN"
  | MIN -> "MIN"
  | MAX -> "MAX"
  | RANDFLOAT -> "RANDFLOAT"
  | RANDINT -> "RANDINT"
  | ATAN2 -> "ATAN2"
  | DIST -> "DIST"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  | DIV -> "DIV"
  | MOD -> "MOD"
  | NUMBER f -> Printf.sprintf "NUMBER(%g)" f
  | DOLLAR -> "DOLLAR"
  | STRING s -> Printf.sprintf "STRING(%s)" s
  | VARNAME s -> Printf.sprintf "VARNAME(%s)" s
  | QUERY s -> Printf.sprintf "QUERY(%s)" s
  | EOF -> "EOF"
