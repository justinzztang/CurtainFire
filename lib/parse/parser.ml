open Tokenizer
open Ast
open Util

type token_stream = token list ref

let peek_token ts =
  match !ts with
  | [ EOF ] -> EOF
  | x :: xs -> x
  | _ ->
      failwith
        "something went wrong while peeking tokens, there should always be EOF \
         leftover"

let consume_token ts =
  match !ts with
  | [ EOF ] -> EOF
  | x :: xs ->
      ts := xs;
      x
  | _ ->
      failwith
        "something went wrong while consuming tokens, there should always be \
         EOF leftover"

let rec pick_query name token_stream parent_env =
  match name with
  | "PLAYER_X" -> Player_X
  | "PLAYER_Y" -> Player_Y
  | "SELF_X" -> Self_X
  | "SELF_Y" -> Self_Y
  | "SELF_ANGLE" -> Self_theta
  | "SELF_SPEED" -> Self_speed
  | "SELF_LIFETIME" -> Self_lifetime
  | "PARENT_X" -> Parent_X
  | "PARENT_Y" -> Parent_Y
  | "PARENT_ANGLE" -> Parent_theta
  | "PARENT_SPEED" -> Parent_speed
  | "PARENT_LIFETIME" -> Parent_lifetime
  | "CLOSEST_ENEMY_X" -> Closest_enemy_X
  | "CLOSEST_ENEMY_Y" -> Closest_enemy_Y
  | "LOOKUP_BULLET_X" -> (
      match consume_token token_stream with
      | STRING s ->
          Lookup_bullet_X (s, parse_number_expression token_stream parent_env)
      | t -> failwith ("expected bullet tag but got: " ^ string_of_token t))
  | "LOOKUP_BULLET_Y" -> (
      match consume_token token_stream with
      | STRING s ->
          Lookup_bullet_Y (s, parse_number_expression token_stream parent_env)
      | t -> failwith ("expected bullet tag but got: " ^ string_of_token t))
  | "LOOKUP_BULLET_ANGLE" -> (
      match consume_token token_stream with
      | STRING s ->
          Lookup_bullet_theta
            (s, parse_number_expression token_stream parent_env)
      | t -> failwith ("expected bullet tag but got: " ^ string_of_token t))
  | "LOOKUP_BULLET_SPEED" -> (
      match consume_token token_stream with
      | STRING s ->
          Lookup_bullet_speed
            (s, parse_number_expression token_stream parent_env)
      | t -> failwith ("expected bullet tag but got: " ^ string_of_token t))
  | "LOOKUP_BULLET_LIFETIME" -> (
      match consume_token token_stream with
      | STRING s ->
          Lookup_bullet_lifetime
            (s, parse_number_expression token_stream parent_env)
      | t -> failwith ("expected bullet tag but got: " ^ string_of_token t))
  | "LOOKUP_ENEMY_X" -> (
      match consume_token token_stream with
      | STRING s ->
          Lookup_enemy_X (s, parse_number_expression token_stream parent_env)
      | t -> failwith ("expected enemy tag but got: " ^ string_of_token t))
  | "LOOKUP_ENEMY_Y" -> (
      match consume_token token_stream with
      | STRING s ->
          Lookup_enemy_Y (s, parse_number_expression token_stream parent_env)
      | t -> failwith ("expected enemy tag but got: " ^ string_of_token t))
  | "LOOKUP_ENEMY_ANGLE" -> (
      match consume_token token_stream with
      | STRING s ->
          Lookup_enemy_theta (s, parse_number_expression token_stream parent_env)
      | t -> failwith ("expected enemy tag but got: " ^ string_of_token t))
  | "LOOKUP_ENEMY_SPEED" -> (
      match consume_token token_stream with
      | STRING s ->
          Lookup_enemy_speed (s, parse_number_expression token_stream parent_env)
      | t -> failwith ("expected enemy tag but got: " ^ string_of_token t))
  | "LOOKUP_ENEMY_LIFETIME" -> (
      match consume_token token_stream with
      | STRING s ->
          Lookup_enemy_lifetime
            (s, parse_number_expression token_stream parent_env)
      | t -> failwith ("expected enemy tag but got: " ^ string_of_token t))
  | "CURRENT_FRAME" -> Current_frame
  | "ELAPSED_FRAMES" -> Elapsed_frames
  | "ALIVE_ENEMIES_COUNT" -> Active_enemies
  | "ALIVE_BULLETS_COUNT" -> Active_bullets
  | "ALIVE_ENEMIES_COUNT_TAG" -> (
      match consume_token token_stream with
      | STRING s -> Active_enemies_tag s
      | t -> failwith ("expected enemy tag but got: " ^ string_of_token t))
  | "ALIVE_BULLETS_COUNT_TAG" -> (
      match consume_token token_stream with
      | STRING s -> Active_bullets_tag s
      | t -> failwith ("expected bullet tag but got: " ^ string_of_token t))
  | "REMAINING_ENEMIES_COUNT" -> Remaining_enemies
  | _ -> failwith ("unrecognized query: " ^ name)

and parse_primary token_stream parent_env =
  match consume_token token_stream with
  | INF -> Number Ast.Infinity
  | PI -> Number Ast.PI
  | E -> Number Ast.E
  | TAU -> Number Ast.TAU
  | NUMBER n -> Number (Float n)
  | VARNAME x -> Number (Variable x)
  | QUERY q ->
      Query (pick_query q token_stream parent_env)
  | LPAREN ->
      let result = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing (num_expr)";
      result
  | LOG ->
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing log";
      let t1 = parse_number_expression token_stream parent_env in
      if peek_token token_stream = COMMA then (
        ignore (consume_token token_stream);
        let t2 = parse_number_expression token_stream parent_env in
        if consume_token token_stream <> RPAREN then
          failwith "expected ) while parsing log";
        Log (t1, t2))
      else (
        if consume_token token_stream <> RPAREN then
          failwith "expected ) while parsing log";
        Log (Number (Float 10.), t1))
  | LN ->
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing ln";
      let t1 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing ln";
      Log (Number E, t1)
  | ABS ->
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing abs";
      let t1 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing abs";
      Abs t1
  | SIGN ->
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing sign";
      let t1 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing sign";
      Sign t1
  | SIN ->
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing sin";
      let t1 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing sin";
      Sin t1
  | COS ->
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing cos";
      let t1 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing cos";
      Cos t1
  | TAN ->
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing tan";
      let t1 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing tan";
      Tan t1
  | ASIN ->
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing asin";
      let t1 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing asin";
      Asin t1
  | ACOS ->
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing acos";
      let t1 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing acos";
      Acos t1
  | ATAN ->
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing atan";
      let t1 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing atan";
      Atan t1
  | CEIL ->
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing ceil";
      let t1 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing ceil";
      Ceil t1
  | FLOOR ->
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing floor";
      let t1 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing floor";
      Floor t1
  | SQRT ->
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing sqrt";
      let t1 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing sqrt";
      Sqrt t1
  | MIN ->
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing min";
      let t1 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> COMMA then
        failwith "expected , while parsing min";
      let t2 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing min";
      Min (t1, t2)
  | MAX ->
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing max";
      let t1 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> COMMA then
        failwith "expected , while parsing max";
      let t2 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing max";
      Max (t1, t2)
  | RANDFLOAT ->
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing randfloat";
      let t1 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> COMMA then
        failwith "expected , while parsing randfloat";
      let t2 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing randfloat";
      RandFloat (t1, t2)
  | RANDINT ->
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing randint";
      let t1 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> COMMA then
        failwith "expected , while parsing randint";
      let t2 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing randint";
      RandInt (t1, t2)
  | ATAN2 ->
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing atan2";
      let t1 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> COMMA then
        failwith "expected , while parsing atan2";
      let t2 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing atan2";
      Atan2 (t1, t2)
  | DIST ->
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing dist";
      let t1 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> COMMA then
        failwith "expected , while parsing dist";
      let t2 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing dist";
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing dist";
      let t3 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> COMMA then
        failwith "expected , while parsing dist";
      let t4 = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing dist";
      Dist ((t1, t2), (t3, t4))
  | t ->
      failwith
        ("expected some number terminal in parse_primary but got: "
        ^ Tokenizer.string_of_token t)

and parse_power token_stream parent_env =
  let t1 = parse_primary token_stream parent_env in
  if peek_token token_stream = CARAT then (
    ignore (consume_token token_stream);
    Pow (t1, parse_power token_stream parent_env))
  else t1

and parse_factor token_stream parent_env : number_expression =
  let t1 = peek_token token_stream in
  if t1 = PLUS || t1 = MINUS then (
    ignore (consume_token token_stream);
    let t2 = parse_power token_stream parent_env in
    match t1 with
    | PLUS -> t2
    | MINUS -> Times (Number (Int (-1)), t2)
    | _ -> failwith "how did this happen? parse_factor")
  else parse_power token_stream parent_env

and parse_term token_stream parent_env : number_expression =
  let t1 = parse_factor token_stream parent_env in

  let rec keep_parsing acc =
    if
      peek_token token_stream = TIMES
      || peek_token token_stream = DIV
      || peek_token token_stream = MOD
    then
      match consume_token token_stream with
      | TIMES ->
          let t2 = parse_factor token_stream parent_env in
          keep_parsing (Ast.Times (acc, t2))
      | DIV ->
          let t2 = parse_factor token_stream parent_env in
          keep_parsing (Ast.Div (acc, t2))
      | MOD ->
          let t2 = parse_factor token_stream parent_env in
          keep_parsing (Ast.Mod (acc, t2))
      | _ -> failwith "how did this happen? parse_term"
    else acc
  in
  keep_parsing t1

and parse_number_expression token_stream parent_env : number_expression =
  let t1 = parse_term token_stream parent_env in

  let rec keep_parsing acc =
    if peek_token token_stream = PLUS || peek_token token_stream = MINUS then
      match consume_token token_stream with
      | PLUS ->
          let te = parse_term token_stream parent_env in
          keep_parsing (Ast.Plus (acc, te))
      | MINUS ->
          let te = parse_term token_stream parent_env in
          keep_parsing (Ast.Minus (acc, te))
      | _ -> failwith "how did this happen? parse_number_expression"
    else acc
  in
  keep_parsing t1

let rec parse_boolean_factor token_stream parent_env =
  let t1 = peek_token token_stream in
  match t1 with
  | INSTANT ->
      ignore (consume_token token_stream);
      Ast.Instant
  | ONCE ->
      ignore (consume_token token_stream);
      Ast.DoOnce (ref false)
  | TRUE ->
      ignore (consume_token token_stream);
      Ast.True
  | FALSE ->
      ignore (consume_token token_stream);
      Ast.False
  | BEFORE ->
      ignore (consume_token token_stream);
      let t = parse_number_expression token_stream parent_env in
      Before t
  | AFTER ->
      ignore (consume_token token_stream);
      let t = parse_number_expression token_stream parent_env in
      After t
  | ELAPSED ->
      ignore (consume_token token_stream);
      let t = parse_number_expression token_stream parent_env in
      Elapsed (t, ref None)
  | WITHIN ->
      ignore (consume_token token_stream);
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing within";
      let xx = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> COMMA then
        failwith "expected , while parsing within";
      let yy = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing within";
      let rr = parse_number_expression token_stream parent_env in
      Within (xx, yy, rr)
  | NOT ->
      ignore (consume_token token_stream);
      Not (parse_boolean_factor token_stream parent_env)
  | LPAREN ->
      ignore (consume_token token_stream);
      let result = parse_boolean_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing (bool_expr)";
      result
  | _ -> (
      let t1 = parse_number_expression token_stream parent_env in
      match consume_token token_stream with
      | LT ->
          let t2 = parse_number_expression token_stream parent_env in
          Ast.LT (t1, t2)
      | EQEQ ->
          let t2 = parse_number_expression token_stream parent_env in
          Ast.EQ (t1, t2)
      | GT ->
          let t2 = parse_number_expression token_stream parent_env in
          Ast.GT (t1, t2)
      | t ->
          failwith ("expected a binary relation but got: " ^ string_of_token t))

and parse_boolean_term token_stream parent_env =
  let t1 = parse_boolean_factor token_stream parent_env in
  let rec keep_parsing acc =
    if peek_token token_stream = AND then (
      ignore (consume_token token_stream);
      let t2 = parse_boolean_factor token_stream parent_env in
      keep_parsing (Ast.And (acc, t2)))
    else acc
  in
  keep_parsing t1

and parse_boolean_xor_term token_stream parent_env =
  let t1 = parse_boolean_term token_stream parent_env in
  let rec keep_parsing acc =
    if peek_token token_stream = OR then (
      ignore (consume_token token_stream);
      let t2 = parse_boolean_term token_stream parent_env in
      keep_parsing (Ast.Or (acc, t2)))
    else acc
  in
  keep_parsing t1

and parse_boolean_expression token_stream parent_env =
  let t1 = parse_boolean_xor_term token_stream parent_env in
  let rec keep_parsing acc =
    if peek_token token_stream = XOR then (
      ignore (consume_token token_stream);
      let t2 = parse_boolean_xor_term token_stream parent_env in
      keep_parsing (Xor (acc, t2)))
    else acc
  in
  keep_parsing t1

let rec parse_behavior token_stream parent_env : Ast.behavior =
  let nt = consume_token token_stream in
  match nt with
  | IF ->
      let own_env = { values = StringMap.empty; next = Some parent_env } in
      let b0 = parse_boolean_expression token_stream own_env in
      if consume_token token_stream <> THEN then
        failwith "expected then while parsing behavior If";
      let b1 = parse_behavior token_stream own_env in
      if consume_token token_stream <> ELSE then
        failwith "expected else while parsing behavior If";
      let b2 = parse_behavior token_stream own_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing behavior If";
      If_then_else (b0, b1, b2, ref None, own_env)
  | SEQUENCE ->
      if consume_token token_stream <> LBRACKET then
        failwith "expected lbracket while parsing behavior sequence";
      let own_env = { values = StringMap.empty; next = Some parent_env } in
      let first_behavior = parse_behavior token_stream own_env in
      let rec keep_parsing acc =
        if peek_token token_stream = RBRACKET then (
          ignore (consume_token token_stream);
          acc)
        else keep_parsing (parse_behavior token_stream own_env :: acc)
      in

      let sl = Array.of_list (List.rev (keep_parsing [ first_behavior ])) in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected semicolon while parsing behavior sequence";
      Sequence (sl, ref 0, own_env)
  | FOR ->
      let vname =
        match consume_token token_stream with
        | VARNAME x -> x
        | _ -> failwith "expected variable name while parsing behavior for"
      in
      if consume_token token_stream <> UPTO then
        failwith "expected upto while parsing behavior for";
      let own_env =
        {
          values = StringMap.add vname (ref 0.) StringMap.empty;
          next = Some parent_env;
        }
      in
      let count = parse_number_expression token_stream own_env in
      let p = parse_behavior token_stream own_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected semicolon while parsing behavior for";
      For (p, vname, count, ref 0, own_env)
  | WHILE ->
      let own_env = { values = StringMap.empty; next = Some parent_env } in
      let cond = parse_boolean_expression token_stream own_env in
      let p = parse_behavior token_stream own_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected semicolon while parsing behavior while";
      While (p, cond, own_env)
  | DIE ->
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing behavior die";
      Single (Die, False, parent_env)
  | SLEEP ->
      if consume_token token_stream <> UNTIL then
        failwith "expected Until while parsing behavior sleep";
      let cond = parse_boolean_expression token_stream parent_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing behavior sleep";
      Single (Sleep, cond, parent_env)
  | SET_ALL -> (
      match peek_token token_stream with
      | STRING tag ->
          ignore (consume_token token_stream);
          let tag = tag in
          let xx = parse_number_expression token_stream parent_env in
          let yy = parse_number_expression token_stream parent_env in
          let tt = parse_number_expression token_stream parent_env in
          let ss = parse_number_expression token_stream parent_env in
          let ta = parse_boolean_expression token_stream parent_env in
          let op = parse_number_expression token_stream parent_env in
          if consume_token token_stream <> UNTIL then
            failwith "expected Until while parsing behavior set_all";
          let cond = parse_boolean_expression token_stream parent_env in
          if consume_token token_stream <> SEMICOLON then
            failwith "expected ; while parsing behavior set_all";
          Single (Set_all (tag, xx, yy, tt, ss, ta, op), cond, parent_env)
      | _ -> failwith "expected a string for tag while parsing behavior set_all"
      )
  | SET_X ->
      let xx = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> UNTIL then
        failwith "expected Until while parsing behavior set_x";
      let cond = parse_boolean_expression token_stream parent_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing behavior set_x";
      Single (Set_X xx, cond, parent_env)
  | SET_Y ->
      let yy = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> UNTIL then
        failwith "expected Until while parsing behavior set_y";
      let cond = parse_boolean_expression token_stream parent_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing behavior set_y";
      Single (Set_Y yy, cond, parent_env)
  | SET_XY ->
      let xx = parse_number_expression token_stream parent_env in
      let yy = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> UNTIL then
        failwith "expected Until while parsing behavior set_xy";
      let cond = parse_boolean_expression token_stream parent_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing behavior set_xy";
      Single (Set_XY (xx, yy), cond, parent_env)
  | SET_ANGLE ->
      let tt = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> UNTIL then
        failwith "expected Until while parsing behavior set_angle";
      let cond = parse_boolean_expression token_stream parent_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing behavior set_angle";
      Single (Set_angle tt, cond, parent_env)
  | SET_SPEED ->
      let tt = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> UNTIL then
        failwith "expected Until while parsing behavior set_speed";
      let cond = parse_boolean_expression token_stream parent_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing behavior set_speed";
      Single (Set_speed tt, cond, parent_env)
  | SET_TANGIBLE ->
      let bx = parse_boolean_expression token_stream parent_env in
      if consume_token token_stream <> UNTIL then
        failwith "expected Until while parsing behavior set_tangible";
      let cond = parse_boolean_expression token_stream parent_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing behavior set_tangible";
      Single (Set_tangible bx, cond, parent_env)
  | SET_OPACITY ->
      let tt = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> UNTIL then
        failwith "expected Until while parsing behavior set_opacity";
      let cond = parse_boolean_expression token_stream parent_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing behavior set_opacity";
      Single (Set_opacity tt, cond, parent_env)
  | SET_VELOCITY ->
      let ss = parse_number_expression token_stream parent_env in
      let tt = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> UNTIL then
        failwith "expected Until while parsing behavior set_velocity";
      let cond = parse_boolean_expression token_stream parent_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing behavior set_velocity";
      Single (Set_velocity (tt, ss), cond, parent_env)
  | DEFINE -> (
      let vn = consume_token token_stream in
      match vn with
      | VARNAME name ->
          if consume_token token_stream <> EQ then
            failwith "expected = while parsing behavior define";
          let va = parse_number_expression token_stream parent_env in
          if consume_token token_stream <> SEMICOLON then
            failwith "expected ; while parsing behavior define";
          Single (Define_variable (name, va), Instant, parent_env)
      | _ ->
          failwith
            ("while parsing behavior define Parsing error: expected varname but got: "
           ^ string_of_token vn))
  | UPDATE -> (
      let vn = consume_token token_stream in
      match vn with
      | VARNAME name ->
          if consume_token token_stream <> EQ then
            failwith "expected = while parsing behavior update";
          let va = parse_number_expression token_stream parent_env in
          if consume_token token_stream <> SEMICOLON then
            failwith "expected ; while parsing behavior update";
          Single (Update_variable (name, va), Instant, parent_env)
      | _ ->
          failwith
            ("while parsing behavior update Parsing error: expected varname but got: "
           ^ string_of_token vn))
  | STOP ->
      if consume_token token_stream <> UNTIL then
        failwith "expected Until while parsing behavior stop";
      let cond = parse_boolean_expression token_stream parent_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing behavior stop";
      Single (Set_speed (Number (Float 0.)), cond, parent_env)
  | CUSTOM ->
      let s = parse_number_expression token_stream parent_env in
      let theta = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> UNTIL then
        failwith "expected Until while parsing behavior custom";
      let cond = parse_boolean_expression token_stream parent_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing behavior custom";
      Single (Set_velocity (theta, s), cond, parent_env)
  | CUSTOM_RECT ->
      let dx = parse_number_expression token_stream parent_env in
      let dy = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> UNTIL then
        failwith "expected Until while parsing behavior custom_rect";
      let cond = parse_boolean_expression token_stream parent_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing behavior custom_rect";
      let calcangle = Atan2 (dy, dx) in
      let calcspeed =
        Sqrt (Plus (Pow (dx, Number (Float 2.)), Pow (dy, Number (Float 2.))))
      in
      Single (Set_velocity (calcangle, calcspeed), cond, parent_env)
  | FORWARDS ->
      let speed = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> UNTIL then
        failwith "expected Until while parsing behavior forwards";
      let cond = parse_boolean_expression token_stream parent_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing behavior forwards";
      Single (Set_speed speed, cond, parent_env)
  | POINT ->
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing behavior point";
      let xx = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> COMMA then
        failwith "expected , while parsing behavior point";
      let yy = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing behavior point";
      let speed = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> UNTIL then
        failwith "expected Until while parsing behavior point";
      let cond = parse_boolean_expression token_stream parent_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing behavior point";
      let dx = Minus (xx, Query Self_X) in
      let dy = Minus (Query Self_Y, yy) in
      let calcangle = Minus (Number (Float 0.), Atan2 (dy, dx)) in
      Single (Set_velocity (calcangle, speed), cond, parent_env)
  | ORBIT ->
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing behavior orbit";
      let ox = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> COMMA then
        failwith "expected , while parsing behavior orbit";
      let oy = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing behavior orbit";
      let radial_speed = parse_number_expression token_stream parent_env in
      let angular_speed = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> UNTIL then
        failwith "expected Until while parsing behavior orbit";
      let cond = parse_boolean_expression token_stream parent_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing behavior orbit";

      let spaced_x =
        Plus (Query Self_X, Times (Number (Float 0.001), Cos (Query Self_theta)))
      in
      let spaced_y =
        Plus (Query Self_Y, Times (Number (Float 0.001), Sin (Query Self_theta)))
      in
      let ang = Atan2 (Minus (spaced_y, oy), Minus (spaced_x, ox)) in
      let radius = Dist ((Query Self_X, Query Self_Y), (ox, oy)) in
      let newang =
        Plus
          (Number (Variable "Orbit_pattern_angle_accumulation"), angular_speed)
      in
      let newrad =
        Plus (Number (Variable "Orbit_pattern_radius_number"), radial_speed)
      in
      let outer_env = { values = StringMap.empty; next = Some parent_env } in
      let own_env = { values = StringMap.empty; next = Some outer_env } in
      let seq_env = { values = StringMap.empty; next = Some own_env } in
      Sequence
        ( [|
            Single
              ( Define_variable ("Orbit_pattern_radius_number", radius),
                Instant,
                outer_env );
            Single
              ( Define_variable ("Orbit_pattern_angle_accumulation", ang),
                Instant,
                outer_env );
            While
              ( Sequence
                  ( [|
                      Single
                        ( Update_variable ("Orbit_pattern_radius_number", newrad),
                          Instant,
                          seq_env );
                      Single
                        ( Update_variable
                            ("Orbit_pattern_angle_accumulation", newang),
                          Instant,
                          seq_env );
                      Single
                        ( Set_XY
                            ( Plus
                                ( ox,
                                  Times
                                    ( Number
                                        (Variable "Orbit_pattern_radius_number"),
                                      Cos
                                        (Number
                                           (Variable
                                              "Orbit_pattern_angle_accumulation"))
                                    ) ),
                              Plus
                                ( oy,
                                  Times
                                    ( Number
                                        (Variable "Orbit_pattern_radius_number"),
                                      Sin
                                        (Number
                                           (Variable
                                              "Orbit_pattern_angle_accumulation"))
                                    ) ) ),
                          Instant,
                          { values = StringMap.empty; next = Some seq_env } );
                      Single
                        ( Set_angle
                            (Number
                               (Variable "Orbit_pattern_angle_accumulation")),
                          DoOnce (ref false),
                          { values = StringMap.empty; next = Some seq_env } );
                    |],
                    ref 0,
                    seq_env ),
                Not cond,
                own_env );
          |],
          ref 0,
          outer_env )
  | GRAVITATE ->
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing behavior gravitate";
      let ox = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> COMMA then
        failwith "expected , while parsing behavior gravitate";
      let oy = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing behavior gravitate";
      let force = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> UNTIL then
        failwith "expected Until while parsing behavior gravitate";
      let cond = parse_boolean_expression token_stream parent_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing behavior gravitate";
      let dist =
        Max (Number (Float 1.), Dist ((Query Self_X, Query Self_Y), (ox, oy)))
      in
      let accel_force =
        Min (Number (Float 1.), Div (force, Pow (dist, Number (Float 2.))))
      in
      let vel_x = Times (Query Self_speed, Cos (Query Self_theta)) in
      let vel_y = Times (Query Self_speed, Sin (Query Self_theta)) in
      let dx = Minus (ox, Query Self_X) in
      let dy = Minus (oy, Query Self_Y) in
      let accel_x = Times (accel_force, Div (dx, dist)) in
      let accel_y = Times (accel_force, Div (dy, dist)) in
      let newspeed =
        Dist
          ( (Number (Float 0.), Number (Float 0.)),
            (Plus (vel_x, accel_x), Plus (vel_y, accel_y)) )
      in
      let slowdown =
        Plus
          ( Number (Float 0.99),
            Times
              (Sign (Minus (newspeed, Query Self_speed)), Number (Float 0.01))
          )
      in
      let ns = Times (newspeed, slowdown) in
      let newang = Atan2 (Plus (vel_y, accel_y), Plus (vel_x, accel_x)) in
      Single (Set_velocity (newang, ns), cond, parent_env)
  | DRIFT ->
      let mu = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> UNTIL then
        failwith "expected Until while parsing behavior drift";
      let cond = parse_boolean_expression token_stream parent_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing behavior drift";
      let sign = Sign (Query Self_speed) in
      let newspeed =
        Times (sign, Max (Number (Float 0.), Minus (Abs (Query Self_speed), mu)))
      in
      Single (Set_speed newspeed, cond, parent_env)
  | STEER ->
      let speed = parse_number_expression token_stream parent_env in
      let angular_speed = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> UNTIL then
        failwith "expected Until while parsing behavior steer";
      let cond = parse_boolean_expression token_stream parent_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing behavior steer";
      let newang = Plus (Query Self_theta, angular_speed) in
      Single (Set_velocity (newang, speed), cond, parent_env)
  | ANGLED ->
      let speed = parse_number_expression token_stream parent_env in
      let angle = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> UNTIL then
        failwith "expected Until while parsing behavior angled";
      let cond = parse_boolean_expression token_stream parent_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing behavior angled";
      Single (Set_velocity (angle, speed), cond, parent_env)
  | ANGLED_REL ->
      let speed = parse_number_expression token_stream parent_env in
      let dt = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> UNTIL then
        failwith "expected Until while parsing behavior angled_rel";
      let cond = parse_boolean_expression token_stream parent_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing behavior angled_rel";
      let own_env = Util.{ values = StringMap.empty; next = Some parent_env } in
      Sequence
        ( [|
            Single (Set_angle (Plus (Query Self_theta, dt)), Instant, own_env);
            Single (Set_speed speed, cond, own_env);
          |],
          ref 0,
          own_env )
  | TURN ->
      let dt = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> UNTIL then
        failwith "expected Until while parsing behavior turn";
      let cond = parse_boolean_expression token_stream parent_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing behavior turn";
      Single (Set_angle (Plus (Query Self_theta, dt)), cond, parent_env)
  | _ -> failwith "expected a behavior"

let parse_bullet_spawn token_stream parent_env =
  if consume_token token_stream <> BULLET then failwith "expected Bullet";
  let t1 = peek_token token_stream in
  match t1 with
  | STRING tag ->
      (*the full one*)
      ignore (consume_token token_stream);
      let xx = parse_number_expression token_stream parent_env in
      let yy = parse_number_expression token_stream parent_env in
      let color =
        match consume_token token_stream with
        | WHITE -> White
        | RED -> Red
        | ORANGE -> Orange
        | YELLOW -> Yellow
        | GREEN -> Green
        | CYAN -> Cyan
        | BLUE -> Blue
        | PURPLE -> Purple
        | _ -> failwith "expected color while parsing full_custom_bullet"
      in
      let btype =
        match consume_token token_stream with
        | CIRCLE_BULLET -> Circle
        | ARROW_BULLET -> Arrow
        | KNIFE_BULLET -> Knife
        | LASER_BULLET ->
            let leng = parse_number_expression token_stream parent_env in
            Laser leng
        | TRAIL_BULLET ->
            let reps = parse_number_expression token_stream parent_env in
            let intv = parse_number_expression token_stream parent_env in
            Trail (reps, intv)
        | _ -> failwith "expected a bullet type"
      in
      let ss = parse_number_expression token_stream parent_env in
      let tt = parse_number_expression token_stream parent_env in
      let rr = parse_number_expression token_stream parent_env in
      let ttl = parse_number_expression token_stream parent_env in
      let ta = parse_boolean_expression token_stream parent_env in
      let op = parse_number_expression token_stream parent_env in
      let parsed_behavior = parse_behavior token_stream parent_env in
      if consume_token token_stream <> UNTIL then
        failwith "expected Until while parsing full_custom_bullet";
      let cond = parse_boolean_expression token_stream parent_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing full_custom_bullet";
      Bullet
        ( tag,
          xx,
          yy,
          ss,
          tt,
          ttl,
          color,
          op,
          btype,
          parsed_behavior,
          rr,
          ta,
          cond,
          parent_env )
  | WHITE | RED | ORANGE | YELLOW | GREEN | CYAN | BLUE | PURPLE ->
      (*nonpositional*)
      ignore (consume_token token_stream);
      let color =
        match t1 with
        | WHITE -> White
        | RED -> Red
        | ORANGE -> Orange
        | YELLOW -> Yellow
        | GREEN -> Green
        | CYAN -> Cyan
        | BLUE -> Blue
        | PURPLE -> Purple
        | _ -> failwith "expected color while parsing nonpositional_bullet"
      in
      let btype =
        match consume_token token_stream with
        | CIRCLE_BULLET -> Circle
        | ARROW_BULLET -> Arrow
        | KNIFE_BULLET -> Knife
        | LASER_BULLET ->
            let leng = parse_number_expression token_stream parent_env in
            Laser leng
        | TRAIL_BULLET ->
            let reps = parse_number_expression token_stream parent_env in
            let intv = parse_number_expression token_stream parent_env in
            Trail (reps, intv)
        | _ -> failwith "expected a bullet type"
      in
      let ne1 = parse_number_expression token_stream parent_env in
      let ne2 = parse_number_expression token_stream parent_env in
      let ne3 = parse_number_expression token_stream parent_env in
      (*if the next thing is UNTIL, were done*)
      if peek_token token_stream = UNTIL then (
        ignore (consume_token token_stream);
        let cond = parse_boolean_expression token_stream parent_env in
        if consume_token token_stream <> SEMICOLON then
          failwith "expected ; while parsing nonpositional_bullet";
        Bullet
          ( "",
            Query Self_X,
            Query Self_Y,
            ne1,
            Number (Float 0.),
            ne3,
            color,
            Number (Float 1.),
            btype,
            Nothing,
            ne2,
            True,
            cond,
            parent_env ))
      else
        let ne4 = parse_number_expression token_stream parent_env in
        if peek_token token_stream = UNTIL then (
          ignore (consume_token token_stream);
          let cond = parse_boolean_expression token_stream parent_env in
          if consume_token token_stream <> SEMICOLON then
            failwith "expected ; while parsing nonpositional_bullet2";
          Bullet
            ( "",
              Query Self_X,
              Query Self_Y,
              ne1,
              ne2,
              ne4,
              color,
              Number (Float 1.),
              btype,
              Nothing,
              ne3,
              True,
              cond,
              parent_env ))
        else
          let parsed_behavior = parse_behavior token_stream parent_env in
          if consume_token token_stream <> UNTIL then
            failwith "expected Until while parsing nonpositional_bullet3";
          let cond = parse_boolean_expression token_stream parent_env in
          if consume_token token_stream <> SEMICOLON then
            failwith "expected ; while parsing nonpositional_bullet3";
          Bullet
            ( "",
              Query Self_X,
              Query Self_Y,
              ne1,
              ne2,
              ne4,
              color,
              Number (Float 1.),
              btype,
              parsed_behavior,
              ne3,
              True,
              cond,
              parent_env )
      (*simpler*)
  | t ->
      let xx = parse_number_expression token_stream parent_env in
      let yy = parse_number_expression token_stream parent_env in
      let color =
        match consume_token token_stream with
        | WHITE -> White
        | RED -> Red
        | ORANGE -> Orange
        | YELLOW -> Yellow
        | GREEN -> Green
        | CYAN -> Cyan
        | BLUE -> Blue
        | PURPLE -> Purple
        | _ -> failwith "expected color while parsing positional_bullet"
      in
      let btype =
        match consume_token token_stream with
        | CIRCLE_BULLET -> Circle
        | ARROW_BULLET -> Arrow
        | KNIFE_BULLET -> Knife
        | LASER_BULLET ->
            let leng = parse_number_expression token_stream parent_env in
            Laser leng
        | TRAIL_BULLET ->
            let reps = parse_number_expression token_stream parent_env in
            let intv = parse_number_expression token_stream parent_env in
            Trail (reps, intv)
        | _ -> failwith "expected a bullet type"
      in
      let ss = parse_number_expression token_stream parent_env in
      let tt = parse_number_expression token_stream parent_env in
      let rr = parse_number_expression token_stream parent_env in
      let ttl = parse_number_expression token_stream parent_env in
      if peek_token token_stream = UNTIL then (
        ignore (consume_token token_stream);
        let cond = parse_boolean_expression token_stream parent_env in
        if consume_token token_stream <> SEMICOLON then
          failwith "expected ; while parsing positional_bullet";
        Bullet
          ( "",
            xx,
            yy,
            ss,
            tt,
            ttl,
            color,
            Number (Float 1.),
            btype,
            Nothing,
            rr,
            True,
            cond,
            parent_env ))
      else
        let parsed_behavior = parse_behavior token_stream parent_env in
        if consume_token token_stream <> UNTIL then
          failwith "expected Until while parsing positional_bullet2";
        let cond = parse_boolean_expression token_stream parent_env in
        if consume_token token_stream <> SEMICOLON then
          failwith "expected ; while parsing positional_bullet2";
        Bullet
          ( "",
            xx,
            yy,
            ss,
            tt,
            ttl,
            color,
            Number (Float 1.),
            btype,
            parsed_behavior,
            rr,
            True,
            cond,
            parent_env )

let rec parse_modification token_stream parent_env =
  let tp = consume_token token_stream in
  match tp with
  | VARNAME q -> (
      if consume_token token_stream <> EQ then
        failwith "expected = while parsing modification";
      match q with
      | "@X" ->
          {
            mod_tag = None;
            mod_tangible = None;
            mod_number_property =
              [ (X, parse_number_expression token_stream parent_env) ];
          }
      | "@Y" ->
          {
            mod_tag = None;
            mod_tangible = None;
            mod_number_property =
              [ (Y, parse_number_expression token_stream parent_env) ];
          }
      | "@S" ->
          {
            mod_tag = None;
            mod_tangible = None;
            mod_number_property =
              [ (Speed, parse_number_expression token_stream parent_env) ];
          }
      | "@A" ->
          {
            mod_tag = None;
            mod_tangible = None;
            mod_number_property =
              [ (Theta, parse_number_expression token_stream parent_env) ];
          }
      | "@O" ->
          {
            mod_tag = None;
            mod_tangible = None;
            mod_number_property =
              [ (Opacity, parse_number_expression token_stream parent_env) ];
          }
      | "@TTL" ->
          {
            mod_tag = None;
            mod_tangible = None;
            mod_number_property =
              [ (TTL, parse_number_expression token_stream parent_env) ];
          }
      | "@TAG" -> (
          let tag = consume_token token_stream in
          match tag with
          | STRING s ->
              {
                mod_tag = Some s;
                mod_tangible = None;
                mod_number_property = [];
              }
          | _ -> failwith "expected a string for tag while parsing modification"
          )
      | "@TANGIBLE" ->
          {
            mod_tag = None;
            mod_tangible =
              Some (parse_boolean_expression token_stream parent_env);
            mod_number_property = [];
          }
      | _ -> failwith "not a valid bullet property")
  | _ -> failwith "expected a bullet property while parsing modification"

let rec parse_spawn_pattern (token_stream : token_stream)
    (parent_env : float ref segmented_list) =
  (*get the next token*)
  let nt = peek_token token_stream in
  match nt with
  | NOTHING ->
      ignore (consume_token token_stream);
      let nnt = consume_token token_stream in
      if nnt = UNTIL then
        let bx = parse_boolean_expression token_stream parent_env in
        let nnnt = consume_token token_stream in
        if nnnt = SEMICOLON then Nothing bx
        else
          failwith
            ("while parsing pattern nothing Parsing error: expected ; but got: "
           ^ string_of_token nnnt)
      else
        failwith
          ("while parsing pattern nothing Parsing error: expected Until but \
            got: " ^ string_of_token nnt)
      (*check until, then parse the boolean expression, then check the
        semicolon, and return it*)
  | BULLET -> parse_bullet_spawn token_stream parent_env
  | DEFINE -> (
      ignore (consume_token token_stream);
      let vn = consume_token token_stream in
      match vn with
      | VARNAME name ->
          let igneq = consume_token token_stream in
          if igneq = EQ then
            let va = parse_number_expression token_stream parent_env in
            let ignsc = consume_token token_stream in
            if ignsc = SEMICOLON then Definition (name, va, parent_env)
            else
              failwith
                ("while parsing pattern define Parsing error: expected ; but \
                  got: " ^ string_of_token ignsc)
          else
            failwith
              ("while parsing pattern define Parsing error: expected = but \
                got: " ^ string_of_token igneq)
      | _ ->
          failwith
            ("while parsing pattern define Parsing error: expected $ but got: "
           ^ string_of_token vn))
  | UPDATE -> (
      ignore (consume_token token_stream);
      let vn = consume_token token_stream in
      match vn with
      | VARNAME name ->
          if consume_token token_stream <> EQ then
            failwith "expected = while parsing pattern update";
          let va = parse_number_expression token_stream parent_env in
          if consume_token token_stream <> SEMICOLON then
            failwith "expected ; while parsing pattern update";
          Update (name, va, parent_env)
      | _ ->
          failwith
            ("while parsing pattern update Parsing error: expected $ but got: "
           ^ string_of_token vn))
  | CONDITIONAL ->
      ignore (consume_token token_stream);
      let own_env = { values = StringMap.empty; next = Some parent_env } in
      let bx = parse_boolean_expression token_stream own_env in
      let pattern = parse_spawn_pattern token_stream own_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing conditional";
      Conditional (pattern, bx, own_env)
  | IF ->
      ignore (consume_token token_stream);
      let own_env = { values = StringMap.empty; next = Some parent_env } in
      let b0 = parse_boolean_expression token_stream own_env in
      if consume_token token_stream <> THEN then
        failwith "expected then while parsing pattern If";
      let p1 = parse_spawn_pattern token_stream own_env in
      if consume_token token_stream <> ELSE then
        failwith "expected else while parsing pattern If";
      let p2 = parse_spawn_pattern token_stream own_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing pattern If";
      If_then_else (b0, p1, p2, ref None, own_env)
  | TIMED ->
      ignore (consume_token token_stream);
      let own_env = { values = StringMap.empty; next = Some parent_env } in
      let start_time = parse_number_expression token_stream own_env in
      let end_time = parse_number_expression token_stream own_env in
      let p1 = parse_spawn_pattern token_stream own_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing pattern timed";
      Timed (start_time, end_time, p1, own_env)
  | MODIFY ->
      ignore (consume_token token_stream);
      let own_env = { values = StringMap.empty; next = Some parent_env } in
      let first_mod = parse_modification token_stream own_env in
      let rec keep_parsing acc =
        if peek_token token_stream = OF then (
          ignore (consume_token token_stream);
          acc)
        else keep_parsing (parse_modification token_stream own_env :: acc)
      in
      let modlisttemp = keep_parsing [] in
      let final_mods =
        List.fold_left
          (fun acc ml ->
            let ta = if ml.mod_tag <> None then ml.mod_tag else acc.mod_tag in
            let ac =
              if ml.mod_tangible <> None then ml.mod_tangible
              else acc.mod_tangible
            in
            let nu =
              if List.is_empty ml.mod_number_property then
                acc.mod_number_property
              else List.hd ml.mod_number_property :: acc.mod_number_property
            in
            { mod_tag = ta; mod_tangible = ac; mod_number_property = nu })
          first_mod modlisttemp
      in
      let pattern = parse_spawn_pattern token_stream own_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected ; while parsing pattern modify";
      Modify
        ( pattern,
          {
            final_mods with
            mod_number_property = List.rev final_mods.mod_number_property;
          },
          own_env )
  | ITERATE -> (
      ignore (consume_token token_stream);
      let own_env = { values = StringMap.empty; next = Some parent_env } in
      match consume_token token_stream with
      | VARNAME name ->
          if consume_token token_stream <> UPTO then
            failwith "expected upto while parsing pattern iterate";
          let count = parse_number_expression token_stream own_env in
          let p = parse_spawn_pattern token_stream own_env in
          if consume_token token_stream <> SEMICOLON then
            failwith "expected ; while parsing pattern iterate";
          Iterate (name, count, p, ref None, own_env)
      | _ -> failwith "expeced variable name while parsing pattern iterate")
  | COMBINE ->
      ignore (consume_token token_stream);
      if consume_token token_stream <> LBRACKET then
        failwith "expected lbracket while parsing pattern combine";
      let own_env = { values = StringMap.empty; next = Some parent_env } in
      let first_pattern = parse_spawn_pattern token_stream own_env in
      let rec keep_parsing acc =
        if peek_token token_stream = RBRACKET then (
          ignore (consume_token token_stream);
          acc)
        else keep_parsing (parse_spawn_pattern token_stream own_env :: acc)
      in
      let sl = first_pattern :: List.rev (keep_parsing []) in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected semicolon while parsing pattern combine";
      Combo (sl, own_env)
  | SEQUENCE ->
      ignore (consume_token token_stream);
      if consume_token token_stream <> LBRACKET then
        failwith "expected lbracket while parsing pattern sequence";
      let own_env = { values = StringMap.empty; next = Some parent_env } in

      let first_pattern = parse_spawn_pattern token_stream own_env in
      let rec keep_parsing acc =
        if peek_token token_stream = RBRACKET then (
          ignore (consume_token token_stream);
          acc)
        else keep_parsing (parse_spawn_pattern token_stream own_env :: acc)
      in
      let sl = Array.of_list (first_pattern :: List.rev (keep_parsing [])) in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected semicolon while parsing pattern sequence";
      Sequence (sl, ref 0, own_env)
  | FOR ->
      ignore (consume_token token_stream);
      let vname =
        match consume_token token_stream with
        | VARNAME x -> x
        | _ -> failwith "expected variable name while parsing pattern for"
      in
      if consume_token token_stream <> UPTO then
        failwith "expected upto while parsing pattern for";
      let own_env =
        {
          values = StringMap.add vname (ref 0.) StringMap.empty;
          next = Some parent_env;
        }
      in
      let count = parse_number_expression token_stream own_env in
      let p = parse_spawn_pattern token_stream own_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected semicolon while parsing pattern for";
      For (p, vname, count, ref 0, own_env)
  | WHILE ->
      ignore (consume_token token_stream);
      let own_env = { values = StringMap.empty; next = Some parent_env } in
      let cond = parse_boolean_expression token_stream own_env in
      let p = parse_spawn_pattern token_stream own_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected semicolon while parsing pattern while";
      While (p, cond, own_env)
  | ANGLED ->
      ignore (consume_token token_stream);
      let own_env = { values = StringMap.empty; next = Some parent_env } in
      let angle = parse_number_expression token_stream own_env in
      let p = parse_spawn_pattern token_stream own_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected semicolon while parsing pattern angled";
      Modify
        ( p,
          {
            mod_tag = None;
            mod_tangible = None;
            mod_number_property =
              [ (Theta, Plus (Number (Variable "@A"), angle)) ];
          },
          own_env )
  | PULSE ->
      ignore (consume_token token_stream);
      let own_env = { values = StringMap.empty; next = Some parent_env } in
      let cond_env = { values = StringMap.empty; next = Some own_env } in
      let interval = parse_number_expression token_stream cond_env in
      let p = parse_spawn_pattern token_stream cond_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected semicolon while parsing pattern pulse";

      Sequence
        ( [|
            Definition ("Pulse_pattern_start_time", Query Self_lifetime, own_env);
            Conditional
              ( p,
                LT
                  ( Mod
                      ( Minus
                          ( Query Self_lifetime,
                            Number (Variable "Pulse_pattern_start_time") ),
                        Plus (interval, Number (Float 1.)) ),
                    Number (Float 0.1) ),
                cond_env );
          |],
          ref 0,
          own_env )
  | AIMED ->
      ignore (consume_token token_stream);
      let own_env = { values = StringMap.empty; next = Some parent_env } in
      let p = parse_spawn_pattern token_stream own_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected semicolon while parsing pattern aimed";
      Modify
        ( p,
          {
            mod_tag = None;
            mod_tangible = None;
            mod_number_property =
              [
                ( Theta,
                  Plus
                    ( Number (Variable "@A"),
                      Atan2
                        ( Minus (Query Player_Y, Number (Variable "@Y")),
                          Minus (Query Player_X, Number (Variable "@X")) ) ) );
              ];
          },
          own_env )
  | SPIN ->
      ignore (consume_token token_stream);
      let own_env = { values = StringMap.empty; next = Some parent_env } in
      if consume_token token_stream <> LPAREN then
        failwith "expected ( while parsing spin";
      let ox = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> COMMA then
        failwith "expected , while parsing spin";
      let oy = parse_number_expression token_stream parent_env in
      if consume_token token_stream <> RPAREN then
        failwith "expected ) while parsing spin";
      let angular_velocity = parse_number_expression token_stream own_env in
      let pattern = parse_spawn_pattern token_stream own_env in
      let combo_env = { values = StringMap.empty; next = Some own_env } in
      let mod_env1 = { values = StringMap.empty; next = Some combo_env } in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected semicolon while parsing pattern spin";
      Sequence
        ( [|
            Definition ("spin_angle_delta_variable", Number (Float 0.), own_env);
            Combo
              ( Update
                  ( "spin_angle_delta_variable",
                    Plus
                      ( Number (Variable "spin_angle_delta_variable"),
                        angular_velocity ),
                    combo_env )
                :: [
                     Modify
                       ( pattern,
                         {
                           mod_tag = None;
                           mod_tangible = None;
                           mod_number_property =
                             [
                               ( X,
                                 Plus
                                   ( ox,
                                     Times
                                       ( Dist
                                           ( (ox, oy),
                                             ( Number (Variable "@X"),
                                               Number (Variable "@Y") ) ),
                                         Cos
                                           (Plus
                                              ( Atan2
                                                  ( Minus
                                                      ( Number (Variable "@Y"),
                                                        oy ),
                                                    Minus
                                                      ( Number (Variable "@X"),
                                                        ox ) ),
                                                Number
                                                  (Variable
                                                     "spin_angle_delta_variable")
                                              )) ) ) );
                               ( Y,
                                 Plus
                                   ( oy,
                                     Times
                                       ( Dist
                                           ( (ox, oy),
                                             ( Number (Variable "@X"),
                                               Number (Variable "@Y") ) ),
                                         Sin
                                           (Plus
                                              ( Atan2
                                                  ( Minus
                                                      ( Number (Variable "@Y"),
                                                        oy ),
                                                    Minus
                                                      ( Number (Variable "@X"),
                                                        ox ) ),
                                                Number
                                                  (Variable
                                                     "spin_angle_delta_variable")
                                              )) ) ) );
                               ( Theta,
                                 Plus
                                   ( Number (Variable "@A"),
                                     Number
                                       (Variable "spin_angle_delta_variable") )
                               );
                             ];
                         },
                         mod_env1 );
                   ],
                combo_env );
          |],
          ref 0,
          own_env )
  | ARC ->
      ignore (consume_token token_stream);
      let own_env = { values = StringMap.empty; next = Some parent_env } in
      let count = parse_number_expression token_stream own_env in
      let start_angle = parse_number_expression token_stream own_env in
      let end_angle = parse_number_expression token_stream own_env in
      let radius = parse_number_expression token_stream own_env in
      let pattern = parse_spawn_pattern token_stream own_env in
      if consume_token token_stream <> SEMICOLON then
        failwith "expected semicolon while parsing pattern arc";
      let seq_env = { values = StringMap.empty; next = Some own_env } in
      let mod_env1 = { values = StringMap.empty; next = Some seq_env } in
      let mod_env2 = { values = StringMap.empty; next = Some mod_env1 } in
      let arc_pattern_angle =
        Plus
          ( start_angle,
            Times
              ( Div (Minus (end_angle, start_angle), count),
                Number (Variable "arc_pattern_index") ) )
      in
      Iterate
        ( "arc_pattern_index",
          count,
          Sequence
            ( [|
                Modify
                  ( Modify
                      ( pattern,
                        {
                          mod_tag = None;
                          mod_tangible = None;
                          mod_number_property =
                            [
                              ( Ast.Y,
                                Plus
                                  ( Number (Variable "@Y"),
                                    Times (radius, Sin (Number (Variable "@A")))
                                  ) );
                              ( Ast.X,
                                Plus
                                  ( Number (Variable "@X"),
                                    Times (radius, Cos (Number (Variable "@A")))
                                  ) );
                            ];
                        },
                        mod_env2 ),
                    {
                      mod_tag = None;
                      mod_tangible = None;
                      mod_number_property =
                        [
                          ( Ast.Theta,
                            Plus (Number (Variable "@A"), arc_pattern_angle) );
                        ];
                    },
                    mod_env1 );
              |],
              ref 0,
              seq_env ),
          ref None,
          own_env )
  | t -> failwith ("expected a spawn pattern but got: " ^ string_of_token t)
