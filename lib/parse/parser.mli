open Tokenizer
open Ast
open Util

type token_stream = token list ref

(** Parse a stream of tokens into a spawn_pattern AST*)
val parse_spawn_pattern :
  token_stream -> float ref segmented_list -> Ast.bullet_pattern

(** Parse a stream of tokens into a behavior AST*)
val parse_behavior : token_stream -> float ref segmented_list -> Ast.behavior
