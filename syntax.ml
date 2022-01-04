
open import "prelude.ml"
open import "./parsing/lpeg.ml"
open import "./parsing/utils.ml"
open import "./ast-temp.ml"
open import "./result.ml"
open import "./hylo-temp.ml"
open import "./utils.ml"
module Map = import "data/map.ml"

type position = Pos of int
let get_pos() = cp

type pterm <- fix (term string)

let rec intercalate s ss = match ss with
| Cons (a, Cons (b, tl)) -> a ^ s ^ intercalate s (Cons (b, tl))
| Cons (a, Nil) -> a
| Nil -> ""

let matchingbracket = Map.from_list [("(|", "|)"), ("[|", "|]"), ("{|", "|}")]

let showterm = cata (function
  | LiteralBool b -> show b
  | StringCons (s, ts) -> "\"" ^ s ^ (map (fun (t, s) -> "$("^t^")"^s) ts |> foldl (^) "") ^ "\""
  | ListCons ts -> intercalate ", " ts
  | RecordCons tts -> map (fun (a, b) -> a ^ " = " ^ b) tts |> intercalate ", "
  | Identifier name -> name
  | Application (f, xs) -> f ^ "(" ^ intercalate ", " xs ^ ")"
  | PartialApplication (f, xs) -> f ^ "(" ^ intercalate "," (map (function | Some x -> x | None -> "_") xs) ^")"
  | InfixOp (a, op, b) -> "("^a^" "^op^" "^b^")"
  | PartialInfixOp (a, op, b) -> "("^(a `or_default` "_")^" "^op^" "^(b `or_default` "_")^")"
  | PrefixOp (op, b) -> "("^op^b^")"
  | PartialPrefixOp (op, b) -> "("^op^(b `or_default` "_")^")"
  | SuffixOp (a, op) -> "("^a^op^")"
  | PartialSuffixOp (a, op) -> "("^(a `or_default` "_")^op^")"
  | Abstraction (ids, body) -> "fun ("^ intercalate ", " ids ^ ") = " ^ body ^ ")"
  | LetBinding (id, def, body) -> "let " ^ id ^ " = " ^ def ^ " in " ^ body
  | Conditional (cond, cons, alt) -> "if " ^ cond ^ " then " ^ cons ^ " else " ^ alt
  | Hole id -> "$?"^id)

let escapechars = lit "\\n" "\n" `alt` lit "\\t" "\t"
(* TODO: fix this *)
(*
let stringfrag = cs (((neg @@ s "\"$\\") `seq` star `alt` escapechars `rep` 0))
*)

let num = r "09"
let alpha_upper = r "AZ"
let alpha_lower = r "az"
let alpha = alpha_upper `alt` alpha_lower
let alnum = alpha `alt` num
let alnum_ext = alnum `alt` p "_"
let eof = neg star

let keyword str = p str `seq` wsp
let keysym str = p str `seq` wsq
let basic_id = c (alpha_lower `seq` (alnum_ext `rep` 0))
(* TODO: that wsp is sus, what about a tailgating operator? *)
let literal_bool: parser1 pterm = lit "true" true `alt` lit "false" false `act` LiteralBool `act` Fix `seq` wsp
(* TODO: is wsq correct? *)
let identifier = basic_id `act` Identifier `act` Fix `seq` wsq
(* TODO: metaprogram this away *)
let string_cons: parser1 pterm = v "string_cons"
let list_cons: parser1 pterm = v "list_cons"
let record_cons: parser1 pterm = v "record_cons"
let let_binding: parser1 pterm = v "let_binding"
let term: parser1 pterm = v "term"

let parser =
  grammar {
  (*
    string_cons = p"\""
    `seq` stringfrag
    `seq` collect_list( collect_tuple (
      p"$" `seq` (identifier `alt` (p"(" `seq` term `seq` p")"))
      `seq` stringfrag) `rep` 0)
  *)
  (* don't forget the comma here *)
    list_cons = keysym "[" `seq` collect_list (sepseq term (keysym ",")) `seq` keysym "]" `act` ListCons `act` Fix
  , record_cons =
      let record_key = identifier `alt` (* string_cons `alt` *) (keysym "(" `seq` term `seq` keysym ")")
      let record_pair = collect_tuple (record_key `seq` keysym "=" `seq` term)
      in keysym "{" `seq` collect_list (sepseq record_pair (keysym ",")) `seq` keysym "}" `act` RecordCons `act` Fix
  (* TODO: is keysym "in" correct? *)
  (* TODO: rec bindings *)
  (* TODO: function bindings *)
  , let_binding = keyword "let" `seq` collect_tuple (basic_id `seq` keysym "=" `seq` term `seq` keysym "in" `seq` term) `act` LetBinding `act` Fix
  , term = literal_bool `alt` (* string_cons `alt` *) list_cons `alt` record_cons `alt` identifier `alt` let_binding
  } term

(* TESTS *)
(* (test, expected result) pairs.
 * Each section may be divided into 3 parts:
 *   tests that should succeed;
 *   tests that should fail;
 *   tests that succeed but are questionable.
 * In some cases the last part is meant to highlight ambiguities in Open's description
 *)
let parser_tests: list (string * option pterm) = [(*
  (* Basic identifiers *)

  ("list", Some (Identifier "list")),
  ("sum", Some (Identifier "sum")),
  ("foldl", Some (Identifier "foldl")),
  ("this_is_a_s1lly_but_v4l1d_identifier", Some (Identifier "this_is_a_s1lly_but_v4l1d_identifier")),

  ("_", None),
  ("++", None),
  ("_internal", None),

  ("x___", Some (Identifier "x___")),
  ("iCons", Some (Identifier "iCons")),
  ("e", Some (Identifier "e")),

  (* TODO: other tests (depends on the parsers) *)

  (* Lists *)

  ("[a,b,c]", Some (ListCons [Identifier "a", Identifier "b", Identifier "c"])),
  ("[ a, b, c ]", Some (ListCons [Identifier "a", Identifier "b", Identifier "c"])),

  (* Records *)

  ("{a=b,c=d,e=f}", Some (RecordCons [(Identifier "a", Identifier "b"), (Identifier "c", Identifier "d"), (Identifier "e", Identifier "f")])),
  ("{ a=b, c = d, e= f }", Some (RecordCons [(Identifier "a", Identifier "b"), (Identifier "c", Identifier "d"), (Identifier "e", Identifier "f")])),

  (* Anonymous functions *)

  (*("fun(a, b, c) = body", Some (ExprFunctionAnon (["a", "b", "c"], ExprId "body"))),*)

  (* Function application *)

  (*("foo(a, b, c)", Some (ExprFunctionCall ("foo", [ExprId "a", ExprId "b", ExprId "c"]))),*)

  (* Let expressions *)
  ("let name = expr in body", Some (LetBinding ("name", Identifier "expr", Identifier "body")))
  (* don't forget the comma at the end of the previous line *)
  (*("let rec name = expr in body", Some (ExprLet (LetRec, LetSimple ("name", ExprId "expr"), ExprId "body"))),*)
  (*("let foo(a, b, c) = foocode in body", Some (ExprLet (Let, LetFunction ("foo", ["a", "b", "c"], ExprId "foocode"), ExprId "body")))*)
*)]

let parser_test parser (test, expected) =
  let got = parse (parser `seq` eof) test
  (expected == got, test, expected, got)
let filter_failing results = filter (fun (success, _) -> not success) results
let run_tests (parser, tests) =
  let results = parser_test parser <$> tests
  let fails = filter_failing results
  map print fails
let _ = run_tests (parser, parser_tests)
