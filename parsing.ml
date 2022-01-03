open import "prelude.ml"
open import "./parsing/lpeg.ml"
open import "./ast-temp.ml"

let opt pat = pat `rep` -1
let neg pat = pat `alt` one (* stub *)
let eof = neg star
let nsepseq pat sep = pat `seq` (sep `seq` pat `rep` 0)
let sepseq pat sep = opt (nsepseq pat sep)
let lit pat v = pat `seq` cc v
let lit_else pat v a = lit pat v `alt` cc a

let num = r "09"
let alpha_upper = r "AZ"
let alpha_lower = r "az"
let alpha = alpha_upper `alt` alpha_lower
let alnum = alpha `alt` num
let alnum_ext = alnum `alt` p "_"
(* highly restrictive punctuation rn
 * can always be relaxed later *)
let punct_op = s "#&*+-/<=>@^"
let punct_key = s "!\"$%'(),.:;?[\\]`{|}~"
let printable = alnum_ext `alt` punct_op `alt` punct_key

let ws = s "\t\n\r "
let wss = ws `rep` 0
let wsp = ws `rep` 1
let wss_after what = what `seq` wss
let wsp_after what = what `seq` wsp
let keysym what = wss_after (p what)
let keyword what = wsp_after (p what)

let punct_modifier = s "*?"
let assignment_shenanigans = p "=" (* stub *)

(* Let's think about this a bit.
 * tl;dr parsers need to be inside a megaparser,
 *       types need a megatype,
 *       also pattern matches need to have their own thing
 * The input is an expression.
 * An expression can be many things, including a number,
 * a basic identifier, a sequence of expressions, etc.
 * That's important to note: sequence of *expressions*.
 * There can be arbitrary expressions in a sequence,
 * including nested sequences.
 * This means the expression parser needs to be recursive
 * in a special way: the parsers for each type of expression,
 * especially those recursive ones, need to be aware of the
 * whole expression parser, including themselves.
 * This forces a particular pattern:
 *   let rec expression_parser =
 * followed by a bunch of lets containing each subparser,
 * until it all comes together in a big `alt` stack.
 * The return types of each parser in the alt stack need
 * to be homogenous (the same) so they can't just return
 * whatever they want. They need to return what they
 * want, though, so some type magic is needed:
 * the expression_type, which contains alternative
 * cases for each type of expression, and each case
 * can have whatever contents the particular corresponding
 * parser wants to return.
 * Pattern matching is also a bit special.
 * The syntax for pattern matching in the design
 * tries to stay as close as possible to the syntax
 * of writing an expression that matches the pattern.
 * However, not all expressions are pattern matches.
 * In particular, operators cannot be pattern matched.
 * So pattern matches must only recurse with
 * pattern-matchable syntaxes.
 * This probably requires pattern matches to have
 * their own parsing system, separate from
 * (but still reachable from) the expression parser.
 *)

(* Types of things *)
(* "You... do realize that the type system is meant to constrain values, right?"
 * "No. No, that doesn't sound right." *)

type suffix_operator_identifier_name_type = SuffixParen | SuffixNamed of string
DERIVE_BATTERIES suffix_operator_identifier_name_type SuffixParen SuffixNamed(name)
let suffix_operator_identifier_name_converter name = SuffixNamed name

type let_expression_type = Let | LetRec
DERIVE_BATTERIES let_expression_type Let LetRec
(* TODO: how to support pattern matching? *)
type let_expression_binding_type 'expr = LetSimple of string * 'expr | LetFunction of string * list string * 'expr
DERIVE_BATTERIES let_expression_binding_type ('expr) LetSimple(name expr) LetFunction(name args expr)
let let_expression_simple_binding_converter (name, expr) = LetSimple (name, expr)
let let_expression_function_binding_converter (name, args, expr) = LetFunction (name, args, expr)

(* fix fixes everything *)
type fix 'f = Fix of 'f string (fix 'f)
let unfix (Fix x) = x

(* term type comes from ast.ml *)
let expression_basic_identifier_converter name = Fix (Identifier name)
let expression_sequence_converter xs = ListCons xs
let expression_table_converter kvs = RecordCons kvs
(*let expression_anonymous_function_converter (args, expr) = ExprFunctionAnon (args, expr)
let expression_function_call_converter (name, args) = ExprFunctionCall (name, args)
let expression_let_expression_converter (kind, binding, expr) = LetBinding (kind, binding, expr)*)

(* Non-recursive parsers *)

(* Identifiers *)
(* basic id *)
let basic_identifier_parser = c (alpha_lower `seq` (alnum_ext `rep` 0))
(* consume following whitespace *)
let basic_identifier_parser_w = wss_after basic_identifier_parser
(* other identifiers *)
let constructor_identifier_parser = c (alpha_upper `seq` (alnum_ext `rep` 0))
(* simplefix means infix prefix or simple suffix *)
let simplefix_operator_identifier_parser = c (punct_op `rep` 1)
(* this is the fancier suffix notation *)
let suffix_operator_identifier_parser = collect_tuple (p "." `seq` c (punct_modifier `rep` 0) `seq` (basic_identifier_parser_w `act` suffix_operator_identifier_name_converter `alt` lit (keysym "()") SuffixParen) `seq` c (opt assignment_shenanigans))

(* Recursive parsers *)

(* Pattern matching first *)
(* TODO lol *)

(* Arbitrary expressions next *)
let rec expression_parser =
  (* Basic identifier*)
  let basic_identifier_expression_parser = basic_identifier_parser_w `act` expression_basic_identifier_converter

(*  (* Structures *)
  let sequence_parser = keysym "[" `seq` collect_list (sepseq expression_parser (keysym ",")) `seq` keysym "]" `act` expression_sequence_converter
  (* TODO: other key syntaxes *)
  let table_key_parser = basic_identifier_parser_w
  let table_keyval_parser = collect_tuple (table_key_parser `seq` keysym "=" `seq` expression_parser)
  let table_parser = keysym "{" `seq` collect_list (sepseq table_keyval_parser (keysym ",")) `seq` keysym "}" `act` expression_table_converter
 
  (* Functions *)
  let partial_function_arguments_parser what = keysym "(" `seq` collect_list (sepseq what (keysym ",")) `seq` keysym ")"
  let function_arguments_call_parser = partial_function_arguments_parser expression_parser
  let function_call_parser = collect_tuple (basic_identifier_parser `seq` function_arguments_call_parser)
 
  (* Anonymous functions *)
  (* TODO: function arguments pattern matching *)
  let function_arguments_definition_parser = partial_function_arguments_parser basic_identifier_parser_w
  let partial_function_definition_parser what = collect_tuple (what `seq` function_arguments_definition_parser `seq` keysym "=" `seq` expression_parser)
  (* i swear to fuck alicorn cannot come soon enough *)
  let partial_function_definition_parser2 what = collect_tuple (what `seq` function_arguments_definition_parser `seq` keysym "=" `seq` expression_parser)
  let anonymous_function_definition_parser = partial_function_definition_parser (p "fun") `act` expression_anonymous_function_converter

  (* Let expressions *)
  (* TODO: let expression pattern matching *)
  let simple_binding_parser = collect_tuple (basic_identifier_parser_w `seq` keysym "=" `seq` expression_parser) `act` let_expression_simple_binding_converter
  let function_binding_parser = partial_function_definition_parser2 basic_identifier_parser `act` let_expression_function_binding_converter
  let let_expression_parser = collect_tuple (keyword "let" `seq` (lit_else (keyword "rec") LetRec Let) `seq` (simple_binding_parser `alt` function_binding_parser) `seq` keyword "in" `seq` expression_parser) `act` expression_let_expression_converter
*)
  (* Finally *)
  in basic_identifier_expression_parser (*`alt` sequence_parser `alt` table_parser `alt` anonymous_function_definition_parser `alt` function_call_parser `alt` let_expression_parser*)

(* TESTS *)
(* Lists of (test, expected result) pairs.
 * Each list is divided (by an empty line) into 3 parts:
 *   tests that should succeed;
 *   tests that should fail;
 *   tests that succeed but are questionable.
 * In some cases the last part is meant to highlight ambiguities in Open's description
 *)

let basic_identifier_parser_tests = [
  ("list", Some "list"),
  ("sum", Some "sum"),
  ("foldl", Some "foldl"),
  ("this_is_a_s1lly_but_v4l1d_identifier", Some "this_is_a_s1lly_but_v4l1d_identifier"),

  ("_", None),
  ("Foo", None),
  ("++", None),
  ("_internal", None),

  ("x___", Some "x___"),
  ("iCons", Some "iCons"),
  ("e", Some "e")
]

let constructor_identifier_parser_tests = [
  ("Left", Some "Left"),
  ("Right", Some "Right"),
  ("Nil", Some "Nil"),
  ("Cons", Some "Cons"),

  ("::", None),
  ("_Cons", None),
  ("foo", None),

  ("CONSTRUCTOR_IDENTIFIER", Some "CONSTRUCTOR_IDENTIFIER"),
  ("B", Some "B")
]

let infix_operator_identifier_parser_tests = [
  ("+", Some "+"),
  ("-", Some "-"),
  ("^", Some "^"),
  ("&&", Some "&&"),
  ("<+>", Some "<+>"),
  ("@", Some "@"),
  ("<@>", Some "<@>"),
  ("*", Some "*"),
  ("/", Some "/"),
  (*("%", Some "%"),*)
  ("<=", Some "<="),
  ("==", Some "=="),
  (*("!=", Some "!="),*)
  ("<>", Some "<>"),
  (">=", Some ">="),
  (*("||", Some "||"),*)
  (*(";", Some ";"),*)
  (*("?:", Some "?:"),*)

  ("$?", None),
  ("mappend", None),
  (".()", None),
  (",", None),

  ("/*", Some "/*"),
  ("#&*+-/<=>@^", Some "#&*+-/<=>@^")
]

let prefix_operator_identifier_parser_tests = [
  ("+", Some "+"),
  ("-", Some "-"),
  ("++", Some "++"),
  ("#", Some "#"),
  (*("!", Some "!"),*)
  (*("~", Some "~"),*)
  ("&", Some "&"),
  ("*", Some "*"),

  ("len", None),
  ("map", None),
  ("(", None)
]

let simple_suffix_operator_identifier_parser_tests = [
  ("++", Some "++"),
  ("--", Some "--")
  (*("?", Some "?")*)
]

let suffix_operator_identifier_parser_tests = [
  (".foo", Some ("", SuffixNamed "foo", "")),
  (".bar", Some ("", SuffixNamed "bar", "")),
  (".await", Some ("", SuffixNamed "await", "")),
  (".()", Some ("", SuffixParen, "")),
  (".foo=", Some ("", SuffixNamed "foo", "=")),
  (".()=", Some ("", SuffixParen, "=")),

  ("foo", None),
  ("Cons", None),
  (":", None),
  ("==", None),
  ("++", None),
  (".()()()()", None),
  (".().().().().", None),
  ("...===", None),
  (".frob_with()via()", None),

  (".yOU_AbsolUTE_GOBLIN", Some ("", SuffixNamed "yOU_AbsolUTE_GOBLIN", ""))
]

let expression_parser_tests = [
  ("[a,b,c]", Some (ListCons [Identifier "a", Identifier "b", Identifier "c"])),
  ("[ a, b, c ]", Some (ListCons [Identifier "a", Identifier "b", Identifier "c"])),
  ("{a=b,c=d,e=f}", Some (RecordCons [(Identifier "a", Identifier "b"), (Identifier "c", Identifier "d"), (Identifier "e", Identifier "f")])),
  ("{ a=b, c = d, e= f }", Some (RecordCons [(Identifier "a", Identifier "b"), (Identifier "c", Identifier "d"), (Identifier "e", Identifier "f")]))
  (*("fun(a, b, c) = body", Some (ExprFunctionAnon (["a", "b", "c"], ExprId "body"))),
  ("foo(a, b, c)", Some (ExprFunctionCall ("foo", [ExprId "a", ExprId "b", ExprId "c"]))),
  ("let name = expr in body", Some (ExprLet (Let, LetSimple ("name", ExprId "expr"), ExprId "body"))),
  ("let rec name = expr in body", Some (ExprLet (LetRec, LetSimple ("name", ExprId "expr"), ExprId "body"))),
  ("let foo(a, b, c) = foocode in body", Some (ExprLet (Let, LetFunction ("foo", ["a", "b", "c"], ExprId "foocode"), ExprId "body")))*)
]

let parser_test parser (test, expected) =
  let got = parse (parser `seq` eof) test
  (expected == got, test, expected, got)
let filter_failing results = filter (fun (success, _) -> not success) results
let run_tests (parser, tests) =
  let results = parser_test parser <$> tests
  let fails = filter_failing results
  map print fails

let identifier_tests = [
  (basic_identifier_parser, basic_identifier_parser_tests),
  (constructor_identifier_parser, constructor_identifier_parser_tests),
  (simplefix_operator_identifier_parser, infix_operator_identifier_parser_tests),
  (simplefix_operator_identifier_parser, prefix_operator_identifier_parser_tests),
  (simplefix_operator_identifier_parser, simple_suffix_operator_identifier_parser_tests)
]
let _ = map run_tests identifier_tests
let _ = run_tests (suffix_operator_identifier_parser, suffix_operator_identifier_parser_tests)
let _ = run_tests (expression_parser, expression_parser_tests)
