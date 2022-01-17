
open import "prelude.ml"
open import "./parsing/lpeg.ml"
open import "./parsing/utils.ml"
open import "./ast.ml"
open import "./hylo.ml"

let num = r "09"
let alpha_upper = r "AZ"
let alpha_lower = r "az"
let alpha = alpha_upper `alt` alpha_lower
let alnum = alpha `alt` num
let alnum_ext = alnum `alt` p "_"

let id_basic_shy = c (alpha_lower `seq` (alnum_ext `rep` 0))
let id_basic = id_basic_shy `seq` wsq
let id_constructor = c (alpha_upper `seq` (alnum_ext `rep` 0)) `seq` wsq
(* TODO: different infix precedences *)
let id_infix = c (s "&+-<@^" `seq` (s "&+-@>" `rep` 0)) `seq` wsq
let id_prefix = c (s "#+-" `rep` 1) `seq` wsq
let id_suffix = c (s "+-" `rep` 1) `seq` wsq

(* TODO: ideally this would recognize an id_basic, check
 * if the capture is equal to str, and cancel the capture *)
(* the problem is idk how to cancel the capture *)
let keyword str = p str `seq` neg alnum_ext `seq` wsq
let keysym str = p str `seq` wsq
let cap pat res = pat `seq` cc res
let excl pat unless = neg unless `seq` pat
let comma_sep pat = collect_list (sepseq pat (keysym ","))
let opt pat = pat `act` Some `alt` cc None

(* x *MUST* capture something, even if it's something dumb like cc () *)
(* otherwise types don't line up *)
(* also amulet doesn't like parser1 'a in the type ascription, who knows why *)
let id_suffix_complex (x: parser (parservals 'a emptyparservals)) =
  (* TODO: cancel id_basic capture to use it here *)
  let variant1 = collect_tuple (c (p "." `seq` alpha_lower `seq` (alnum_ext `rep` 0)) `cap` [])
  let variant2 = collect_tuple (c (p ".(") `seq` wsq `seq` collect_list (collect_tuple (x `seq` c (p ")"))))
  in variant1 `alt` variant2 `seq` wsq

(* TODO: bools are just type bool = True | False *)
(* this means conditionals are just pattern matches on bool *)
(* (not strictly true, Open has Ideas) *)
let parse_bool = function | "true" -> Some true | "false" -> Some false | _ -> None
let term_bool: parser1 pterm = id_basic `actx` parse_bool `act` term_bool_fix
let term_identifier_shy: parser1 pterm = id_basic_shy `act` identifier_basic_fix
let term_identifier: parser1 pterm = id_basic `act` identifier_basic_fix
let term_constructor: parser1 pterm = id_constructor `act` identifier_constructor_fix
let infix_op: parser1 pterm = id_infix `act` identifier_infix_fix
let prefix_op: parser1 pterm = id_prefix `act` identifier_prefix_fix
let suffix_op: parser1 pterm = id_suffix `act` identifier_suffix_fix
(* https://www.youtube.com/watch?v=T-BoDW1_9P4&t=11m3s *)
let unzip xs = foldr (fun (l, r) (ls, rs) -> (l::ls, r::rs)) ([], []) xs
let fixsuffix f (h, rs) = let (args, parts) = unzip rs in (f (h, parts), args)
let suffix_complex_op x = id_suffix_complex x `act` fixsuffix identifier_suffix_complex_fix

(* Attempt to control variables usage in compiled lua *)
(* TODO: move this even higher and figure out the type errors *)
let syntax () =

  let term_ref: parser1 pterm = v "term"
  let term_paren_shy = keysym "(" `seq` term_ref `seq` p ")"
  let term_paren = term_paren_shy `seq` wsq
  let term_definition = keysym "=" `seq` term_ref

  let pattern_sequence () =
    let pattern_binding = id_basic `act` pattern_binding_basic_fix

    in (
      (*`alt`*) pattern_binding
    )
  let pattern_sequence = pattern_sequence ()

  let abstraction_body =
    let args = keysym "(" `seq` comma_sep pattern_sequence `seq` keysym ")"
    in collect_tuple (args `seq` term_definition) `act` term_abstraction_fix

  let define_sequence () =
    let define_simple =
      let pat = pattern_sequence
      in collect_tuple (pat `seq` term_definition)

    let define_function =
      let left = id_basic `act` pattern_binding_basic_fix
      in collect_tuple (left `seq` abstraction_body)

    let define_infix =
      let left = pattern_sequence
      let op = id_infix `act` pattern_binding_infix_fix
      let right = pattern_sequence
      let fixup (l, op, r, body) = (op, term_abstraction_fix ([l, r], body))
      in collect_tuple (left `seq` op `seq` right `seq` term_definition) `act` fixup

    let define_prefix =
      let op = id_prefix `act` pattern_binding_prefix_fix
      let right = pattern_sequence
      let fixup (op, r, body) = (op, term_abstraction_fix ([r], body))
      in collect_tuple (op `seq` right `seq` term_definition) `act` fixup

    let define_suffix =
      let left = pattern_sequence
      let op = id_suffix `act` pattern_binding_suffix_fix
      let fixup (l, op, body) = (op, term_abstraction_fix ([l], body))
      in collect_tuple (left `seq` op `seq` term_definition) `act` fixup

    let define_suffix_complex =
      let left = pattern_sequence
      let rights = id_suffix_complex pattern_sequence `act` fixsuffix pattern_binding_suffix_complex_fix
      let fixup (l, (op, rs), body) = (op, term_abstraction_fix (l::rs, body))
      in collect_tuple (left `seq` rights `seq` term_definition) `act` fixup

    in (
            define_simple
      `alt` define_function
      `alt` define_infix
      `alt` define_prefix
      `alt` define_suffix
      `alt` define_suffix_complex
    )
  let define_sequence = define_sequence ()

  let term_key () =
    let term_string =
      let chars_regular = c (star `excl` s "\n\r\"$\\")
      let chars_multiline = c (star `excl` (s "$\\" `alt` p "''"))
      let chars_triple = star `excl` p "'''"
      let escape_chars =
        foldl1 alt ((fun (s, r) -> lit s r) <$> [
          ("\\t", "\t"),
          ("\\n", "\n"),
          ("\\r", "\r"),
          ("\\\"", "\""),
          ("\\$", "$"),
          ("\\\\", "\\")
        ])
      (* This looks really weird because escape_chars provides a custom capture,
       * which can't be handled with a simple c (bad things happen) *)
      let string_body chars =
        let string_frag = collect_list (chars `alt` escape_chars `rep` 0) `act` foldl (^) ""
        let splice_frag = p "$" `seq` (term_identifier_shy `alt` term_paren_shy)
        let splice_list = collect_list (collect_tuple (splice_frag `seq` string_frag) `rep` 0)
        in collect_tuple (string_frag `seq` splice_list)
      let string_regular = p "\"" `seq` string_body chars_regular `seq` keysym "\""
      (* TODO: magic whitespace sensitivity? *)
      let string_multiline = p "''" `seq` string_body chars_multiline `seq` keysym "''"
      let string_triple_body = collect_tuple (c (chars_triple `rep` 0) `cap` [])
      let string_triple = p "'''" `seq` string_triple_body `seq` keysym "'''"
      in string_regular `alt` string_multiline `alt` string_triple `act` term_string_fix

    let term_list =
      let list_elements = comma_sep term_ref
      (* TODO: requiring a comma before the tail is probably silly *)
      (* but doing [...tail] seems silly too *)
      let list_tail = keysym "," `seq` keysym "..." `seq` term_ref
      let list_body = collect_tuple (list_elements `seq` opt list_tail)
      in keysym "[" `seq` list_body `seq` keysym "]" `act` term_list_fix

    let term_record =
      let unrepresentable (l, r) = match unfix l with
        | PatternBinding id -> Some (term_identifier_fix id, r)
        | _ -> None
      let record_binding1 = define_sequence `actx` unrepresentable
      let record_binding2 = collect_tuple (term_string `alt` term_paren `seq` term_definition)
      let record_binding = record_binding1 `alt` record_binding2
      in keysym "{" `seq` comma_sep record_binding `seq` keysym "}" `act` term_record_fix

    let term_abstraction = keyword "fun" `seq` abstraction_body

    let let_body =
      let fixup ((a, b), c) = (a, b, c)
      in collect_tuple (define_sequence `seq` keyword "in" `seq` term_ref) `act` fixup
    let term_let = keyword "let" `seq` let_body `act` term_let_fix
    let term_let_rec = keyword "let" `seq` keyword "rec" `seq` let_body `act` term_let_rec_fix

    let term_hole = p "$?" `seq` opt (id_basic `act` IdentifierBasic) `act` term_hole_fix

    in (
            term_paren
      `alt` term_bool
      `alt` term_string
      `alt` term_list
      `alt` term_record
      `alt` term_abstraction
      `alt` term_let
      `alt` term_let_rec
      `alt` term_hole
      `alt` term_identifier
      `alt` term_constructor
    )
  let term_key = term_key ()

  (* Left-recursive parsers are hard >< *)
  (* The basic idea here is we're isolating each left-recursive parser
   * in a way where each parser can collect itself in a loop, and
   * higher-precedence parsers, but requires parens for lower-
   * -precedence parsers *)
  (* Incidentally, these are all function application of some kind. Go figure. *)

  let term () =
    let partial_argument t = (t `act` Some) `alt` (keysym "_" `cap` None)

    let application =
      (* cursed idea: _(arg) meta-partial function application *)
      let left = term_key
      let paren_app = keysym "(" `seq` comma_sep (partial_argument term_ref) `seq` keysym ")"
      let paren_rep = collect_list (paren_app `rep` 0)
      let application_ops = collect_tuple (left `seq` paren_rep)
      let fold (l, ps) = foldl (fun l p -> term_application_fix (l, p)) l ps
      in application_ops `act` fold

    let suffix_app =
      let left = partial_argument application
      (* hacky workaround to make sure suffix ops don't eat infix ops *)
      (* TODO: possibly causes a lot of backtracking, test this! *)
      let suffix1 = suffix_op `seq` neg term_ref `act` (fun x -> (x, []))
      let suffix2 = suffix_complex_op (partial_argument term_ref)
      let suffix = suffix1 `alt` suffix2
      let suffix_rep = collect_list (suffix `rep` 0)
      let suffix_ops = collect_tuple (left `seq` suffix_rep)
      let fold (l, ops) = foldl (fun l (op, args) -> Some (term_application_fix (op, l::args))) l ops
      in suffix_ops `actx` fold

    (* prefix ops aren't left recursive, but putting
     * them here is necessary for correct precedence *)
    let prefix_app =
      let right = partial_argument suffix_app
      let prefix_rep = collect_list (prefix_op `rep` 0)
      let prefix_ops = collect_tuple (prefix_rep `seq` right)
      let fold (ops, r) = foldr (fun op r -> Some (term_application_fix (op, [r]))) r ops
      in prefix_ops `actx` fold

    let infix_app =
      let left = partial_argument prefix_app
      let right = collect_tuple (infix_op `seq` left)
      let right_rep = collect_list (right `rep` 0)
      let infix_ops = collect_tuple (left `seq` right_rep)
      let fold (l, rs) = foldl (fun l (op, r) -> Some (term_application_fix (op, [l, r]))) l rs
      in infix_ops `actx` fold

    in infix_app
  let term = term ()

  in grammar { term = term } term_ref

let syntax = syntax ()
