open import "prelude.ml"
open import "./parsing/lpeg.ml"

type types_type = NoTypes | WithTypes of list string

let concat_string ss = foldl (^) "" ss
let concat_string_with sep = function
  | Nil -> ""
  | Cons (v, vs) -> v ^ concat_string ((sep ^) <$> vs)
let figure_those_out b ts = concat_string_with " * " ((b ^ " " ^) <$> ts)
let figure_it_out b type_name = function
  | NoTypes -> b ^ " " ^ type_name
  | WithTypes ts -> figure_those_out b ts ^ " => " ^ b ^ " (" ^ type_name ^ " " ^ concat_string_with " " ts ^ ")"
let tuplify args = "(" ^ concat_string_with ", " args ^ ")"
let figure_show_alts_out alts =
  foldl (fun a (alt, args) -> match args with
    | NoTypes -> a ^ "    | " ^ alt ^ " -> \"" ^ alt ^ "\"\n"
    | WithTypes ts ->
      let ta = tuplify ts
      in a ^ "    | " ^ alt ^ " " ^ ta ^ " -> \"" ^ alt ^ "\" ^ show " ^ ta ^ "\n"
  ) "" alts
let figure_eq_alts_out alts =
  foldl (fun a (alt, args) -> match args with
    | NoTypes -> a ^ "    | " ^ alt ^ ", " ^ alt ^ " -> true\n"
    | WithTypes ts ->
      let ta = tuplify ts
      let ta2 = tuplify ((^ "2") <$> ts)
      in a ^ "    | " ^ alt ^ " " ^ ta ^ ", " ^ alt ^ " " ^ ta2 ^ " -> " ^ ta ^ " == " ^ ta2 ^ "\n"
  ) "" alts
let batteries_inserter (type_name, types, alts) =
  (* cursed whitespace sensitivity *)
  "instance " ^ figure_it_out "show" type_name types ^ "\n" ^
   "  let show = function\n" ^
    figure_show_alts_out alts ^
     "instance " ^ figure_it_out "eq" type_name types ^ "\n" ^
      "  let x == y = match x, y with\n" ^
       figure_eq_alts_out alts ^
        "    | _ -> false\n"

let excl pat unless = neg unless `seq` pat
let alnum = r "09" `alt` r "AZ" `alt` r "az" `alt` s "'_"
let alnump = alnum `rep` 1
let nl = p "\n"
let nl_after pat = pat `seq` nl
let not_nl = star `excl` nl
let not_nls = not_nl `rep` 0
let w = s "\t\n\r ,"
let ws = w `rep` 0
let wp = w `rep` 1
let ws_after pat = pat `seq` ws
let wp_after pat = pat `seq` wp
let keysym str = ws_after (p str)
let keyword str = wp_after (p str)
let endword str = nl_after (p str)

let types_converter ts = WithTypes ts
let types_parser = keysym "(" `seq` collect_list (ws_after (c alnump) `rep` 1) `seq` keysym ")" `act` types_converter `alt` cc NoTypes

let alts_parser = collect_list (ws_after (collect_tuple (c alnump `seq` types_parser)) `rep` 1)

let battery_spec_parser = keyword "#DERIVE_BATTERIES" `seq` collect_tuple (wp_after (c alnump) `seq` types_parser `seq` alts_parser) `seq` endword "#END_DERIVE_BATTERIES"
let batteries_included_parser = battery_spec_parser `act` batteries_inserter

let uninteresting_line_parser = c (nl_after not_nls)

let line_parser = batteries_included_parser `alt` uninteresting_line_parser
let lines_parser = collect_list (line_parser `rep` 0)

let Some input = Io.read_all (Io.file_of Io.standard_in)
let Some out_lines = parse lines_parser input
let output = concat_string out_lines
let () = put_bytes output
