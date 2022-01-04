syntax.lua: syntax.ml ast-temp.ml
	amc compile syntax.ml -o syntax.lua

ast-temp.ml: ast.ml batteries.lua
	lua batteries.lua <ast.ml >ast-temp.ml

batteries.lua: batteries.ml
	amc compile batteries.ml -o batteries.lua
