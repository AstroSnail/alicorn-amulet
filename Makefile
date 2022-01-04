syntax.lua: syntax.ml ast-temp.ml hylo-temp.ml
	amc compile syntax.ml -o syntax.lua

ast-temp.ml: ast.ml batteries.lua
	lua batteries.lua <ast.ml >ast-temp.ml

hylo-temp.ml: hylo.ml batteries.lua
	lua batteries.lua <hylo.ml >hylo-temp.ml

batteries.lua: batteries.ml
	amc compile batteries.ml -o batteries.lua
