parsing.lua: parsing-temp.ml
	amc compile parsing-temp.ml -o parsing.lua

parsing-temp.ml: parsing.ml batteries.lua
	lua batteries.lua <parsing.ml >parsing-temp.ml

batteries.lua: batteries.ml
	amc compile batteries.ml -o batteries.lua
