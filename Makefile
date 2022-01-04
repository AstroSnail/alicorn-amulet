syntax.lua: syntax.ml
	amc compile syntax.ml -o syntax.lua

batteries.lua: batteries.ml
	amc compile batteries.ml -o batteries.lua

clean:
	rm *.lua

PHONY: clean
