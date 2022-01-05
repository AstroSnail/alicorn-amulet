syntax-tests.lua: syntax-tests.ml syntax.ml # add other deps?
	amc compile syntax-tests.ml -o syntax-tests.lua

batteries.lua: batteries.ml
	amc compile batteries.ml -o batteries.lua

clean:
	rm *.lua

PHONY: clean
