TSCFLAGS = --noImplicitAny --target ES5

all: evreact.js test

clean:
	-rm *.js

test: test.js
	@if node test.js ; then echo "Test passed" ; else echo "Test failed" ; fi

evreact.js: evreact.ts
	tsc $(TSCFLAGS) --out $@ $^

test.js: test.ts
	tsc $(TSCFLAGS) --out $@ $^

.PHONY: all clean test
