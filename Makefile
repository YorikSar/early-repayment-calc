all: calc.js

.PHONY: calc.js
calc.js:
	elm make src/Main.elm --output calc.js

.PHONY: test
test:
	elm-test
