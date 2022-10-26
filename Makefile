.PHONY: clean

public/elm.js: src/Main.elm
	elm make src/Main.elm --output=public/elm.js

clean:
	rm public/elm.js
