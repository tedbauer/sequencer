.PHONY: clean  # Phony target for cleanup

main.js: elm/Main.elm
	elm make elm/Main.elm --output=$@

build: main.js
	npx webpack

dev: build
	open index.html

clean:
	rm -rf dist
	rm -rf node_modules
	rm main.js
