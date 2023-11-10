# blogex
# Makefile

all:
	deno compile --output processor/blogexprocess --allow-read processor/blogexprocess.js
	cp processor/blogexprocess $$(stack path --local-bin)
	stack install

