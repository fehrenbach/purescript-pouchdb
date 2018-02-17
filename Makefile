.PHONY: test

SOURCES = $(shell find src/)
TEST_SOURCES = $(shell find test/)

output/Test.Main/index.js: $(SOURCES) $(TEST_SOURCES)
	purs compile 'bower_components/*/src/**/*.purs' 'src/**/*.purs' 'test/**/*.purs'

test: output/Test.Main/index.js
	node -e 'global.PouchDB = require("pouchdb");require("./output/Test.Main").main()'
