export npm_config_loglevel=silent

include excellent.mk
include lang/include.mk

SOURCES := $(wildcard *.ts)
OBJECTS := $(patsubst %,build/%,$(shell cat package.json | jq -rcM '.main'))

${OBJECTS}: ${SOURCES} ${lang_OBJECTS} package.json
	npm run build

.PHONY: build
build: ${OBJECTS}

# Generation of graph diagrams from bigdeal files
%.dot: %.bigdeal ${OBJECTS}
	npm run bigdeal graph $< > $@

%.png: %.dot
	dot -Tpng -o $@ < $<

ALL_BIGDEALS := $(shell find . -name '*.bigdeal')
ALL_DOTS := $(patsubst %.bigdeal,%.dot,${ALL_BIGDEALS})
ALL_PNGS := $(patsubst %.dot,%.png,${ALL_DOTS})

.PHONY: graphs
graphs: ${ALL_PNGS}

.PHONY: clean
clean:
	${RM} -r build/
	${RM} ${ALL_PNGS}
	${RM} ${ALL_DOTS}

.PHONY: mrproper
mrproper: clean
	${RM} -r node_modules/
