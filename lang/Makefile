include ../excellent.mk

CONFIGS := package.json tsconfig.json webpack.config.cjs
SOURCES := $(wildcard src/*) $(wildcard lib/*.bigdeal)
OBJECTS := $(patsubst %, ./%,$(shell cat package.json | jq -rcM ".exports[]"))

${OBJECTS}: ${SOURCES} ${CONFIGS}
	(pushd ./; npm run build; popd) && touch $@

.PHONY: build
build: ${OBJECTS}

.PHONY: test
test: ${OBJECTS}
	cd ./; npx ava

.PHONY: clean
clean:
	${RM} -r build/

.PHONY: mrproper
mrproper:
	${RM} -r node_modules/
