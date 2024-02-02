import * as fs from 'fs';
import * as path from 'path';
import test from 'ava';
import { dfsFromNode } from 'graphology-traversal';
import parse from '../build/main.js';

import { fileURLToPath } from 'url'
const __filename = fileURLToPath(import.meta.url)
const __dirname = path.dirname(__filename)

const examples = [
    { name: "field_from_source", paths: { "y": "a" } },
    { name: "projection", paths: { "y": "a" } },
    { name: "sibling", paths: { "y": "a" } },
    { name: "multiple_sources_up", paths: { "y": "a" } },
    { name: "item_name", paths: { "c": "a" } },
    { name: "item_name_multi", paths: { "y": "c", "c": "b" } },
    { name: "all", paths: { "c": "a", "b": "w", "x": "w" } },
]

examples.forEach(example => {
    const filename = path.resolve(__dirname, `scoping_${example.name}.bigdeal`)
    test(`can resolve scopes for ${path.basename(filename)}`, (assert) => {
        const source = fs.readFileSync(filename).toString()
        const result = parse(source)
        assert.truthy(result)

        const unresolvedIdentifiers = result.filterNodes((_, attr) => Array.isArray(attr))
            .map(node => result.getNodeAttributes(node))
        assert.is(unresolvedIdentifiers.length, 0, `${unresolvedIdentifiers} were unresolved`)

        for (const [defnName, targetName] of Object.entries(example.paths)) {
            const defn = result.findNode((_, attr) => attr.name == defnName && "count" in attr)
            assert.truthy(defn, `couldn't find defn "${defnName}"`)

            const target = result.findNode((_, attr) => attr.name == targetName && "count" in attr)
            assert.truthy(target, `couldn't find target "${targetName}"`)

            let found = false
            dfsFromNode(result, defn, (node) => { found ||= node == target })
            assert.true(found, `couldn't find path from ${defnName} to ${targetName}`)
        }
    })
})

const failureExamples = [
    { name: "item_name_no_implicit", paths: {"b": "a"}},
]

failureExamples.forEach(example => {
    const filename = path.resolve(__dirname, `scoping_${example.name}.bigdeal`)
    test(`cannot resolve faulty scopes for ${path.basename(filename)}`, (assert) => {
        const source = fs.readFileSync(filename).toString()
        const result = parse(source)
        assert.truthy(result) // should still return something?

        for (const [defnName, targetName] of Object.entries(example.paths)) {
            const defn = result.findNode((_, attr) => attr.name == defnName && "count" in attr)
            assert.truthy(defn, `couldn't find defn "${defnName}"`)

            const target = result.findNode((_, attr) => attr.name == targetName && "count" in attr)
            assert.truthy(target, `couldn't find target "${targetName}"`)

            let found = false
            dfsFromNode(result, defn, (node) => { found ||= node == target })
            assert.false(found, `should not have found path from ${defnName} to ${targetName}`)
        }
    })
})
