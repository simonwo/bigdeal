import * as fs from 'fs';
import * as path from 'path';
import test from 'ava';
import { dfsFromNode } from 'graphology-traversal';
import { edgeDFS } from '../build/edgeTraversal.js';
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
    { name: "aggregates", paths: { "z": "y", "a": "y" } },
    // `d` is in the filter of `e`, so we should be able to find it by only
    // following src or filter edges
    { name: "sibling_aggs", paths: { "e": "d", "b": "c" }, via: ["src", "filter"] },
    { name: "agg_inheritance", paths: { "z": "c" }, via: ["src"] },
    { name: "agg_ctx_edge", paths: { "is_happy": "happy" }, via: ["src"] }
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
            edgeDFS(result, defn, (node, attr, prev) => {
                found ||= (node === target)
                const edges = result.filterEdges(node, (edge, attr) => {
                    // Only follow edges in the forward direction – no going upwards
                    const forward = result.source(edge) === node
                    if (!forward) return false

                    // If the example contains "via" edge kinds, make sure we are only following those edge kinds.
                    if (example.via !== undefined) {
                        return example.via.includes(attr.kind)
                    } else return true
                })
                return edges.map(edge => {
                    return { edge: edge, forward: result.source(edge) === node }
                })
            })
            assert.true(found, `couldn't find path from ${defnName} to ${targetName}`)
        }
    })
})

const failureExamples = [
    { name: "item_name_no_implicit", paths: { "b": ["a"] }, unresolved: [["a"]] },
    { name: "no_implicit_aggregate", paths: {}, unresolved: [["x"]] },
    { name: "no_self_inheritance", paths: {}, unresolved: [["a", "y"]] },
]

failureExamples.forEach(example => {
    const filename = path.resolve(__dirname, `scoping_${example.name}.bigdeal`)
    test(`cannot resolve faulty scopes for ${path.basename(filename)}`, (assert) => {
        const source = fs.readFileSync(filename).toString()
        const result = parse(source)
        assert.truthy(result) // should still return something?

        const unresolvedIdentifiers = result.filterNodes((_, attr) => Array.isArray(attr))
            .map(node => result.getNodeAttributes(node))
        for (const name of example.unresolved) {
            assert.truthy(
                unresolvedIdentifiers.find(id => name.every((v, i) => id[i] === v)),
                `expecting ${name} to be unresolved, only found [${unresolvedIdentifiers}]`,
            )
        }

        for (const [defnName, targetName] of Object.entries(example.paths)) {
            const defn = result.findNode((_, attr) => attr.name == defnName && "count" in attr)
            assert.truthy(defn, `couldn't find defn "${defnName}"`)

            const target = result.findNode((_, attr) => targetName.every((v, i) => attr.name[i] === v) && "count" in attr)
            assert.truthy(target, `couldn't find target "${targetName}"`)

            let found = false
            dfsFromNode(result, defn, (node) => { found ||= node == target })
            assert.false(found, `should not have found path from ${defnName} to ${targetName}`)
        }
    })
})
