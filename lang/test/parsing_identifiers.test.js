import * as fs from 'fs';
import * as path from 'path';
import test from 'ava';
import { dfsFromNode } from 'graphology-traversal';
import parse from '../build/main.js';

import { fileURLToPath } from 'url'
const __filename = fileURLToPath(import.meta.url)
const __dirname = path.dirname(__filename)

const filename = path.resolve(__dirname, `parsing_identifiers.bigdeal`)
const source = fs.readFileSync(filename).toString()
const result = parse(source)

const expected = {
    "x": ["z"],
    "a": ["c", "b"],
    "d": ["g", "f", "e"],
    "w": ["h", "g", "f", "e"]
}

Object.keys(expected).forEach(name => {
    const path = expected[name]
    test(`can parse "${name}" into ${path}`, (assert) => {
        const defn = result.findNode((_, attr) => attr.name == name)

        var found
        dfsFromNode(result, defn, node => {found = result.getNodeAttributes(node)})
        assert.deepEqual(found, path)
    })
})
