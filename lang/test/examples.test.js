import * as fs from 'fs';
import * as path from 'path';
import test from 'ava';
import { Grammar, parse } from '../build/main.js';

import { fileURLToPath } from 'url'
const __filename = fileURLToPath(import.meta.url)
const __dirname = path.dirname(__filename)

const examples = [
    'cards',
    'tic-tac-toe',
]

examples.forEach(example => {
    const filename = path.resolve(__dirname, "../../examples/", example + '.bigdeal')
    test(`can parse the ${example} example`, (assert) => {
        const source = fs.readFileSync(filename).toString()
        const result = Grammar.match(source)
        assert.truthy(result)
        assert.truthy(result.succeeded)
    })

    test(`can resolve scopes for the ${example} example`, (assert) => {
        const source = fs.readFileSync(filename).toString()
        const result = parse(source)
        assert.truthy(result)
    })
})
