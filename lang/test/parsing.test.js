import * as fs from 'fs';
import * as path from 'path';
import test from 'ava';
import parse from '../build/main.js';

import { fileURLToPath } from 'url'
const __filename = fileURLToPath(import.meta.url)
const __dirname = path.dirname(__filename)

const examples = [
    "block",
    "gaps",
    "aggregates",
]

examples.forEach(example => {
    const filename = path.resolve(__dirname, `parsing_${example}.bigdeal`)
    test(`can parse ${path.basename(filename)}`, (assert) => {
        const source = fs.readFileSync(filename).toString()
        const result = parse(source)
        assert.truthy(result)
    })
})
