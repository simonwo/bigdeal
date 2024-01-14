// The MIT License (MIT)
//
// Copyright (c) 2023 Simon Worthington
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

import { Grammar } from 'ohm-js';

// This implementation of an indentation finder is designed to extend the
// indentation finding built into Ohm.js with the extra capability of ignoring
// runs of successive newlines with no other content ("gaps").
//
// For example, the below snippet:
//
//     key:
//       key:
//         key: value
//
//         key: value
//
//
//       key: value
//
// contains two gaps and would actually be seen by the parser as:
//
//     key:
//       key:
//         key: value
//         key: value
//       key: value
//
// This allows the indentation-sensitive parser to correctly issue indents and
// dedents at the right locations. Otherwise, it treats the gap as a full set of
// dedents and then the subsequent return to the indented content as a single
// indent, which means it is impossible to know to what level the content after
// the gap is indented.
//
// We do this by first identifying and removing all of the gaps in the input
// string, making a note of where they were removed. We then run the amended
// string through the normal indentation detector to retrieve the indents. Then,
// we compare the indent positions with the record of removed positions, and add
// to the indent positions any length that we removed. This gives us a set of
// indent positions that work correctly for the original string.
export function findIndentationIgnoringGaps(input: string): IndentPositions {
    type ReplacePositions = IndentPositions
    const replacements: ReplacePositions = {}

    // Find all of the gaps and record where the gap starts and much space we
    // are going to remove, leaving just the single newline.
    const GAP = /\n([ \t]*\n)+/mg
    let match: ReturnType<typeof GAP.exec>
    while ((match = GAP.exec(input)) != null) {
        const start = match.index
        const length = GAP.lastIndex - start - 1
        replacements[start] = length
    }

    // Run the regex again, this time replacing the gaps with just a newline.
    const gapless = input.replace(GAP, "\n")

    // Pass the simplified source to the indentation finder.
    const gaplessIndents = findIndentation(gapless)

    // Update the gapless indent positions with extra removed space
    const replacePositions = Object.keys(replacements).map(v => parseInt(v)).sort((a, b) => a - b)
    const indentPositions = Object.keys(gaplessIndents).map(v => parseInt(v)).sort((a, b) => a - b)
    const newIndents: IndentPositions = {}
    let currentReplacementIndex = 0
    let currentReplacementSum = replacements[replacePositions[currentReplacementIndex]]
    for (let pos of indentPositions) {
        // The current pos will be an indent position in the gapless source
        // To find any relevant replacements, we need to move that position
        // forward to what it will be in the original source - hence continued
        // use of pos + currentReplacementSum

        // Update the sum to take account of any replacements we passed since
        // the last indent.
        while (replacePositions.length > currentReplacementIndex && pos + currentReplacementSum >= replacePositions[currentReplacementIndex+1]) {
            currentReplacementIndex++
            currentReplacementSum += replacements[replacePositions[currentReplacementIndex]]
        }

        // Now set the new position of the indent to the gapless position plus
        // the sum of any replacements passed so far.
        let newPos = pos
        if (pos+currentReplacementSum >= replacePositions[currentReplacementIndex]) {
            newPos = pos + currentReplacementSum
        }

        newIndents[newPos] = gaplessIndents[pos]
    }

    return newIndents
}

// The below function is copied from ohm-js/src/findIndentation.js and is used
// under the MIT License terms above.
function findIndentation(input: string) {
    let pos = 0;
    const stack = [0];
    const topOfStack = () => stack[stack.length - 1];

    const result: {[key: string]: number} = {};

    const regex = /( *).*(?:$|\r?\n|\r)/g;
    let match;
    while ((match = regex.exec(input)) != null) {
      const [line, indent] = match;

      // The last match will always have length 0. In every other case, some
      // characters will be matched (possibly only the end of line chars).
      if (line.length === 0) break;

      const indentSize = indent.length;
      const prevSize = topOfStack();

      const indentPos = pos + indentSize;

      if (indentSize > prevSize) {
        // Indent -- always only 1.
        stack.push(indentSize);
        result[indentPos] = 1;
      } else if (indentSize < prevSize) {
        // Dedent -- can be multiple levels.
        const prevLength = stack.length;
        while (topOfStack() !== indentSize) {
          stack.pop();
        }
        result[indentPos] = -1 * (prevLength - stack.length);
      }
      pos += line.length;
    }
    // Ensure that there is a matching DEDENT for every remaining INDENT.
    if (stack.length > 1) {
      result[pos] = 1 - stack.length;
    }
    return result;
  }

type IndentPositions = ReturnType<typeof findIndentation>

// Here we replace the indent positions populated by the previous simple finder
// with the ones from the new gapless finder.
export function makeGapSkippingGrammar(grammar: Grammar) {
    // @ts-ignore
    const old = grammar._matchStateInitializer

    Object.assign(grammar, {
        _matchStateInitializer(state: unknown) {
            old(state)
            // @ts-ignore
            state.userData = findIndentationIgnoringGaps(state.input)
        }
    })

    return grammar
}
