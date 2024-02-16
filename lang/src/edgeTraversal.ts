// The MIT License (MIT)

// Copyright (c) 2023 Simon Worthington

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

// This edge-aware DFS was written for the bigdeal project based on the DFS
// implementation found in graphology-traversal at
// https://github.com/graphology/graphology/blob/master/src/traversal/dfs.js,
// written by and copyright Guillaume Plique, which is used as reference under
// the permissive MIT license shown above. This file is also released under the
// MIT license.

import { Attributes, AbstractGraph, EdgePredicate } from 'graphology-types';

export type EdgeTraversal<EdgeAttributes> = {
    edge: string
    attr: EdgeAttributes
    forward: boolean
}

type EdgeTraversalRecord<EdgeAttributes> = {
    prev?: EdgeTraversal<EdgeAttributes>
    node: string
}

type EdgeSorter<NodeAttributes, EdgeAttributes> = (
    node: string,
    attr: NodeAttributes,
    prev?: EdgeTraversal<EdgeAttributes>,
) => string[]

class edgeDFSStack<N extends Attributes, E extends Attributes, G extends Attributes, T> {
    graph: AbstractGraph<N, E, G>;
    stack: T[];
    seen: Set<string>;
    size: number;

    constructor(graph: AbstractGraph<N, E, G>) {
        this.graph = graph
        this.stack = new Array(this.graph.edges().length)
        this.seen = new Set()
        this.size = 0
    }

    hasAlreadySeenEverything(): boolean {
        return this.seen.size === this.graph.edges().length;
    };

    countUnseenEdges(): number {
        return this.graph.edges().length - this.seen.size;
    };

    forEachEdgeYetUnseen(callback: EdgePredicate<N>) {
        const seen = this.seen;
        const graph = this.graph;

        graph.someEdge(function (edge, attr) {
            // Useful early exit for connected graphs
            if (seen.size === graph.edges().length) return true; // break

            // Node already seen?
            if (seen.has(edge)) return false; // continue

            const source = graph.source(edge)
            const target = graph.target(edge)
            const sourceAttr = graph.getNodeAttributes(source)
            const targetAttr = graph.getNodeAttributes(target)
            const shouldBreak = callback(edge, attr, source, target, sourceAttr, targetAttr, graph.isDirected(edge));

            if (shouldBreak) return true;

            return false;
        });
    };

    has(edge: string): boolean {
        return this.seen.has(edge);
    };

    pushWith(edge: string, item: T) {
        const seenSizeBefore = this.seen.size;

        this.seen.add(edge);

        // If node was already seen
        if (seenSizeBefore === this.seen.size) return false;

        this.stack[this.size++] = item;

        return true;
    };

    pop(): T | undefined {
        if (this.size === 0) return;

        return this.stack[--this.size];
    };
}

export function edgeDFS< N extends Attributes, E extends Attributes, G extends Attributes >(
    graph: AbstractGraph<N, E, G>,
    startingNode: string,
    visitNode: EdgeSorter<N, E>,
) {
    const stack = new edgeDFSStack<N, E, G, EdgeTraversalRecord<E>>(graph);

    const doVisit = function(record: EdgeTraversalRecord<E>) {
        const edges = visitNode(record.node, graph.getNodeAttributes(record.node), record.prev)
        edges.reverse().forEach(edge => {
            const forward = graph.source(edge) == record.node
            const newRecord = {
                prev: {
                    edge: edge,
                    attr: graph.getEdgeAttributes(edge),
                    forward: forward,
                },
                node: graph.opposite(record.node, edge),
            }
            stack.pushWith(edge, newRecord)
        })
    }

    doVisit({node: startingNode})
    while (stack.size !== 0) {
        const record = stack.pop()
        if (record === undefined) return;

        doVisit(record)
    }

    return
}
