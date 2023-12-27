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

import { DFSStack } from 'graphology-indices';
import { Attributes, AbstractGraph } from 'graphology-types';

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

export function edgeDFS<
    NodeAttributes extends Attributes,
    EdgeAttributes extends Attributes,
    GraphAttributes extends Attributes,
>(
    graph: AbstractGraph<NodeAttributes, EdgeAttributes, GraphAttributes>,
    startingNode: string,
    visitNode: EdgeSorter<NodeAttributes, EdgeAttributes>,
) {
    const stack = new DFSStack<EdgeTraversalRecord<EdgeAttributes>, NodeAttributes>(graph);
    stack.pushWith(startingNode, { node: startingNode })

    while (stack.size !== 0) {
        const record = stack.pop()
        if (record === undefined) return;

        const edges = visitNode(record.node, graph.getNodeAttributes(record.node), record.prev)
        const records = edges.map<EdgeTraversalRecord<EdgeAttributes>>(edge => {
            const forward = graph.source(edge) == record.node
            return {
                prev: {
                    edge: edge,
                    attr: graph.getEdgeAttributes(edge),
                    forward: forward,
                },
                node: graph.opposite(record.node, edge),
            }
        })

        records.reverse().forEach(record => stack.pushWith(record.node, record))
    }

    return
}
