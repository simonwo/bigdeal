import * as ohm from 'ohm-js';
import * as graphology from 'graphology';
import grammar from './bigdeal.ohm';
import { EdgeTraversal, edgeDFS } from './edgeTraversal.js';
import { makeGapSkippingGrammar } from './indentation.js';

const IndentationSensitive = makeGapSkippingGrammar(ohm.ExperimentalIndentationSensitive);

import Graph = graphology.MultiGraph;

enum LiteralType {
 TextType = "text",
 IntType = "int",
 BoolType = "bool",
}

const ManyValue = "many"

type LiteralNode = {
    type: LiteralType
    value: string | number | boolean
}

enum EdgeType {
    Source = "src",
    Definition = "defn",
    Modification = "mod",
    Count = "count",
    Filter = "filter",
    Init = "init",
    Context = "ctx",
    Aggregate = "agg",
}

type Edge = {
    kind: EdgeType
}

// A SourceTree represents a the first parsing of source code. The identifiers
// in a source tree have not yet been resolved – as such, there should not be
// loops in the data structure yet and it should be a tree. (Although, loops are
// possible and are treated as a validation error.)
type SourceTree = graphology.DirectedGraph<SourceNode, Edge, any>;

type SourceNode =
    SequenceReference |
    Definition |
    IdentifierPath |
    Expression |
    FillReference |
    Modification |
    LiteralNode;

// A SequenceReference represents a number of items from a sequence. As well as
// having a number of characterising flags, it will also have at least a
// `source` edge and possibly also a `filter` and `count` edge.
type SequenceReference = {
    name?: string[]
    every: boolean
    take: boolean
    ordered: boolean
    choose: boolean
}

function isSeqRef(node: SourceNode): node is SequenceReference {
    return "every" in node
}

type FillReference = {
    count: number
    path: IdentifierPath
}

enum Operation {
    Add = "+",
    Subtract = "-",
    Multiply = "*",
    Divide = "/",
    Any = "any",
    All = "all",
    Not = "not",
    Equal = "=",
    NotEqual = "!=",
    GreaterOrEqual = ">=",
    GreaterThan = ">",
    LessOrEqual = "<=",
    LessThan = "<",
    Contains = "in",
    Range = "to",
}

type Expression = {
    op: Operation,
}

enum ModificationType {
    Change = "change",
    AddTo = "add to",
    PrependTo = "prepend to",
}

type Modification = {
    name: string[]
    op: ModificationType
}

// An IdentifierPath represents a yet-to-be-resolved identifier. The ordering of
// the identifiers represents steps on the path to resolve it.
// E.g. `content of board` would appear as ["board", "content"]
type IdentifierPath = string[]

function isIdentifierPath(node: SourceNode): node is IdentifierPath {
    return Array.isArray(node)
}

// A Definition specifies a property of items in a parent sequence. The
// Definition specifies a `source` edge saying where the content of this
// property will come from. The Definition can also specify extensions to the
// source sequence – items in that sequence will have these extra properties
// when they are evaluated.
type Definition = {
    name: string[]
    count: number | null
}

function isDefinition(node: SourceNode): node is Definition {
    return "name" in node && "count" in node
}

// We will refer to the root of the tree using this node
const RootNodeName: string = "game"

export const Grammar = ohm.grammar(grammar, {IndentationSensitive: IndentationSensitive});
export const Semantics = Grammar.createSemantics()

// addSelf adds the current node to the graph.
Semantics.addOperation<string>("addSelf(graph, attribs)", {
    _terminal(...children)    { return this.args.graph.addNode(this.sourceNodeName, this.args.attribs) },
    _nonterminal(...children) { return this.args.graph.addNode(this.sourceNodeName, this.args.attribs) },
})

// edge returns the name of a newly added directed edge going from this node to
// the target node, tagged with the passed edge kind.
Semantics.addOperation<string>("edge(graph, targetNode, kind)", {
    _nonterminal(...children) {
        return this.args.graph.addDirectedEdge(this.sourceNodeName, this.args.targetNode, {kind: this.args.kind})
    }
})

// maybeEdge only adds the target node as the end of a directed edge if the
// target node actually exists. This is useful for situations in the language
// where something optional would result in a new edge, such as a filter.
Semantics.addOperation<string | undefined>("maybeEdge(graph, targetNode, kind)", {
    _nonterminal(...children) {
        if (this.args.targetNode !== undefined) {
            return this.edge(this.args.graph, this.args.targetNode, this.args.kind)
        } else {
            return undefined
        }
    },
})

// manyEdge adds all of the target nodes as the end of a directed edge from this
// node, using the passed edge kind.
Semantics.addOperation<string[]>("manyEdge(graph, targetNodes, kind)", {
    _nonterminal(...children) {
        const nodes: string[] = this.args.targetNodes
        return nodes.map(node => this.maybeEdge(this.args.graph, node, this.args.kind))
    },
})

// sourceNodeName is the internal name of the source tree node, used to populate
// the graph. It needs to be unique within the graph currently being generated.
Semantics.addAttribute<string>("sourceNodeName", {
    Game(_seq, _) {
        return RootNodeName
    },
    _terminal(...children) {
        const src: ohm.LineAndColumnInfo = this.source.getLineAndColumn()
        return [this.ctorName, src.lineNum, src.colNum].join("/")
    },
    _nonterminal(...children) {
        const src: ohm.LineAndColumnInfo = this.source.getLineAndColumn()
        return [this.ctorName, src.lineNum, src.colNum].join("/")
    },
})


// sourceNode will write the source tree into the passed graph object and return
// the name of the root node.
Semantics.addOperation<string>("sourceNode(graph)", {
    Game(seq, _) {
        const game: Definition = { name: [], count: null }
        this.addSelf(this.args.graph, game)
        this.manyEdge(this.args.graph, seq.children.flatMap<string>(value => value.sourceNodes(this.args.graph)), EdgeType.Definition)
        return this.sourceNodeName
    },

    FieldDefinition(int, path, _, seq, _i, init) {
        const node: Definition = { name: path.path, count: int.quantity }
        this.addSelf(this.args.graph, node)
        this.maybeEdge(this.args.graph, seq.sourceNode(this.args.graph), EdgeType.Source)
        this.maybeEdge(this.args.graph, init.sourceNode(this.args.graph), EdgeType.Init)
        return this.sourceNodeName
    },

    BlockDefinition(int, path, _colon, seq, block) {
        const node: Definition = { name: path.path, count: int.quantity }
        this.addSelf(this.args.graph, node)
        this.maybeEdge(this.args.graph, seq.sourceNode(this.args.graph), EdgeType.Source)
        block.sourceNodes(this.args.graph).forEach((n: string) => {
            const attribs = <SourceNode>this.args.graph.getNodeAttributes(n)
            var type: EdgeType = EdgeType.Definition
            if (isSeqRef(attribs)) type = EdgeType.Source
            if ("count" in attribs && attribs.count !== null) type = EdgeType.Source
            if ("op" in attribs) type = EdgeType.Modification
            this.edge(this.args.graph, n, type)
        })
        return this.sourceNodeName
    },

    Modification(operation, path, _colon, seq) {
        const node: Modification = {name: path.path, op: operation.modificationType}
        this.addSelf(this.args.graph, node)
        this.manyEdge(this.args.graph, seq.sourceNodes(this.args.graph), EdgeType.Source)
        return this.sourceNodeName
    },

    FilterExpression(_where, seq) {
        this.addSelf(this.args.graph, {op: Operation.All})
        this.manyEdge(this.args.graph, seq.sourceNodes(this.args.graph), EdgeType.Source)
        return this.sourceNodeName
    },

    CompoundExpression(_where, anyAll, _colon, block) {
        const op: Operation = anyAll.numChildren > 0 ? anyAll.operation : Operation.All
        const node: Expression = {op: op}
        this.addSelf(this.args.graph, node)
        this.manyEdge(this.args.graph, block.sourceNodes(this.args.graph), EdgeType.Source)
        return this.sourceNodeName;
    },

    BinaryExpression(atom, operator, sequenceLiteral) {
        const node: Expression = {op: operator.operation}
        this.addSelf(this.args.graph, node)
        this.edge(this.args.graph, atom.sourceNode(this.args.graph), EdgeType.Source)
        this.manyEdge(this.args.graph, sequenceLiteral.sourceNodes(this.args.graph), EdgeType.Source)
        return this.sourceNodeName
    },

    NotExpression(_not, expr) {
        this.addSelf(this.args.graph, {op: Operation.Not})
        this.edge(this.args.graph, expr.sourceNode(this.args.graph), EdgeType.Source)
        return this.sourceNodeName
    },

    FillSequence(integer, path) {
        this.addSelf(this.args.graph, {count: integer.value, path: path.path})
        return this.sourceNodeName
    },

    SequenceReference(take, choose, next, every, quantity, literal, filter) {
        const quantitySpec: QuantitySpec = quantity.quantitySpec(this.args.graph)
        const node: SequenceReference = {
            name: quantitySpec.iterationName,
            every: every.numChildren > 0,
            take: take.numChildren > 0,
            ordered: next.numChildren > 0,
            choose: choose.numChildren > 0,
        }
        this.addSelf(this.args.graph, node)
        this.maybeEdge(this.args.graph, quantitySpec.countNode, EdgeType.Count)
        this.maybeEdge(this.args.graph, filter.sourceNode(this.args.graph), EdgeType.Filter)
        this.manyEdge(this.args.graph, literal.sourceNodes(this.args.graph), EdgeType.Source)
        return this.sourceNodeName
    },

    text(literal) {
        const node: LiteralNode = {type: LiteralType.TextType, value: this.value}
        return this.addSelf(this.args.graph, node)
    },
    integer(literal) {
        const node: LiteralNode = {type: LiteralType.IntType, value: this.value}
        return this.addSelf(this.args.graph, node)
    },
    many(_literal) {
        const node: LiteralNode = {type: LiteralType.IntType, value: ManyValue}
        return this.addSelf(this.args.graph, node)
    },
    IdentifierPath(_path, _, _more) {
        const node: IdentifierPath = this.path
        return this.addSelf(this.args.graph, node)
    },
    BlockFilter(defn, compound) {
        const defnName = defn.sourceNode(this.args.graph)
        const compoundNode = compound.sourceNode(this.args.graph)
        if (compoundNode !== undefined) {
            this.args.graph.addDirectedEdge(defnName, compoundNode, {kind: EdgeType.Filter})
        }
        return defnName
    },

    Range(start, _, end) {
        const node: Expression = { op: Operation.Range }
        this.addSelf(this.args.graph, node)
        this.manyEdge(this.args.graph, [start.sourceNode(this.args.graph), end.sourceNode(this.args.graph)], EdgeType.Source)
        return this.sourceNodeName
    },

    SingleLineStatement(node, _) { return node.sourceNode(this.args.graph) },
    PureSingleLineStatement(node, _) { return node.sourceNode(this.args.graph) },

    _iter(...children) {
        if (children.length > 1) {
            throw new Error("Expecting only 0 or 1 children")
        } else if (children.length == 0) {
            return undefined
        } else {
            return children[0].sourceNode(this.args.graph)
        }
    },
})

// sourceNodes is the same as source node but for language constructs that we
// expect to return more than one node, such as a sequence.
Semantics.addOperation<string[]>("sourceNodes(graph)", {
    NotExpression(_keyword, _arg)   { return this.sourceNode(this.args.graph)} ,
    UnaryExpression(_keyword, _arg) { return this.sourceNode(this.args.graph)} ,
    Atom(x) {
        return [x.sourceNode(this.args.graph)]
    },
    Range(start, _, end) {
        return [this.sourceNode(this.args.graph)]
    },
    BlockOf(_nl, _indent, listOf, _dedent) {
        return listOf.sourceNodes(this.args.graph)
    },
    ListOf(x) {
        return x.sourceNodes(this.args.graph)
    },
    BinaryExpression(_atom, _op, _seq) {
        return [this.sourceNode(this.args.graph)]
    },
    FillSequence(_int, _path) {
        return [this.sourceNode(this.args.graph)]
    },
    NonemptyListOf(x, _, xs) {
        return [x, ...xs.children].flatMap(x => x.sourceNodes(this.args.graph))
    },
    EmptyListOf() {
        return []
    },
    _iter(...children) {
        return children.map(c => c.sourceNode(this.args.graph))
    },
    _nonterminal(...children) {
        return children.flatMap(x => x.sourceNodes(this.args.graph))
    },
    _terminal(..._) {
        return [this.sourceNode(this.args.graph)]
    },
})

type QuantitySpec = {
    countNode?: string
    iterationName: string[]
}

Semantics.addOperation<QuantitySpec>("quantitySpec(graph)", {
    QuantitySpecifier(node, name, _) {
        return {
            countNode: node.sourceNode(this.args.graph),
            iterationName: name.path,
        }
    },
    _iter(...children) {
        return children.length > 0 ? children[0].quantitySpec(this.args.graph) : {}
    },
})

type LiteralValue = string | number

// value returns a literal bigdeal value as a native JS value
Semantics.addAttribute<LiteralValue>("value", {
    integer(_): LiteralValue {
        return parseInt(this.sourceString)
    },
    dquoteText(_q1, chars, _q2): LiteralValue {
        return chars.sourceString;
    },
})

// quantity is used for optional quantity values, and will either return the
// specified quantity as a number or null to indicate that the quantity was
// unspecified
Semantics.addAttribute<number | null>("quantity", {
    _iter(...children) {
        return children.length > 0 ? children[0].quantity : null
    },
    integer(_) {
        return parseInt(this.sourceString)
    }
})

Semantics.addAttribute<Operation>("operation", {
    binaryOperator_eq: (_) => Operation.Equal,
    binaryOperator_ne: (_) => Operation.NotEqual,
    binaryOperator_ge: (_) => Operation.GreaterOrEqual,
    binaryOperator_gt: (_) => Operation.GreaterThan,
    binaryOperator_le: (_) => Operation.LessOrEqual,
    binaryOperator_lt: (_) => Operation.LessThan,
    binaryOperator_in: (_) => Operation.Contains,
    binaryOperator_add: (_) => Operation.Add,
    binaryOperator_sub: (_) => Operation.Subtract,
    binaryOperator_mul: (_) => Operation.Multiply,
    binaryOperator_div: (_) => Operation.Divide,
    booleanOperator_any: (_) => Operation.Any,
    booleanOperator_all: (_) => Operation.All,
    _iter: (...l)  => l[0].operation,
})

Semantics.addAttribute<ModificationType>("modificationType", {
    modificationKeyword_add:     (_) => ModificationType.AddTo,
    modificationKeyword_prepend: (_) => ModificationType.PrependTo,
    modificationKeyword_change:  (_) => ModificationType.Change,
})

// path returns an array of strings representing the path to some value, with
// the most significant part (i.e. highest ancestor) of the name first
// e.g. "a of b of c" will return [c, b, a]
Semantics.addAttribute<string[]>("path", {
    IdentifierPath(identifier, _, identifiers): string[] {
        return identifiers.path.concat([identifier.sourceString])
    },
    _iter(...children): string[] {
        return children.flatMap(x => x.sourceString).reverse()
    }
})

enum VisitOrder {
    DoNotVisit = -1,
    CheckFirst = 0,
    TravelDownwards = 1,
    TravelUpwards = 2,
}

function assessEdge(next: EdgeTraversal<Edge>, prev: EdgeTraversal<Edge> | undefined, startForward: boolean, namedSeqRef: boolean): VisitOrder {
    // 1. No previous edge, just started.
    if (prev === undefined) {
        if (startForward && next.forward) {
            switch (next.attr.kind) {
                case EdgeType.Definition: return VisitOrder.CheckFirst
                case EdgeType.Aggregate:  return VisitOrder.CheckFirst
                case EdgeType.Source:     return VisitOrder.TravelDownwards
                default:                  return VisitOrder.DoNotVisit
            }
        } else {
            // Want to look upwards (at reverse edges) only.
            return next.forward ? VisitOrder.DoNotVisit : VisitOrder.TravelUpwards
        }
    }

    // 2. We just came up an edge.
    if (!prev.forward) {
        // a. We can always follow an inbound edge upwards, just we want
        //    to do it last and check potential matches first.
        if (!next.forward) return VisitOrder.TravelUpwards

        // b. If we followed a src edge upwards, we can only go up.
        if (prev.attr.kind === EdgeType.Source) return VisitOrder.DoNotVisit

        // c. If we followed a defn edge upwards, we can look at sibling
        //    defn edges or go down a src edge
        // d. If we followed neither source nor defn edge upwards (so
        //    it's a count or filter), we can only look at downward
        //    source edges
        switch (next.attr.kind) {
            case EdgeType.Definition: return (prev.attr.kind === EdgeType.Definition) ? VisitOrder.CheckFirst : VisitOrder.DoNotVisit
            case EdgeType.Source:     return VisitOrder.TravelDownwards
            default:                  return VisitOrder.DoNotVisit
        }
    }

    // 3. We just came down an edge. We don't want to follow up edges
    //    from here (we assume the DFS algorithm will take care of
    //    popping us back up)
    if (!next.forward) return VisitOrder.DoNotVisit

    // a. If it was a defn or agg edge, we are checking this local definition
    //    for a match and we don't want to follow any edges from here.
    if ([EdgeType.Definition, EdgeType.Aggregate].includes(prev.attr.kind)) return VisitOrder.DoNotVisit

    // b. Presumably it was a src edge, so we want to check defn edges
    //    first or keep going down src edges.
    //
    //    i. But only if we're not on a sequence ref with a name. If we are
    //    and it matches our target, it should be handled before we get here
    //    and a new search started from this point. Else, everything below
    //    this node will not be implicitly in scope due to the name.
    switch (next.attr.kind) {
        case EdgeType.Definition: return VisitOrder.CheckFirst
        case EdgeType.Aggregate:  return VisitOrder.CheckFirst
        case EdgeType.Source:     return namedSeqRef ? VisitOrder.DoNotVisit : VisitOrder.TravelDownwards
        default:                  return VisitOrder.DoNotVisit
    }
}

type FoundNode = {
    foundPath: string[]
}

type ResolveLater = {
    afterName: string[]
    afterNode: string
}

type ResolveNext = {
    startAt: string
    lookFor: IdentifierPath
    replace: string
    progress: string[]
}

type MultipleSources = {
    names: string[]
}

type NotFound = {
    missingName: string[]
    missingNode: string
}

type SearchResult =
    FoundNode |
    ResolveLater |
    ResolveNext |
    MultipleSources |
    NotFound

function findParentDefinition(graph: SourceTree, resolve: ResolveNext): SearchResult {
    const startNode = resolve.startAt
    const lookingFor = resolve.lookFor
    const startForward = graph.getNodeAttributes(startNode) != lookingFor
    try {
        edgeDFS(
            graph,
            startNode,
            (node, attr, prev) => {
                console.error("\tVisit", node, attr, "via", prev)
                // TODO: nodes with multiple source edges
                // We are visiting a node.
                // 1. If we have found another identifier path, we need to resolve
                //    it first because what it resolves to might contain our target
                if (node !== startNode && isIdentifierPath(attr)) {
                    const first: ResolveLater = { afterName: attr, afterNode: node }
                    console.error("\tNeed to resolve", first, "first")
                    throw first
                }

                // 2. If we have found the a component of our identifier path we
                //    can remove a component of the path. This is allowed if:
                //
                //    a. We came down a defn or agg edge and we find a named
                //    definition, or
                //    b. We came down a src edge and we find a named sequence
                //    reference (loop var), or
                //    c. We came up any edge and found any named item
                const namedSeqRef = isSeqRef(attr) && attr.name !== undefined && attr.name.length > 0
                const namedDefn = isDefinition(attr) && attr.name !== undefined && attr.name.length > 0
                const backwardEdge = prev?.forward === false
                const canResolve = (namedSeqRef && (backwardEdge || prev?.attr.kind === EdgeType.Source)) ||
                    (namedDefn && (backwardEdge || (prev && [EdgeType.Definition, EdgeType.Aggregate].includes(prev?.attr.kind))))
                if (canResolve && attr.name && attr.name.length > 0) {
                    const defn = Array.from(attr.name) // copy
                    const remaining = Array.from(lookingFor) // copy
                    while (remaining.length > 0 && defn.length > 0 && remaining[0] === defn[0]) {
                        remaining.shift()
                        defn.shift()
                    }

                    if (remaining.length === 0 && defn.length === 0) {
                        const found: FoundNode = { foundPath: Array.of(node) }
                        throw found
                    } else if (defn.length === 0) {
                        const next: ResolveNext = {
                            startAt: node,
                            lookFor: remaining,
                            replace: resolve.replace,
                            progress: [node],
                        }
                        throw next
                    }
                }

                // We can return edges in the order specified by `assessEdge`.
                const edges = graph.mapEdges(node, (edge, attr, src, dst) => {
                    const record = { edge: edge, attr: attr, forward: graph.source(edge) == node }
                    console.error("\t\tAssess", src, "->", dst, record, "=", assessEdge(record, prev, startForward, namedSeqRef))
                    return record
                })
                    .filter(edge => assessEdge(edge, prev, startForward, namedSeqRef) !== VisitOrder.DoNotVisit)
                    .sort((a, b) => assessEdge(a, prev, startForward, namedSeqRef) - assessEdge(b, prev, startForward, namedSeqRef))
                    .map(edge => edge.edge)

                console.error("\tEdges to visit:", edges)
                return edges
            },
        )
    } catch (e: any) {
        return <SearchResult>e
    }

    const notFound: NotFound = {missingName: lookingFor, missingNode: resolve.replace}
    console.error("\tFailed to find", lookingFor)
    return notFound
}

function replaceNode(graph: SourceTree, oldNode: string, newNode: string) {
    graph.forEachInEdge(oldNode, (edge, attr, src, dst) => {
        console.error("Replace edge", src, "->", dst)
        graph.dropEdge(edge)
        graph.addDirectedEdge(src, newNode, attr)
    })
    graph.dropNode(oldNode)
}

function resolveIdentifier(graph: SourceTree, pathNode: string, foundNodes: string[]) {
    const lookingForPath = graph.getNodeAttributes(pathNode)

    // We have found the home for this aggregate. Now attach it with an `agg`
    // edge to the final discovered node.
    if (isDefinition(lookingForPath) && lookingForPath.name.length >= 2) {
        lookingForPath.name = [lookingForPath.name.pop() as string]
        graph.forEachInEdge(pathNode, edge => graph.dropEdge(edge))

        const target = foundNodes.pop()
        graph.addDirectedEdge(target, pathNode, {kind: EdgeType.Aggregate})
        console.error("Link aggregate", target, "->", pathNode)
        return
    }

    // If the path only has 1 component, we can just replace the identifier path
    // with the found node.
    if (isIdentifierPath(lookingForPath) && foundNodes.length === 1) {
        replaceNode(graph, pathNode, foundNodes[0])
        return
    }

    // If the target node is a definition, this is a projection or an aggregate.
    // We can replace each pair of nodes with an intermediate node that:
    // - has a `src` edge to the target or previous intermediate node
    // - has a `fn` edge to the definition that applies the projection
    let targetNode = foundNodes[foundNodes.length-1]
    console.error("Final target:", targetNode, graph.getNodeAttributes(targetNode), "Found nodes:", foundNodes)
    if (isIdentifierPath(lookingForPath) && isDefinition(graph.getNodeAttributes(targetNode))) {
        // TODO handle fields of aggregates where foundNodes.length !== lookingForPath.length
        while (foundNodes.length > 2) {
            const fnNode = foundNodes.pop()
            const fnId = lookingForPath.pop() as string
            const sourceNode = foundNodes.pop()
            const sourceId = lookingForPath.pop() as string
            console.error("Adding intermediate node for", sourceId, "with field", fnId)

            const intermediateNode = `I!${sourceNode}@${fnNode}`
            const attr: SequenceReference = {
                name: [sourceId, fnId],
                every: false,
                take: false,
                ordered: false,
                choose: false,
            }
            graph.addNode(intermediateNode, attr)

            // TODO: work out if this check is needed. Shouldn't aggs and projs both have contexts now?
            const inboundAggEdge = graph.someInEdge(fnNode, (_, attr) => { return attr.kind === EdgeType.Aggregate })
            if (inboundAggEdge) {
                graph.addDirectedEdge(intermediateNode, fnNode, {kind: EdgeType.Source})
            } else {
                graph.addDirectedEdge(intermediateNode, sourceNode, {kind: EdgeType.Source })
                graph.addDirectedEdge(intermediateNode, fnNode, {kind: EdgeType.Context })
            }

            foundNodes.push(intermediateNode)
            lookingForPath.push([sourceId, fnId].join(" of "))
        }

        // We are down to the last two components, which will be the original
        // definition node that the identifier was attached to and either the
        // final intermediate node or the found target.
        //
        // Turn the path node into a sequence reference node (retaining the
        // source information and inbound edges) and then attach edges to the
        // discovered source and context.
        if (foundNodes.length !== 2)
            throw new Error(`should have 2 nodes left but have ${foundNodes}`)
        const sourceNode = foundNodes[0]
        const defnNode = foundNodes[1]

        graph.updateNodeAttributes(pathNode, _ => {
            return {every: false, take: false, ordered: false, choose: false}
        })
        graph.addDirectedEdge(pathNode, defnNode, {kind: EdgeType.Source})
        graph.addDirectedEdge(pathNode, sourceNode, {kind: EdgeType.Context})
        return
    }

    throw new Error(`programming error: don't know how to resolve ${lookingForPath}`)
}

function resolveScopes(graph: SourceTree): NotFound[] {
    const stack: ResolveNext[] = []

    // First, resolve all the aggregates by finding the objects they should be
    // attached to. Doing it first is because subsequently resolving references
    // to those aggregates can then follow the usual process of finding the
    // first node and the resolving the second path element downwards.
    graph.forEachNode((node, attr) => {
        if (isDefinition(attr) && attr.name.length > 1) {
            const target = Array.from(attr.name)
            target.pop()

            stack.push({
                startAt: node,
                lookFor: target,
                replace: node,
                progress: [],
            })
        }
    })

    // Second, find each identifier path and resolve it.
    graph.forEachNode((node, attr) => {
        if (isIdentifierPath(attr)) {
            stack.push({
                startAt: node,
                lookFor: <IdentifierPath>attr,
                replace: node,
                progress: [],
            })
        }
    })

    const notFound: NotFound[] = []
    var todo: ResolveNext | undefined
    while (todo = stack.shift()) {
        console.error("Stack:", stack.map(rn => [rn.replace, rn.lookFor]))
        console.error("Find", todo.lookFor, "starting at", todo.startAt)
        const searchResult = findParentDefinition(graph, todo)

        if ("foundPath" in searchResult) {
            console.error("Got 'im! Found", todo.progress.concat(searchResult.foundPath), "to replace", todo.lookFor)
            resolveIdentifier(graph, todo.replace, todo.progress.concat(searchResult.foundPath))
        } else if ("afterName" in searchResult) {
            console.error("Need to resolve", searchResult.afterName, "first")
            console.error("Not found so far:", notFound)
            const stackIndex = stack.findIndex(j => j.replace === searchResult.afterNode)
            const nodeUnfound = notFound.find(value => value.missingNode === searchResult.afterNode)
            if (stackIndex < 0 && !nodeUnfound) throw new Error("programming error: unresolved ID not in stack")

            if (nodeUnfound) {
                // We need to resolve something we have previously marked as unfindable
                // So this node is unfindable too
                notFound.push({ missingName: todo.lookFor, missingNode: todo.replace })
                continue
            }

            // Found the identifier on the stack, so shift it up to occur next
            const requiredJob = stack[stackIndex]
            stack.copyWithin(stackIndex, stackIndex+1, stack.length)
            stack.length -= 1
            stack.unshift(todo)
            stack.unshift(requiredJob)
        } else if ("missingName" in searchResult) {
            notFound.push(searchResult)
        } else if ("startAt" in searchResult) {
            searchResult.progress.unshift(...todo.progress)
            stack.unshift(searchResult)
        } else {
            throw new Error("programming error: unhandled default case: " + searchResult.toString())
        }
    }

    return notFound
}

export function parse(game: string) {
    const matchResult = Grammar.match(game)
    if (matchResult.failed()) {
        console.error(Grammar.trace(game).toString())
        throw new Error(matchResult.message)
    }

    const graph: SourceTree = new Graph();
    Semantics(matchResult).sourceNode(graph)

    // Resolve scopes
    const notFound = resolveScopes(graph)
    notFound.forEach(missing => console.error(`Missing identifier "${missing.missingName}"`))

    return graph
}

export default parse
