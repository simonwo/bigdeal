import grammar from './bigdeal.ohm';
import tictactoe from './../../examples/tic-tac-toe.bigdeal';
import cards from './../../examples/cards.bigdeal';
import * as ohm from 'ohm-js';
import * as graphology from 'graphology';

import Graph = graphology.MultiGraph;

enum LiteralType {
 TextType = "text",
 IntType = "int",
 BoolType = "bool",
}

type LiteralNode = {
    type: LiteralType
    value: string | number | boolean
}

enum EdgeType {
    Source = "src",
    Definition = "defn",
    Count = "count",
    Filter = "filter",
    Init = "init",
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
    every: boolean
    take: boolean
    ordered: boolean
    choose: boolean
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
    GreaterThan = ">=",
    LessThan = "<=",
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
    name: string
    op: ModificationType
}

// An IdentifierPath represents a yet-to-be-resolved identifier. The ordering of
// the identifiers represents steps on the path to resolve it.
// E.g. `content of board` would appear as ["board", "content"]
type IdentifierPath = string[]

// A Definition specifies a property of items in a parent sequence. The
// Definition specifies a `source` edge saying where the content of this
// property will come from. The Definition can also specify extensions to the
// source sequence – items in that sequence will have these extra properties
// when they are evaluated.
type Definition = {
    name: string[]
    count: number | null
}

// We will refer to the root of the tree using this node
const RootNodeName: string = "game"

export const Grammar = ohm.grammar(grammar, {IndentationSensitive: ohm.ExperimentalIndentationSensitive});
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
            const attribs = <Definition>this.args.graph.getNodeAttributes(n)
            const type = attribs.count === null ? EdgeType.Definition : EdgeType.Source
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
        const node: SequenceReference = {
            every: every.numChildren > 0,
            take: take.numChildren > 0,
            ordered: next.numChildren > 0,
            choose: choose.numChildren > 0,
        }
        this.addSelf(this.args.graph, node)
        this.maybeEdge(this.args.graph, quantity.sourceNode(this.args.graph), EdgeType.Count)
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
    QuantitySpecifier(node, _) { return node.sourceNode(this.args.graph) },

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
    BinaryOperator_eq: (_) => Operation.Equal,
    BinaryOperator_ne: (_) => Operation.NotEqual,
    BinaryOperator_gt: (_) => Operation.GreaterThan,
    BinaryOperator_lt: (_) => Operation.LessThan,
    BinaryOperator_in: (_) => Operation.Contains,
    BooleanOperator_any: (_) => Operation.Any,
    BooleanOperator_all: (_) => Operation.All,
    _iter: (...l)  => l[0].operation,
})

Semantics.addAttribute<ModificationType>("modificationType", {
    ModificationKeyword_add:     (_) => ModificationType.AddTo,
    ModificationKeyword_prepend: (_) => ModificationType.PrependTo,
    ModificationKeyword_change:  (_) => ModificationType.Change,
})

// path returns an array of strings representing the path to some value, with
// the most significant part (i.e. highest ancestor) of the name first
// e.g. "a of b of c" will return [c, b, a]
Semantics.addAttribute<string[]>("path", {
    IdentifierPath(identifier, _, identifiers): string[] {
        return identifiers.path.concat([identifier.sourceString])
    },
    _iter(...children): string[] {
        return children.flatMap(x => x.sourceString)
    }
})


export default function main() {
    const matchResult = Grammar.match(tictactoe)
    if (matchResult.failed()) {
        console.log(Grammar.trace(tictactoe).toString())
        console.log(matchResult.toString(), matchResult.message)
        return
    }

    const graph: SourceTree = new Graph();
    graph.on("nodeAdded", payload => console.log(`"${payload.key}" [label="${Object.entries(payload.attributes).map(entry => `${entry[0]}: ${entry[1]}`).join("\n")}"]`))
    graph.on("edgeAdded", payload => console.log(`"${payload.source}" -> "${payload.target}" [label=${payload.attributes.kind}]`))

    console.log("digraph tictactoe {")
    Semantics(matchResult).sourceNode(graph)
    console.log("}")
}

main()
