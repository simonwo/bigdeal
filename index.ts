import { program } from "commander"
import { parse } from "bigdeal-lang"
import fs from "fs"
import path from "path"

program
    .command("graph <file.bigdeal>")
    .action(sourceFile => {
        const source = fs.readFileSync(sourceFile).toString()
        const graph = parse(source)

        const name = path.basename(sourceFile)
        console.log(`digraph "${name}" {`)
        graph.forEachNode((node, attr) => console.log(`"${node}" [label="${Object.entries(attr).map(entry => `${entry[0]}: ${entry[1]}`).join("\n")}"]`))
        graph.forEachEdge((edge, attr, src, dst) => console.log(`"${src}" -> "${dst}" [label=${attr.kind}]`))
        console.log("}")
    })

program.parse(process.argv)
