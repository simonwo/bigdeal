import grammar from './bigdeal.ohm';
import tictactoe from './../../examples/tic-tac-toe.bigdeal';
import * as ohm from 'ohm-js';

export default function main() {
    const matcher = ohm.grammar(grammar, {IndentationSensitive: ohm.ExperimentalIndentationSensitive})
    const matchResult = matcher.match(tictactoe)
    if (matchResult.failed()) {
        console.log(matcher.trace(tictactoe).toString())
        console.log(matchResult.toString(), matchResult.message)
    }
    console.log(matchResult.succeeded())
}

main()
