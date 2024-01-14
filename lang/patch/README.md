# Ohm.js patches

This folder contains patches to Ohm.js to make their indentation support work in
a non-buggy way. As of writing, there is an [issue open
here](https://github.com/ohmjs/ohm/issues/465) to discuss merging patches like
these into the upstream.

They were generated from my local hacks using:

    npm diff --diff ohm-js@17.1.0 --diff lang/node_modules/ohm-js

The patches are:

1. Keep memoization disabled – remove the switch that flips memoization off, and
   no longer make assumptions about whether memoized states exist. This is
   necessary because when we initially fail to parse something, backtrack and
   then successfully parse and indent or dedent, and then return to the same
   rule later, the memoized copy still thinks a parse failure is inenvitable.
2. Output indents in traces – include arrow characters in the trace output to
   show the position of indents and dedents. Not strictly necessary, but is
   incredibly helpful for understanding why parses fail due to invisible
   characters.
