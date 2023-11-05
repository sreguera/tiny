# Tiny Basic

A web browser implementation of the [Tiny BASIC](https://en.wikipedia.org/wiki/Tiny_BASIC)
programming language.

## Implementation defined behaviour

* REM statement.
* Uninit variables are 0.
* Case sensitive.

## Development

See the [elm documentation](https://guide.elm-lang.org/install/elm).

## TODO

* Error handling for user and system errors.
* Error handling in the VM assembler:
    * Goto a label outside of range, e.g. label is the last inst.
    * Positions 2 (read line) and 7 (stmt) are significative. Check labels.
* Decide on system limits: Basic line numbers range, number range, stack size.
* Unit/fuzz/prop complete testing.
* Better module exports.
* UI: Terminal, terminal style save/load, VM debugging.
* Break/Ctrl-C support. Tick.
* Compile as a github action.