# Tiny Basic

A web browser implementation of the [Tiny BASIC](https://en.wikipedia.org/wiki/Tiny_BASIC)
programming language.

## TODO

* Error handling for user and system errors.
* Error handling in the VM assembler. Error if dup labels, unknown label.
* Decide on system limits: Basic line numbers range, number range, stack size.
* Add REM instruction.
* Unit/fuzz/prop complete testing.
* Better module exports.
* UI: Terminal, terminal style save/load, VM debugging.
* Break/Ctrl-C support. Tick.
* Compile as a github action.