# Grime

## Introduction

Grime is a two-dimensional pattern matching language based on Boolean grammars.
The basic idea is to construct rectangular patterns from smaller components, and check whether they are found in the input matrix.

## Usage

A Grime program is called  a _grammar_, and the correct file extension for a grammar is `.gr`, although this is not enforced.
The program is called like

    runhaskell grime.hs [options] grammarfile matrixfile

where `matrixfile` is a file containing the character matrix.
For example, the digits program would be called like

    runhaskell grime.hs digits.gr digit-matrix

By default, the interpreter prints the first match it finds, but this can be controlled using the option flags:

- `-e`: match only the entire matrix, print `1` for match and `0` for no match.
- `-n`: print the number of matches, or the entire matrix if `-e` is also given.
- `-a`: print all matches.
- `-p`: print also the positions of the matches, in the format `(x,y,w,h)`.
- `-s`: do not print the matches themselves.
- `-b`: include borders of width 1.
- `-d`: print debug information.

These can be inserted from the command line, or in the beginning of any line of a grammar file, separated from the rest of the line by a backtick `` ` ``.

## Syntax and Semantics

A Grime grammar consists of one or more _definitions_, each on a separate line.
Each of them defines the value of a _nonterminal_, and one of them must define the anonymous _toplevel nonterminal_.
The syntax of a definition is either `N=E` or `E`, where `N` is an uppercase letter and `E` is an _expression_.

Expressions are constructed as follows.

- Any character escaped with `\` matches any `1x1` rectangle containing that character.
- `.` matches any single character.
- `b` matches a `1x1` rectangle outside the character matrix. It's only relevant if the `-b` option is used, or if the input is not rectangular.
- `$` always matches.
- `f` matches any 0-height (flat) rectangle, and `t` matches any 0-width (thin) rectangle.
- The pre-defined character groups are `d`igit, `u`ppercase, `l`owercase, `a`lphabetic, alpha`n`umeric, and `s`ymbol.
- New character classes can be defined by the syntax `[a-prt-w,d-gu]`. The letters on the left are included, and those on the right are excluded, so this matches exactly the letters `abchijklmnoprtvw`. If the left side is empty, it is taken to contain all characters. The comma can be omitted if the right side is empty. The characters `[],-\` must be escaped with `\`.
- An unescaped uppercase letter is a nonterminal, and matches the expression it is assigned.
- If `P` and `Q` are expressions, then `PQ` is just their horizontal concatenation, and `P/Q` is their vertical concatenation, with `P` on top.
- `P Q` is like `PQ`, but with lower precedence than `/`. This sometimes saves parentheses.
- `P+` is one or more `P`s aligned horizontally, and `P/+` is the same aligned vertically.
- The Boolean operations are denoted `P|Q`, `P&Q` and `P!`.
- `_` is shorthand for `f|t`.
- `P?` is shorthand for `t|P`, `P/?` for `f|P`, `P*` for `t|P+`, and `P/*` for `f|P/+`.
- `P#` matches any rectangle that contains a match of `P`.
- `P{a-b,c-d}`, where `abcd` are nonnegative integers, is a _size constraint_ on `P`. If `P` is a character class, then the expression matches any `mxn` rectangle containing only those characters, provided that `m` is between `a` and `b` inclusive, and `n` is between `c` and `d` inclusive. In other cases, the expression matches any rectangle that has the correct size and that `P` also matches. If `a` or `c` are omitted, they are taken to be `0`, and if `b` or `d` are omitted, they are infinite. If the hyphen between `a` and `b` is omitted, then we use the same number for both ends of the interval. If the entire `c-d` part is omitted, both axes are constrained. To clarify, `{-b}` is equivalent to `{0-b,0-b}`, and `{a-,c}` is equivalent to `{a-infinity,c-c}`.

## Notes

Grime does allow paradoxical definitions of the form `A=A!` with undefined behavior.
However, they will not cause crashes or infinite loops.

Grime supports non-rectangular inputs; the rows are simple aligned to the left, and the gaps can be matched using `$`.
