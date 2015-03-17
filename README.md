# Grime

## Introduction

Grime is based on conjunctive grammars.
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
- `-d`: print debug information.

## Syntax and Semantics

A Grime grammar consists of one or more _definitions_, each on a separate line.
Each of them defines the value of a _nonterminal_, and one of them must define the anonymous _toplevel nonterminal_.
The syntax of a definition is either `N=E` or `E`, where `N` is an uppercase letter and `E` is an _expression_.

Expressions are constructed as follows.

- Any character escaped with `\` matches any `1x1` rectangle containing that character.
- `.` matches any single character.
- `$` matches a `1x1` rectangle outside the character matrix.
- `_` matches any rectangle of zero width or height.
- The pre-defined character groups are `d`igit, `u`ppercase, `l`owercase, `a`lphabetic, alpha`n`umeric, and `s`ymbol.
- An unescaped uppercase letter is a nonterminal, and matches the expression it is assigned.
- If `P` and `Q` are expressions, then `PQ` is just their horizontal concatenation, and `P/Q` is their vertical concatenation, with `P` on top.
- `P+` is one or more `P`s aligned horizontally, and `P/+` is the same aligned vertically.
- The Boolean operations are denoted `P|Q`, `P&Q` and `P!`.
- `P?` is shorthand for `P|_`, `P*` for `P+|_`, and `P/*` for `P/+|_`.
- `P#` matches any rectangle that contains a match of `P`.

## Notes

Grime does allow paradoxical definitions of the form `A=A!` with undefined behavior.
However, they will not cause crashes or infinite loops.

Grime supports non-rectangular inputs; the rows are simple aligned to the left, and the gaps can be matched using `$`.

My parser is not as smart as I would like it to be.
In particular, it cannot handle espressions of the type `A#!`, so I have to insert parentheses: `(A#)!`.
This can be seen in the chessboard examples.
Use the debug flag to check your expressions, if you notice anything strange.