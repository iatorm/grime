# Grime

## Introduction

Grime is a two-dimensional pattern matching language based on Boolean grammars.
The basic idea is to construct rectangular patterns from smaller components, and check whether they are found in the input matrix.

You can [try Grime online](http://grime.tryitonline.net/), courtesy of [Dennis from PPCG](http://codegolf.stackexchange.com/users/12012/dennis).
Be sure to check the [tutorial](https://github.com/iatorm/grime/blob/master/tutorial.md) too.

## Usage

A Grime program is called  a _grammar_, and the correct file extension for a grammar is `.gr`, although this is not enforced.
The program is called like

    runhaskell grime.hs [options] grammarfile matrixfile

where `matrixfile` is a file containing the character matrix.
For example, the digits program would be called like

    runhaskell grime.hs digits.gr digit-matrix

If you want a speed boost, you should compile Grime with the command

	ghc -O2 grime.hs

and then run the resulting executable:

	grime [options] grammarfile matrixfile

There's also a bash script `rungrime` that compiles Grime if needed and then calls the executable with the provided arguments.

By default, the interpreter prints the first match it finds, but this can be controlled using the option flags:

- `-e`: match only the entire matrix, print `1` for match and `0` for no match.
- `-n`: print the number of matches, or the entire matrix if `-e` is also given.
- `-a`: print all matches.
- `-p`: print also the positions of the matches, in the format `(x,y,w,h)`.
- `-s`: do not print the matches themselves.
- `-b`: include borders of width 1.
- `-d` or `-d0`: print debug information.
- `-d1`: print logs from the matcher.

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
- New character classes can be defined by the syntax `[a-prt-w,d-gu]`. The letters on the left are included, and those on the right are excluded, so this matches exactly the letters `abchijklmnoprtvw`. If the left side is empty, it is taken to contain all characters and the border. The comma can be omitted if the right side is empty. The characters `[],-\` must be escaped with `\`. `\b` matches a border.
- An unescaped uppercase letter is a nonterminal, and matches the expression it is assigned.
- `_` is the anonymous toplevel nonterminal.
- If `P` and `Q` are expressions, then `PQ` is just their horizontal concatenation, and `P/Q` is their vertical concatenation, with `P` on top.
- `P Q` is like `PQ`, but with lower precedence than `/`. This sometimes saves parentheses.
- `P+` is one or more `P`s aligned horizontally, and `P/+` is the same aligned vertically.
- The Boolean operations are denoted `P|Q`, `P&Q` and `P!`. Boolean equivalence is `P~Q`.
- `P?` is shorthand for `t|P`, `P/?` for `f|P`, `P*` for `t|P+`, and `P/*` for `f|P/+`.
- `P#` matches any rectangle that contains a match of `P`.
- `P{a-b,c-d}`, where `abcd` are nonnegative integers, is a _size constraint_ on `P`. If `P` is a character class, then the expression matches any `mxn` rectangle containing only those characters, provided that `m` is between `a` and `b` inclusive, and `n` is between `c` and `d` inclusive. In other cases, the expression matches any rectangle that has the correct size and that `P` also matches. If `a` or `c` are omitted, they are taken to be `0`, and if `b` or `d` are omitted, they are infinite. If the hyphen between `a` and `b` is omitted, then we use the same number for both ends of the interval. If the entire `c-d` part is omitted, both axes are constrained. To clarify, `{-b}` is equivalent to `{0-b,0-b}`, and `{a-,c}` is equivalent to `{a-infinity,c-c}`.
- `PoS`, where `S` is a (greedily parsed) string of the characters `01234567`, optionally terminated with `}`, is an _orientation modifier_. Each of the characters stands for a rotation (`0123`) or reflection (`4567`), which is applied to the expression `P`. The transformed expressions are combined with Boolean disjunctions. The characters `OXNTKHADC` encode convenient classes of orientations. The character `F` fixes an expression, so that its orientation cannot be modified.
- `<P>` is a _context bracket_. It matches any rectangle **r** that's contained in a larger rectangle that matches `P`. The rectangle **r** can be matched by a digit in `P`; this is `0` by default, and increases with the nesting depth.

You can add the prefix `^` to any infix operator or chain of postfix operators to raise its precedence higher than all other operators, or `v` to lower it; for example, `\a\b^|\cv+` is parsed as `(\a(\b|\c))+`.
Double quotes `""` are used for swapping the `\`-escaped status of all characters inside them, except for the characters `"\/`.
They allow text grids like `"this/is a/grid"`.
Non-literal syntax elements, like nonterminals, operations and parentheses, can be used in quoted expressions by escaping them.
Note that quotes define syntax elements, so `"ab/cd"+/E` will be parsed as `(\a\b/\c\d)+/E`.
All open parentheses, brackets and quotes are implicitly closed at the end of a line.
Lines beginning with `|` are comments, and the parser ignores them.

## Notes

Grime does allow paradoxical definitions of the form `A=A!`; these will result in no match.
In general, the matching engine can resolve any grammar where no match of a single nonterminal on a single rectangle depends on itself (although for complex grammars, this can take a while).
A nontrivial example of a resolvable grammar is

    A=\a|E\a
    B=[ab]+&A!
    E=A|B
    a`A

Given a one-dimensional string of `a`s and `b`s, this grammar will match every substring that ends in `a`.
Note how the nonterminal `A` is self-referential in more than one way (via `E` directly, and via `E` through `B`), but since the `E` in `A` must be a proper substring, the system can always be resolved.

Grime supports non-rectangular inputs; the rows are simply aligned to the left, and the gaps can be matched using `b`.
