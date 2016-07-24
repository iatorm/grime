# Tutorial

_This tutorial is incomplete.
Suggestions are welcome!_

## First Principles

Grime is a language for matching two-dimensional patterns (grids) of characters.
This is analogous to how regular expressions are used to match strings, which are one-dimensional patterns of characters.
The idea is that a Grime program, called a _grammar_, defines an _expression_ that may or may not match a given rectangular grid.
The executable takes such a grammar and a grid, and either tries to find a match in the grid (the default), enumerates all matches, or just tries to match the entire grid.

As with regular expressions, Grime expressions are constructed from simple parts using different combinators (infix operations) and modifiers (postfix operations).
For example, take the regular expression `a+ba+`.
The parts `a` and `b` are _atomic expressions_, which match the respective letters.
The modifier `+` transforms the expression `a` into `a+`, which matches one or more adjacent copies of `a`.
The concatenation `a+ba+` can be seen as a combination of `a+`, `b` and `a+`: it matches first `a+`, then `b`, then `a+` again.

## A Simple Grammar

As said, a Grime expression may match a rectangular grid of characters.
The simplest expression is a _literal_, which matches a **1&times;1** grid containing a fixed character.
All Grime literals are escaped with a backslash; for example, the expression `\a` matches the character `a`.

The modifier `+` is _horizontal repetition_.
If `E` is an expression, then `E+` matches any rectangle formed by gluing one or more `E`s in a row.
The `E`s may be of different widths, but their heights must match.
Here's an ASCII art picture of the situation:

    +----+---+-------+--+
	|    |   |       |  |
	| E  | E |   E   |E |
	|    |   |       |  |
	+----+---+-------+--+

For example, the expression `\a+` matches any **n&times;1** grid of `a`s, where **n &ge; 1**.

The concatenation of two expressions (which is really an "invisible" binary operator) means horizontal gluing: `EF` matches a rectangle whose left side matches `E` and right side matches `F`.
The border between `E` and `F` may go anywhere in the rectangle, and the expressions may even have zero width.
Here's again a picture:

    +----+-------+
	|    |       |
	| E  |   F   |
	|    |       |
	+----+-------+

Thus, `\a+\b` matches an **n&times;1** rectangle, where **n &ge; 2**, the rightmost character is a `b`, and the other are `a`s.
The expression `\a+\b\a+` matches one `b` surrounded by one or more `a`s on both sides.

Let's try out this grammar!
Create a grammar file named `ex1.gr`, containing only the expression `\a+\b\a+`, and a pattern file named `pat1` containing the string `ababaa`.
Then, run `grime ex1.gr pat1` on the command line.
The result should be `abaa`, the first matching sub-rectangle that Grime found in the pattern file (it tries large rectangles first, so `abaa` matches before anything else).

The grammar file may contain _option flags_, which are reparated from the main grammar by a backtick.
If you modify the grammar into `` a`\a\b+ ``, the output will list all matched sub-rectangles instead of just the first.
Also, `` n`\a\b+ `` counts the number of matches, and `` e`\a\b+ `` matches the entire pattern against the expression, producing `0` for "no match" in our case.

## Vertical Concatenation

In addition to gluing rectangles side by side, Grime allows them to be glued on top of each other.
The operator `/` is vertical concatenation: `E/F` matches a rectangle whose top part matches `E` and bottom part matches `F`.
Also, `E/+` matches one or more `E`s stacked vertically on top of each other.
Analogously to the horizontal case, the parts may have different heights, but their widths must match.

Let's put the vertical concatenation operators into action!
Consider the expression `\a\b+/\d+`.
The `/` has lower precedence than horizontal concatenation, so it will be parsed as `(\a\b+)/(\d+)`.
On the left, we have the expression `\a\b+`, which matches an **n&times;1** rectangle consisting of an `a` and then one or more `b`s.
On the right, we have `\d+`, which matches an **n&times;1** rectangle consisting of one or more `d`s.
Their vertical concatenation matches an **n&times;2** rectangle like

    abbb
	dddd

Let's make a vertical repetition of this pattern.
We can use parentheses for grouping, so `(\a\b+/\d+)/+` does the job: it matches patterns like

	abbb
	dddd
	abbb
	dddd
	abbb
	dddd

where the above **n&times;2** pattern is repeated one or more times.

> ### Excercise
>
> Write an expression that matches any **n&times;m** rectangle, where **n&ge;3** and **m&ge;3**, that consists entirely of `a`s with the exception of one `b` that doesn't touch the borders.
> Like this:
>
>     aaaaa
>     aaaba
>     aaaaa
>     aaaaa

## Character Classes

Similarly to regular expressions, Grime supports character classes defined by brackets `[]`.
The expression <code>[&hellip;]</code> matches any **1&times;1** pattern whose character occurs inside the brackets.
It is effectively a disjunction of literals, but with a more terse syntax.<sup>1</sup>
For example `[abcdr]` matches either `a`, `b`, `c`, `d` or `r`.
Hyphens can be used to shorten ranges of characters, so the above is equivalent to `[a-dr]`.
The comma is used to define _negative characters_ that are excluded from the class.
For example, `[a-z,aeiou]` matches all lowercase English consonants, since we excluded the vowels.
If no non-negative characters are listed, as in `[,asdf]`, the resulting class will match all characters except the negative ones.
The characters `[]-,\` have to be escaped with backslashes inside the character class construct.

> ### Excercise
>
> Write an expression that matches any **n&times;m** rectangle, where **n&ge;2** and **m&ge;2**, that contains only vowels on its left and bottom borders, and only consonants everywhere else.
> Like this:
>
>     anrbz
>     ejhjp
>     egghw
>     eaaio

## Logical Operators

Expressions can also be combined using logical conjunction (`E&F`) or disjunction (`E|F`), which behave as expected.
For example, `\a+|\a/+` matches either an **n&times;1** or **1&times;n** rectangle of `a`s.
Note that the logical operations only work on expressions, not operators, so you cannot do `\a(+|/+)` or something similar.
Disjunction has higher precedence than conjunction, so `E&F|G` is parsed as `(E&F)|G`.
Logical negation is denoted by the exclamation mark: `E!` matches whenever `E` does not match.

Logical conjunction is already a quite powerful tool.
For example, we can match a rectangle that's divided into four parts by two lines, where the upper-left and lower-right parts consist of `a`s and the others of `b`s, like this:

    aaabbbb
	aaabbbb
	bbbaaaa
	bbbaaaa
	bbbaaaa

Namely, we can get the unbroken horizontal line with the expression `(\a+/+\b+/+)/(\b+/+\a+/+)`, and the unbroken vertical line with the expression `(\a+/+/\b+/+)(\b+/+/\a+/+)`.
Their conjunction

    (\a+/+\b+/+)/(\b+/+\a+/+)&(\a+/+/\b+/+)(\b+/+/\a+/+)

has both an unbroken horizontal line and unbroken vertical line.

> ### Excercise
>
> Write an expression that matches any rectangle of `a`s and `b`s where every horizontal row and vertical column contains at least one `b`, like this:
>
>     aaabb
>     baaab
>     aabab
>     abaab

## Built-in Expressions

Grime contains several atomic expressions that are built into the language.
We list some of them here.

- The dot `.` is equivalent to `[]`; it matches any single character.
- The dollar `$` is a "wildcard" that matches all rectangles, regardless of size or content.
- The letter `f` (called "flat") matches all rectangles of height 0, and `t` (called "thin") matches all rectangles of width 0.
- There are six built-in character classes: `d`igit, `u`ppercase, `l`owercase, `a`lphabetic, alpha`n`umeric, and `s`ymbol. They all work in the ASCII range only.

Some operators are defined in terms of these expressions.
The option operator `E?` is syntactic sugar for `t|E`, and the vertical choice `E/?` is equivalent to `f|E`.
We also have the zero-or-more-repetitions operators `E*` (equivalent to `t|E+`) and `E/*` (equivalent to `f|E/+`).

## Containment

The operator `#` denotes containment: `E#` matches any pattern that contains a match of `E` as a sub-pattern.
For example, `(\a\b/\b\a)#` matches a pattern that contains a copy of

    ab
	ba

The idiom `E#!` denotes _forbidden patterns_, and it is surprisingly powerful.
For example, consider the task of matching an arbitrary-sized "chessboard" of `a`s and `b`s, like this:

    abababa
	bababab
	abababa
	bababab

The top left corner may also be a `b`.
We could construct an expression by repeating the pattern `\a\b/\b\a` and taking care of the borders in some way, but this quickly becomes complicated.
A much simpler approach is to match a rectangle of `a`s and `b`s that contains no adjacent characters:

    [ab]+/+&(\a\a|\a/\a|\b\b|\b/\b)#!

## Size Constraints

In Grime, there is a simple way for constraining the size of a expression: _size constraints_.
A size constraint is a postfix operator defined with braces `{}`, and it contains one or two numerical ranges.
The most complete form of a size constraint is `E{a-b,c-d}`, and it matches a pattern that matches `E`, and is also of size **n&times;m**, where **a&le;n&le;b** and **c&le;m&le;d**.
For example, a **3&times;3** chessboard can be matched with

    ([ab]+/+&(\a\a|\a/\a|\b\b|\b/\b)#!){3-3,3-3}

However, if `E` is a literal, a dot, or a character class, then `E{a-b,c-d}` matches a rectangle of the given sizes that only contains characters from that class or literal.
For example, `[aeiou]{2-2,2-4}` matches a rectangle of vowels of width 2 and height 2, 3 or 4.
If you leave out some of the numbers, reasonable defaults will be used instead.
If the second range is missing, the first one is simply dupicated.
A missing lower bound is replaced by `0`, and a missing upper bound by infinity.
A missing dash gives equal upper and lower bounds, so `{2,4}` means `{2-2,4-4}`.
Thus, a more compact way of matching **3&times;3** chessboards is

    [ab]{3}&(\a\a|\a/\a|\b\b|\b/\b)#!

In particular, for character literals `\a*/*` is equivalent to `\a{}`.

## Nonterminals

Nonterminals in Grime are used for two purposes: as shorthands for long expressions, and to implement recursion.
A nonterminal is denoted by an uppercase letter, and it must be defined on a separate line in the grammar file.
The syntax for defining a nonterminal, in this case `A`, is this:

    A=\a\b\c

This line defines `A` as the expression `\a\b\c`, and it can then be used anywhere in the grammar file.
For example, you could define the main expression `A/A/A`, which would match the pattern

    abc
	abc
	abc

However, the main strength of nonterminals is that they can refer to themselves.
As an example, suppose we want to match all rectangles of size **n&times;n** where **n&ge;1** (also known as squares).
We can build a square recursively: it is either a **1&times;1** rectangle, or it is a smaller rectangle plus a border of width 1 below and to its right.
Let's turn this into a definition:

    S=.|S./+/.+

As stated above, this defines `S` as either a single character (`.`), or a smaller `S` with a vertical border (`./+`) to its right, and a horizontal border (`.+`) below that.
As a picture, where `A` stands for `./+` and `B` for `.+`:

    +---+-+
	|   | |
	| S |A|
	|   | |
	+---+-+
	|  B  |
	+-----+

Note how the concatenations force the borders to have the correct lengths.
Now, a grammar file must contain exactly one anonymous "toplevel expression" that's not bound to any nonterminal.
Thus a complete grammar file for matching square-shaped inputs is this:

    S=.|S./+/.+
	e`S

As a shorthand, the character `_` refers to the toplevel nonterminal, and can be used in its own definition.
This shortens the above grammar file to the following:

    e`.|_./+/.+

You can define very complex grammars using nonterminals whose definitions refer to each other.

> ### Excercise
>
> Write a grammar that matches an odd-sized square of `a`s whose center point is a `b`, like this:
>
>     aaaaa
>     aaaaa
>     aabaa
>     aaaaa
>     aaaaa

> ### Excercise
>
> Write a grammar that matches correctly nested parentheses `()` and `[]`, like this:
>
>     (())[()([])]

<sup>1</sup> This is not entirely true, as character classes are given special treatement by the size constraint operator. We'll return to that later.