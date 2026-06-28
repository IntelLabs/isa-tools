---
layout: post
title: "Designing language syntax for readability"
author: Alastair Reid
tags: language, design
---

One of the things that I learned on the Haskell Committee is that the syntax
of a language really matters (see section 4 of
["A history of Haskell: Being lazy with class," HOPL III (2007)](https://dl.acm.org/doi/10.1145/1238844.1238856)).
This is especially true when designing an ISA specification language because
we want a user of the spec to be able to pick up the instruction manual, read
the description of an instruction and have a clear (and correct) understanding
of what the instruction does without having to read any of the rest of the manual.
It must be obvious what the syntax means.


*[This article is part of a series of articles about the design of the The Intel® ISA Specification Language.
For more articles, see 
[#1]({{ site.baseurl }}{% post_url 2026-06-27-designing-a-language-for-readability %}),
[#2]({{ site.baseurl }}{% post_url 2026-06-28-naming-conventions-for-readability %}),
[#3]({{ site.baseurl }}{% post_url 2026-06-29-designing-syntax-for-readability %})
]*

## Character set

The widespread availability of Unicode removes many of the restrictions on character sets: we are free to use any symbol that most clearly conveys our intended meaning.
In particular, we can use mathematical symbols like "×" for multiplication, "÷" for division, "←" for assignment, "⌊" and "⌋" for floor, etc.
This is used with particular effectiveness in the machine-checked theorem proving community where the statement of a theorem often looks the same as you would find in a mathematical textbook.

However, we chose not to do this because, even though we are designing the language to be *read*, we know that some of our readers will have to *write* in the language as they submit bug reports, quote instruction descriptions in comments and code reviews, ask us questions, etc.
Unfortunately, typing non-standard characters is awkward and annoying (especially if you only have to do it occasionally) and,
although most tools, interfaces, communication systems, etc. fully support Unicode, there are still a few that are limited to old character sets.

So, unfortunately, we are stuck with the 7-bit ASCII character set and, in particular, the characters you see on the keyboard in front of you.
This does not give us a lot of symbols to work with so we have to be extremely careful about how we use them.
A particular problem is that there are very few obvious pairs of symbols: "(" and ")"; "[" and "]"; "{" and "}"; and (if we get really desperate) "<" and ">".

One way around this limitation is to use digraphs (pairs of characters) such as `<-`, `->`, `=>`, `:=`, `||`, `<<`, etc.
And we can solve the problem of limited pairs of symbols by using digraphs such as so-called "fat-brackets" `[[` and `]]` (often rendered as `⟦...⟧`)
or "lenses" `[(` and `)]` (often rendered as `〖...〗`).
Here too, we have to be careful though.
Does adding a space change the meaning of a piece of code (e.g., `x <- y` versus `x < -y`?
Would a reader picking up the specification for the first time understand what `x[[i]]`
(or `x⟦i⟧`) is supposed to mean?
Any use of digraphs needs to be either very, very traditional or so insanely useful that it is worth the cost of using it.

With so few symbols available, we inevitably end up using the same symbols with multiple different meanings
and we must be careful that this overloading helps the reader and does not cause confusion.
Some of this overloading is very conventional (e.g., using the same parentheses symbols both to group subexpressions (as in `4 * (size - 1)`) and for function application (as in `Rotate_Left(src, 1)`).

Some overloading aims to guide the reader by using the same syntax for
similar concepts.
For example, we use the symbol `=>` whenever we are creating an association from values/labels/keys to something.
In array and record initializers we associate indexes and field names with values.
In case statements we associate case labels with blocks of code.
In function calls, we associate argument names with values.

Similarly, we use `x[i]` for array indexing and for bitvector indexing
to emphasize the view that bitvectors are just arrays of bits.
And we build on that by using square brackets for bitslicing (extracting
a sequence of bits from a bitvector) as in `x[31 : 24]` which extracts an 8-bit byte from a bitvector.

## Comments

There are two common forms of comment: single-line comments and block comments.

Single-line comments start with a marker such as `//` and extend to the end of the line.
These are very readable because the presence of the marker 
makes it easy to see that it is a comment and not code.

Block comments start with an opening marker such as `/*` or `(*` and continue
until a closing marker such as `*/` or `*)` is found.
We initially decided not to support this style of comment because it is harder to find the
start and end of the comment.

However, as we focused more on generating high quality documentation for specifications,
it became clear that we wanted to add metadata to definitions.
This metadata is primarily used for documentation and contains short and long descriptions
of the definition, a category for the definition, references to sections of the
Software Developer's Manual, etc.
We currently write metadata in YAML but we are considering switching to XML so we used
the popular "fenced code block" syntax for metadata.
For example, here is the file for the `Byte_Reverse` function.

    ```yaml
    architectural: true
    category: integer/data_transfer
    kind: Function
    name: Byte_Reverse
    shortdesc: Reverse byte order
    ```
    
    function Byte_Reverse(x : Bits(8*size), size : Integer) -> Bits(8*size)
    begin
        var r : Bits(8*size);
        for i := 0 to size-1 do
            let j := size-1 - i;
            r[i *: 8] := x[j *: 8];
        endfor;
        return r;
    end

From the point of view of our compiler, a fenced code block is just a block comment.
But it is a very unusual "comment" because our documentation generation tools don't
directly show the comment to the user.

*[If you are thinking that these metadata blocks are basically the same as doc comments like in
[JavaDoc](https://en.wikipedia.org/wiki/Javadoc),
[rustdoc](https://doc.rust-lang.org/rustdoc/what-is-rustdoc.html),
etc.
then you are more or less right.
The most important difference is probably a shift of emphasis:
this is for any metadata that we want to add, not just for documentation.]*

## Numbers

There can't be many programming languages that don't treat a sequence of digits such as "12345" as a decimal number.
Some extensions of this basic idea are a bit unfortunate. For example, C treats a number that starts with a "0" as an octal number so "012345" is a very different number (30061 in decimal).
And some extensions we decided were worth adopting. 
For example, Ada allows the addition of underscores within a number to make it easier to read large numbers such as "1_000_000_000".
Although not enforced by our compiler, our style guide requires use of "_" every three digits.


When used in English prose, Intel's documentation tends to use an `H` suffix to indicate hexadecimal
numbers (for example `00H`) but we felt that programmers would more readily
recognize the more conventional "0x" prefix instead
since it is used in
many languages such as C, C++, Python, Javascript, etc. .
As with decimal numbers, we allow underscores to improve readability,
and we usually place the underscores every 4 digits.



Binary numbers are, of course, incredibly useful in ISA specifications but there is not a lot of agreement on how to write them.
Intel's existing documentation uses a `B` suffix (for example, one might write `1100B`).
Verilog uses a "b" prefix (for example `b1100`).
Ada specifies the base and surrounds the bits with "#" (for example `2#1100#`).
VHDL uses double quotes (for example `"1100"`).
Rust uses a "0b" prefix (for example `0b1100`).

We rejected the use of double quotes because software developers would likely think that they are strings.
We felt that the Ada notation was a bit too verbose and clunky for something that is extremely common in ISA specifications.
The brevity of using a "b" or "B" prefix or suffix was attractive but we were concerned that readers might confuse tokens like `b1` for a 2-letter variable name or `1b` for a hex number.

In the end, we decided to copy Rust's example and use a "0b" prefix. While many of our readers will not know Rust, it is consistent with and an obvious extension of the hexadecimal notation.

As with decimal and hex numbers, we allow underscores to make it easier to read binary numbers. For example `0b1111_0000_0000_1111`.
Although our general rule is to use an underscore every fourth bit, it is often useful to break that rule based on how different bits are used.
For example, suppose that some 8-bit value is interpreted as three fields of
3-, 3- and 2- bits: in that context, writing `0b110_110_11` makes it easier
to interpret the value.

## Operators

There is a small set of operators that can be considered uncontroversial (+, -, *, <, >).
Every other operator has issues.

Assignment (`:=`)
: Since C, it has become common to write `x = 1;` to mean "assign 1 to x".
  But it has also been necessary to add warning flags to C compilers to warn when
  the code should have used an equality test `x == 1`.

  An alternative is to use a backwards arrow "x ← 1;".
  This is obvious and unambiguous and Intel manuals used to use this syntax.
  However, it is harder to type (I had to cut and paste the symbol from a web page of unicode symbols)
  and some bug reporting tools don't display unicode correctly.

  Algol used the digraph `:=`.
  (This was apparently due to the limitations of character sets and keyboards
  from the time and assignment was often typeset as "←" in books and papers.)
  So we chose to use `x := 1;` for assignment.
  Although this notation is out of fashion in programming languages, we decided to adopt it because
  we believe that all readers will recognize it as an assignment and we want a
  symbol that cannot be confused with an equality test.


Equality and inequality tests (`==` and `!=`)
: Since we are not using the syntax `x = 1` for assignment, we could use `=` for equality tests
  but, because `=` means assignment in many programming languages,
  this choice would be sure to cause confusion for both writers and readers.

  So, a bit reluctantly, we followed common practice and write `x == 1`
  for equality tests and `x != 1` for inequality tests.

Ordering comparisons (`<=`, `<`, `>` and `>=`)
: The `Integer` type represents the mathematical integers so a comparison like `x < y` clearly
  has to be a signed comparison.

  But what about bitvectors? Should we treat them as signed or unsigned?
  Bitvectors are used in a wide variety of ways in an ISA.
  They are used to represent signed values (e.g., when used as address offsets
  or as inputs to the signed multiply instruction);
  they are used to represent unsigned values (e.g., when used as size fields
  in instruction format or as version numbers in a CPUID register);
  and many uses are not clearly signed or unsigned (e.g., when testing whether
  a control bit is set; when used as an address).

  Given this range of interpretations, treating bitvectors as either signed or
  unsigned will sometimes cause confusion because readers will sometimes make
  the opposite assumption about whether they are signed or unsigned.

  We could follow C's example and have signed and unsigned bitvectors.
  But the lesson from C is that signedness is a rich source of confusion
  so this is not an attractive option.

  We could follow the example found in DEC ISA specifications of writing
  `x <u y` for unsigned comparisons and `x <s y` for signed comparisons.
  But, if we do this for bitvectors, should we do this for integers even
  though they do not need it?

  Our approach is not to directly provide ordering comparisons for bitvectors.
  Instead, the specification explicitly converts the bitvectors to integers
  using either `Std::Bits::Unsigned` to interpret the bitvector as an unsigned
  binary number or using `Std::Bits::Signed` to interpret the bitvector as
  a 2's complement signed binary number. That is

  - `Unsigned(x) < Unsigned(y)` --- for unsigned comparison
  - `Signed(x) < Signed(y)` --- for signed comparison

  This is a bit verbose but we think that it is very clear about the
  comparison being performed.
  It is also more flexible: there are a few places in our specification
  where we write `Signed(x) < Unsigned(y)`.

Division and remainder (`/` and `%`)
: The symbols `/` and `%` for division and remainder are well established and understood.
  (There is no need to follow Modula-2 or Haskell and use keywords like `div` and `mod`.)

  What is not well established is the result of dividing negative numbers.
  Various languages and ISAs have adopted different answers and some (C)
  declined to define the meaning, we cannot expect readers to reliably
  guess whether `-7 / 2` (say) is
  `-3` (rounding up) or `-4` (rounding down).

  Our solution to this problem is to extend the idea that division by zero is disallowed
  and to also disallow division of negative arguments.
  Almost all uses of `/` and `%` in ISA specifications only involve positive numbers
  such as sizes and offsets so this restriction has little impact on the specification.
  And testing is very effective at finding cases that do involve division of negative
  numbers.

  Of course, there are a few places where division of negative arguments is important in
  an ISA: most obviously, in the signed integer division instruction.
  In these few places, we want the specification to be quite explicit about the rounding
  behavior of the operation so, instead of trying to use a concise but ambiguous
  operator `/`, we use one of several verbose, unambiguous function names

  - `Floor_Divide` and `Floor_Remainder` which round down to -∞
  - `Ceiling_Divide` and `Ceiling_Remainder` which round up to +∞
  - `Truncated_Divide` and `Truncated_Remainder` which round towards zero.

Ranges (`..`)
: Haskell and other languages let you write `1..10` to represent the values
  from `1` to `10` (inclusive) or to write `0..` to represent values from
  `0` upwards.
  We found ourselves wanting to write ranges often enough that we adopted
  this notation.
  We also added the option of writing `..10` to represent
  all the values up to and including 10. (Note that this includes negative numbers.)

  (We were also tempted by the way that Python let's you write `0 <= x <= 15`.
  This is very clear and we may add this notation as well.)


Exponentiation (`**`)
: Early languages such as FORTRAN and Algol had special syntax for exponentiation
  (i.e., calculating "x raised to the power y") but exponentiation is not used much
  in most programs and recent languages tend to provide exponentiation in math
  libraries instead.

  ISA specifications deal with binary values a lot so quite a few parts of
  a specification need to talk about powers of 2 and so we went back to those
  early languages to see what symbols they used.

  The obvious operator to use for exponentiation is `^`. For example `2^16`.
  Unfortunately, anybody who has used C will expect `^` to mean exclusive-or
  and think that `2 ^ 16 == 18` instead of `2 ^ 16 == 65536`.

  So, reluctantly, we had to use the second best choice which is `**`.
  For example, `2 ** 16`.

  _(This is probably one of the clearest examples in the language design
  where readability concerns force us to make a non-obvious syntax choice.)_

Boolean and bitvector operators
: For operations on Boolean values (like `True` and `False`) and bitvector values (like `0x00` and `0b1111`)
  we have a choice of C-like symbols such as `&`, `|`, `^` and `!`
  or keywords such as `and`, `or`, `xor` and `not`.
  Since these operators are used quite heavily, our first thought was to use symbols since they are
  slightly shorter.
  However, the `^` symbol is a bit cryptic (will programmers who do not use C
  guess that it means exclusive-or?) so we decided to go with the keywords since
  they are explicit about what they mean.

Short circuit boolean operators (`and then` and `or else`)
: The C language has two kinds of boolean operators:
  'strict' operators that always evaluate both operands;
  and 'lazy' operators that only evaluate their second operand if required.
  For example, the C expression `p && *p > 0` only dereferences the pointer `p`
  if `p` is not null.
  Lazy operators are also known as "short circuit operators".

  If we had followed C's example and used `&` and `|` for the strict boolean operators, then
  we would obviously use `&&` and `||` for the lazy boolean operators.
  Early versions of Pascal chose not to define whether `and` and `or` were strict
  or lazy and was rightly criticized for this.

  We chose to follow Ada's example of using `and then` and `or else` for the lazy operators.
  These are more verbose than the strict operators and much more verbose than `&&` and `||`
  but we regard this as a strength not a weakness because
  we think that it is important to draw attention to the fact that part of an expression
  is not being evaluated since this implies that any side-effects in the second
  operand will not be applied.
  In practice, we only use `and then` in a few places in the x86 specification
  and, so far, we have not used `or else` at all.

Statement sequencing (`;`)
: Most languages use semicolons either
  at the end of statements (as in C/C++) or between statements (as in Pascal, Ada, ML).
  The difference between these variants can be seen in the trailing semicolon in this C
  code

  ```C
  x = 1; y = 2; z = 3;
  ```

  and the absence of a trailing semicolon in this Pascal code

  ```Pascal
  x := 1; y := 2; z := 3
  ```

  Strictly speaking, we don't need to add semicolons and we could copy Python in
  not having them.
  However, readers are used to the presence of semicolons at the end of statements
  so, at least for now, we require them at the end of statements.
  (As we start generating more documentation and get reader feedback, we may
  change this rule.)

Bitslices (`:`, `+:`, `-:` and `*:`)
: Hardware documentation has one incredibly useful concept that is
  not found in any software language that we know of: the bitslice.
  A bitslice is a contiguous sequence of bits extracted from a bitvector.
  For example, we might want to extract byte number 6 (i.e., bits 48 up to 55)
  of a bitvector `src`.

  Languages like Verilog provide several different but equivalent ways of writing this

  - `src[55:48]`

    `x[hi : lo]` means a slice of size `hi-lo+1` consisting of all the bits
    from `x[lo]` up to `x[hi]` inclusive.

  - `src[48 +: 8]`

    `x[lo +: size]` means a slice of size `size` consisting of all the bits
    from `x[lo]` up to `x[lo + size - 1]` inclusive.

  - `src[55 -: 8]`

    `x[hi -: size]` means a slice of size `size` consisting of all the bits
    from `x[hi - size + 1]` up to `x[hi]` inclusive.

  - `src[6 *: 8]`

    `x[i *: size]` means the i'th element of size `size`. That is, all the bits
    from `x[i * size]` up to `x[(i+1) * size - 1]` inclusive. Or, if you
    prefer, `x[(i * size) +: size]`.

  Each notation is useful in a different circumstance. For example, `x[hi :
  lo]` works well when `hi` and `lo` are small constants while `x[lo +: size]`
  is useful when we want to emphasize the width or when `lo` or `size` are
  non-trivial expressions.

  Use of these notations breaks our rule that we should only use notation that
  readers will be able to guess the meaning of but we don't see any way
  round this: bitslices are absolutely ubiquitous in ISA descriptions
  so we decided to spend most of our limited "weirdness budget"[^weirdness-budget]
  here.

  Since these notations are not in common use in software programming
  languages, we need to look for additional ways to help the reader understand
  what they mean.  In web pages, we can use tooltips when hovering the mouse
  over this syntax.  In instruction descriptions, there is often some
  accompanying text that describes the instruction verbally.  And we can hope
  that the notation is used so frequently that, even if the reader is a little
  puzzled the first time that they see bitslices, they will remember what it
  means the next time that they see it.

  _(You may wonder whether we need so many ways of writing bitslices?
  Would `src[55:48]` not be enough?
  From a minimalist point of view, the answer is "yes".
  But, from that same minimalist point of view, we could simplify arithmetic
  by removing subtraction and write expressions like `a + (-b)`
  or simplify logical expressions by removing `or` and `xor`.
  These "simplifications" would make code harder to read and the same
  would be true if we only supported one form of bitslice.)_

[^weirdness-budget]:
    A popular idea among language designers is that you have a limited
    "weirdness budget" to spend before users decide that your language
    is too weird to learn or use.
    Since we want our language to be readable by a very diverse range
    of users, our weirdness budget is very, very small.


## Operator precedence

Operator precedence is concerned with when you need to add parentheses
to an expression and when can you leave them out.
For example, you could write `(1 + (2 * (3 ** 4)))` but
most/all cultures around the world agree[^bodmas-mnemonic] that you can omit all of these
parentheses without changing the meaning.
The rule is that Brackets/Parentheses have priority Over Exponentiation which
has priority over Multiplication and Division which has priority over Addition and Subtraction.

[^bodmas-mnemonic]:
    However, the have not agreed on [the mnemonics](https://en.wikipedia.org/wiki/Order_of_operations#Mnemonics)
    used to remember operator precedence:
    different countries call this BODMAS, BOMDAS, PEMDAS, BIDMAS, BEDMAS, *Punktrechnung vor Strichrechnung*, ...

But what about non-arithmetic operations:
logical operators, bitwise operators, comparison operators, shift
operators, slices, ...?
Where do they fit into the BODMAS hierarchy?
Different programming languages have different precedence rules
and programmers sometimes forget the rules so having a well defined
precedence hierarchy is not enough to ensure that everyone
can read our specifications correctly.

To avoid problems, we need to be willing to reject expressions
that different readers might interpret in different ways.
That is, we need to decide that some
pairs of operations cannot be used next to each other without parentheses
because the combination might confuse readers.
For example, we might want to disallow the following expressions

```
a and b or c
a + b << c
a * b << c
```

Under the standard approach to defining operator precedence by
assigning a precedence level to each operator, we would define
where parentheses would be added to these expressions by the parser
but we cannot disallow any of these expressions.
To disallow these expressions, we need to move away from using a *total order*
(i.e., an order that defines the relative precedence of all pairs of
operations).

What we need is a *partial order* that defines the relative precedence
of *some* pairs of operations but does not define the precedence of
*all* pairs.
For example, if we do not define `and` to be higher, lower, or equal precedence to `or`:
then we can define that it is an error to use the operators next to each other without
explicitly adding parentheses.
(Of course, in practice, it is easier to define which pairs of operators can be
unambiguously used next to each other.)

One classic way of implementing operator precedence is
to use Dijkstra's [Shunting yard algorithm](https://en.wikipedia.org/wiki/Shunting_yard_algorithm).
This is normally defined in terms of a total order but
it is trivial to change it to use a partial order
and report that an expression is ambiguous whenever
adjacent operators cannot be ordered.

With the machinery of a partial order and a suitable precedence algorithm in place, we can
then start tweaking the partial order to achieve a balance between confusing
expressions (not enough parentheses) and excessive parentheses (not enough comparable
operators).
Code review processes are invaluable here: watch for reviewers suggesting the
addition or removal of parentheses.

*[Minor confession: the task of finding the perfect precedence rules is not yet done.
Recent code reviews have said that bitslices like `src[x + y +: 8]` should be
rewritten as `src[(x + y) +: 8]`.
In some sense, this is not really a problem because the other way of adding parentheses
(`src[x + (y +: 8)]`) is not a legal expression and, in fact, `+:` isn't really an operator.
But, it is also clear that the combination of `+` with the relatively unfamiliar `+:` symbol
is making it a bit hard for readers to figure out the structure of the expression so
we should probably implement a check for this and similar expressions.]*


## Keywords


Algol introduced block-structure such as

```
if x >= 10 then
   x := x - 10;
fi
```

While people found this `if-fi` pairing kinda cute, generalizations such as
`case-esac`, `do-od`, `function-noitcnuf`, etc. show the limitations of
reversing the keywords.  Later languages avoided the problem by having a
dedicated pair of tokens to surround sequences of statements such as `begin ...
end` in Pascal and `{ ... }` in C but, in nested blocks, it can be tricky for readers
to correctly pair an `end` or `}` with the corresponding start of the block.
(LISP readers will be familiar with the problem of pairing parentheses with each other.)

The problem of pairing the start and end of a block is especially significant in printed code because a page break makes it hard to rely on indentation.
(This is also the reason that we did not adopt indentation-based syntax
as found in Python, Haskell, Occam, ...)

Helping the reader to correctly understand the block structure thus lead us to
the following syntax.

```
if x >= 10 then
    x := x - 10;
endif
```

(and, similarly, `endwhile`, `endcase`, ...)

Fearing a rebellion from the team who were writing specifications, 
our initial implementation also allowed just `end` but we quickly decided that
the more specific but slightly more verbose `endif`, `endcase`, etc.
improved readability enough to be worth the additional typing effort.


*[There is a good argument for minimizing the number of keywords in a language:
every keyword is a variable name that you have stolen from the user.
In this case though, we decided that ISA specifications were unlikely to need to use
the names `endif`, `endwhile`, etc.]*


## Summary

Defining the syntax of a language seems like a trivial task: everyone agrees what numbers, identifiers, and operators look like and keywords like `if`, `then` and `else` are a no-brainer.

Alas, nothing could be further from the truth.
The basic syntax of the language is rich in "religious wars" fought over the tiniest of details;
every language has made a different set of choices with no language designer getting it perfect (IMHO);
including a specification within huge (5000+ page) book strains normal ideas about how to structure large programs;
and describing hardware requires us to borrow some of the (less familiar) syntax of hardware description languages because software languages are quite bad at describing bit-level manipulation.


--------------------------------

*This article is part of a series of articles about the design of the The Intel® ISA Specification Language.
For more articles, see*

- [Designing a language for readability]({{ site.baseurl }}{% post_url 2026-06-27-designing-a-language-for-readability %})
- [Naming conventions for readability]({{ site.baseurl }}{% post_url 2026-06-28-naming-conventions-for-readability %})
- [Designing syntax for readability]({{ site.baseurl }}{% post_url 2026-06-29-designing-syntax-for-readability %})
