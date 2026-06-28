---
layout: post
title: "Designing a language for readability"
author: Alastair Reid
tags: [language, design]
---

Most programming languages are designed to support people writing code and so
most languages provide features that make *programming* easier.  More
importantly, the language can make assumptions about people writing code in that
language: that they understand the language and have / will consult language
documentation when they need to.


An ISA specification language has a lot in common with a programming language
but the focus is on making *reading and understanding* the specification
easier with the expectation that most users will never write code
in the specification language.
They are almost certainly programmers but their daily programming
experience could be any on of assembly language, C/C++, Verilog, Rust, Swift,
Javascript, LISP, ... 

Despite users' diverse language experience,
despite the likelihood that they will never write a single line of code in our language,
and despite the fact that most users will not even realize that the language even has any documentation much less read it,
we want readers to be able to easily and accurately understand specifications written in this language.

So the language design task is a bit different from normal language design.
The language must be clear and precise but it also has to be recognizable
and understandable by typical programmers.
This rules out mathematical languages like [Rocq](https://rocq-prover.org/docs/tour-of-rocq) or [Alloy](https://practicalalloy.github.io) that require users to find the time and interest to learn the language.

Assuming a diverse range of programming backgrounds makes the choice of notation harder because programmers will come to our language with assumptions about what certain symbols mean.
Does '=' mean assignment or test for equality?
Does ["1 << 2 + 3"](https://rust-lang.github.io/rust-clippy/master/index.html#precedence)
mean "1 << (2 + 3)" or "(1 << 2) + 3"?
What order are expressions evaluated in?
What is the result of adding a 32-bit signed number to a 64-bit unsigned number?
Is the remainder of dividing "-15" by "4" equal to "-3" or "+1" or some other value?
If a reader of a specification needs to be able to answer any of these
questions, then we have failed in our design task.


One of the primary uses of the language is for use in documentation by people who will never run the specification in their life
but we also have to consider some [non-documentation uses of the language](https://alastairreid.github.io/uses-for-isa-specs/)).
The specifications must be executable;
we need to be able to test specifications;
we need to be able to use specifications in test harnesses, formal verification flows, simulators, etc.;
and we will want to benefit from AI-based code review tools like Copilot
as we develop the specification.


Moreover, although we write specifications for clarity instead of performance, we cannot completely ignore performance.
My first encounter of an executable ISA specification language was as a compiler
engineer working on a vectorizing compiler.
The vector architecture had an executable specification that could automatically generate a simulator.
Unfortunately, the simulator ran at about 2,000 instructions per second which meant that a typical use such as running a few hundred regression tests of just 10,000 instructions each took an annoying 15 minutes.
As we added more tests, we had to tune our benchmarking process to avoid
blowing up the testing time and
running a full SPEC benchmark was completely out of the question.
The problem was that every arithmetic operation was converted to a
call to a rather low performance arithmetic library that dynamically
allocated heap space to store the result. So a trivial calculation like
"1 + 2" was thousands of times slower than native execution.
We need to do better than this.



## Goals

The goals of the language design include a lot of things not to do:

- avoid sources of confusion found in other languages
- avoid sources of confusion caused by differences between languages
- avoid error-prone language features
- avoid the various gotchas and [footguns](https://en.wiktionary.org/wiki/footgun) found in languages like C and C++
- avoid language lawyers: you should not need a detailed understanding of the language to understand specifications correctly
- avoid making cultural assumptions (to the extent that we understand our own cultural biases)

And some things that we specifically want the language to be good for

- describing the bit-level manipulations that occur in ISA
  specifications

- the repeatable, deterministic parts of most instructions

- supporting deliberate *under-specification* of ISA features that support
  a range of different behaviors. For example, some instructions might
  leave the status flags in an unspecified state for certain input values: perhaps 0 on one
  processor and 1 on another processor.


## Non-goals

There are many parts to an ISA specification and each one is best served by a
different notation.

For example, it is certainly possible to describe the fields of a register using a
record/struct-like syntax that provides names for each slice of the register.
But we have found that it is better to describe registers using a data format
such as JSON or XML and then use the data to generate documentation, diagrams,
tables, code, etc.
A data format is simpler and more flexible.
And it provides a simple, obvious way to add meta-information about
a field or register such as which version of the architecture the field was
introduced in, the meaning of different bit-patterns, reserved values, etc.

Another example is (weak) memory models.
Memory models can introduce a large amount of non-determinism
over the order of memory accesses that researchers have found is best handled by specialized
notations such as axiomatic specifications that compactly capture the
ordering constraints and support reasoning about the range of
possible behaviors.

Another important non-goal is that we are not aiming to make the language concise.
Our experience with using some very expressive languages such as Haskell or APL is that the more concise your coding style, the longer it takes somebody else to understand what your code means.
Minimizing the number of strokes required should be left for the [golf course](https://code.golf).


## Language design guidance

People have been designing programming languages for over 70 years and
a lot has been written about the strengths and weaknesses of those
languages.
How did the intended use of the language affect the language design?
What did they get right?
What mistakes in other languages were they trying to avoid?
What did they get wrong?
etc.

The most obvious place to look for ideas is the descriptions of programming languages.
How did they solve particular problems?
What syntax or naming conventions did they use?

Languages that follow a community design process such as [Python](https://peps.python.org/pep-0001/)
also have extensive records of individual design choices, the problem they wanted to solve, the alternatives considered and individual pros and cons, etc.
These are an incredible resource for any budding language designer.
(However, these generally only record proposed changes to an original
base design so they may not cover the fundamental design decisions in such depth.)

The Ada language had a lengthy public consultation process with a number of
language proposals and revisions. In particular, the
[Rationale for the Design of the Ada Programming Language](http://archive.adaic.com/standards/83rat/html/Welcome.html)
contains detailed discussion of
the fundamental choices of the language and how they relate to the design goals.

Sometimes the original language designers write about the goals and design decisions of the language.
Some of this will appear in the original language description, and similar
contemporaneous descriptions of the language.
But, in the case of serial language designer Niklaus Wirth, the best place to learn
what he did not like about a language design is to look at what he wrote
when describing his next language: what had he learned from using the previous language;
what could he not do with the previous language; what had he got wrong?

And, finally, a great source of information on language design is
the [History of Programming Languages conference](https://en.wikipedia.org/wiki/History_of_Programming_Languages_(conference)).
This contains retrospectives by the designers of many of the most influential languages
talking about the design process, their goals, the history of the projects and
what design decisions they think are the most important to the success of their language.

## Algol heritage

Many languages are said to be based on the Algol tradition
so we can benefit from the familiarity that this brings.
But I don't think anybody ever says what they think the Algol tradition is
and which bits of Algol they chose to adopt or reject.

Some features of Algol compared with other languages from that time
such as COBOL and FORTRAN are so ingrained in language design that they
are hardly worth mentioning: separating lexical syntax from the language grammar, use of reserved words, etc.

Algol also had more emphasis on types than other languages of its time and the syntax "x : T" (meaning "x has type T") has been adopted in so many languages that it is the obvious syntax to use.
We will adopt both the syntax and the spirit of Algol types:
using static typechecking to rule out a broad class of potential bugs
and using types as checkable documentation to guide the reader in
understanding the specification.

Algol also introduced hierarchial block structure: providing 'if-then-else',
'begin-end', etc. instead of relying on 'goto' to specify control flow.
Many refinements of this approach have been explored in later languages.

## Expressivity

Modern programming languages have developed a broad range of language
features to make programming more expressive so that you can get more
done with less code and less repetition.

Some of the more popular are object-oriented classes,
parametric type polymorphism, ad-hoc type polymorphism,
generic types, type classes, higher-order functions,
anonymous functions (aka 'lambdas'), ...

Talking about the [Sail ISA specification language](https://github.com/rems-project/sail/blob/sail2/README.md)
[Prof. Peter Sewell](https://www.cl.cam.ac.uk/~pes20/)
argues that ISA specification languages should be as small and inexpressive as possible.
His motivation is that we want to be able to use ISA specifications for
[as many different uses as possible](https://alastairreid.github.io/uses-for-isa-specs/)
and that requires us to convert the specifications to other languages.
The more features that our specification language has, the harder
this conversion process will be resulting not just in greater effort
to write the conversion tool (and, since implementation effort is limited,
less tools) but, in general, worse results and more
potential conversion bugs.

So, like Sail, we chose to keep our language small and inexpressive:
reducing the effort required to implement the language,
and the effort for readers to understand the language
while maximizing the number of languages into which we can convert
our specifications.

However, like Sail, we did include a few features that greatly
improve readability of the specification but that cause us problems
when compiling the code.

--------------------------------

*This article is part of a series of articles about the design of the The Intel® ISA Specification Language.
For more articles, see*

- [Designing a language for readability]({{ site.baseurl }}{% post_url 2026-06-27-designing-a-language-for-readability %})
- [Naming conventions for readability]({{ site.baseurl }}{% post_url 2026-06-28-naming-conventions-for-readability %})
