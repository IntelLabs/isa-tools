---
layout: post
title: "Naming conventions for readability"
author: Alastair Reid
tags: [language, design]
---

## Camels, snakes and naming conventions

Many language design choices trigger so-called "religious wars": disputes over trifling points like indentation level, choice of editor, and the correct way to capitalize an identifier made up of multiple words.
Some of these choices have very obvious consequences and aligning the choice with the your goals is easy.
But, sometimes, you need a lot of examples to really understand that you have made the wrong choice.

We wrote most of our specification of the IA architecture (aka x86) using 
the [CamelCase](https://en.wikipedia.org/wiki/Camel_case)
capitalization rule.
For example, we had names like `CheckWritePermission` and `LoadSegment`.
These names seemed to work well.
The capitalization made it obvious how to split the name into separate words.

But, as we wrote more of the specification, we found that some function names naturally contained acronyms like `EAX`, `IO`, `TLB`, etc.
and names like `ReadEAX`, `CheckIOBitmap` and `InvalidateTLBByAddress`
did not work so well --- it took more effort for readers to split the names into words.

The standard advice for using acronyms with CamelCase is to only abbreviate the
first letter and we did this for a while.  But, while it is easy to split names
like `Read_Eax`, `CheckIoBitmap` and `InvalidateTlbByAddress` into words, it
still takes work for the reader to realize that `Tlb` or `Io` may be an acronym
and writing `Eax` breaks the strong convention that register names are written
in ALL_CAPS.

In retrospect, it took us a ridiculously long time to make the change but,
eventually, we added underscores to the names of globals such as functions,
variables and types and we renamed everything accordingly:
`Check_Write_Permission`, `Load_Segment`, `Read_EAX`, `Check_IO_Bitmap`,
`Invalidate_TLB_By_Address`.


## Local and global names

We made a subtly different choice for local variable names.
One of the tricks that I learned from Haskell was to use capitalization to indicate
the scope of a name.

- Names that start with upper-case letters are used for global names.
- Names that start with lower-case letters are used for local names.

This rule is (more or less) used in Haskell to handle some parsing ambiguities but it always
seemed to me like a helpful rule to give a subliminal hint to readers about
the scope of a variable and so I chose to adopt the same rule.
Unlike Haskell, the rule is not essential to parsing code so the rule
is not enforced by the compiler but, instead, it is demonstrated in
all code examples, in the standard library, in the specifications that
we write, in our style guide, and it is enforced during code review.
Our compiler does not enforce the rule but it supports review by
generating lists of all local and global variables in a specification
and we occasionally check that no miscapitalizations have snuck past the reviewers.

Many readers will not notice that the rule is being applied but, for those
that do, it gives a little extra hint about the scope of a variable.


## Namespaces

The idea of namespaces is, in part, a response to the growing size of our
specification and, in part, borrowing the structuring elements found in popular
languages.

As the specification we were working on grew, we started to use naming
conventions to group related functions and variables.  For example, we added an
`FP` prefix to floating point support functions and a `Cache` suffix to cache
management functions.

Although both prefixes and suffixes initially seem like equally good choices,
using a consistent prefix for related functions has the advantage that an
alphabetical sort of their names (e.g., in an index) groups related functions
so we prefer using a prefix to using a suffix.

Other languages such as C++, Rust, etc. have adopted the idea of using multiple
nested namespaces to structure large bodies of code.  For example, in C++ you
can have names such as `std::cout` that are in the `std` namespace.  This
notation inspired us to add a limited form of namespace support to our language
by allowing identifiers to contain double colons. That is, allowing names like
`FP::Add`, and `Cache::Flush`.

This change had most impact on the standard library of the language resulting in names such as `Std::Bits::Zeros`, 
`Std::Bits::Align_Up`,
`Std::Integer::Log2`,
and `Std::Integer::Align_Up`.
This both helped distinguish the standard library names from user defined names and solved the problem of how to disambiguate corresponding functions on bitvectors and on integers such as alignment functions.

We also copied C++ and Rust in providing a way to remove the namespace prefix
when not needed.  For example, you can write

```
use Std::Integer::Log2 as Log2;
```

This mechanism should be used with care though.  ISA specifications are often
included in large PDF documents that are thousands of pages long and it is
unrealistic to expect a reader to have noticed that page 1337 (say) of the
document happens to remove the namespace for some function.  So, we recommend
that namespaces are used consistently within a single specification and the
'use' mechanism only used at the boundary between specifications.

This restriction is slightly less important when rendering the specification in
web pages because we can freely use tooltips to display the full name of a
function when the mouse hovers over a function call.


## Avoiding abbreviations

One issue that affects both the language design and the way that we write
specifications concerns abbreviations.

If you are a native English speaker who is already familiar with the
architecture being specified, then abbreviated names like `SExt`, `Rev`, `LIP`,
`fn`, etc. may make complete sense to you.  But if you only occasionally deal
with the architecture or if English is your second language (or, perhaps, your
third or fourth), then you might be better served with names like
`Sign_Extend`, `Reverse`, `Logical_IP`, and `function`.  Even if you don't know
a word, having the word spelt out in full means that you can look it up in a
dictionary or on the internet.[^CERN]

[^CERN]:
    Some years ago, I had a summer internship at [CERN](https://home.cern)
    working on a codebase written by native French speakers and
    I was constantly thrown by the abbreviated variable names in
    their code because my schoolboy French was not good enough to realize
    that `Vp` probably meant "battery voltage" (because the French word
    for "battery" is "pile").
    If there was nobody around to ask, I had to reverse engineer the
    meaning of the code from first principles.

So we decided to avoid abbreviating words
in keywords (e.g., function definitions start with the keyword `function` instead of `fn` or `func`);
in standard library names (e.g., we use `Rotate_Left` instead of `RotateL`, `Rol`, etc.);
and in the specifications we write (e.g., we write `AES::Substitute_Byte` instead of `AES::Sub_Byte`).

This takes a little more effort to write because names are longer but, more
significantly, because we have to break the habits of a lifetime.  But this is
worthwhile because it makes it easier for readers to understand the
specification.

(The sharp eyed reader will note that we break the general rule of avoiding
abbreviations for the `Std` namespace. We cut ourselves a little slack for
names whose abbreviations are extremely common in software, unlikely to cause
confusion, and that occur frequently within our specification.)

## Summary

Naming conventions are an important part of a language:
improving readability, consistency and structure.
They are especially important for projects like writing an ISA
specification that are typically written and maintained
by many different engineers over a period of decades and
that are used in documentation.

Our conventions avoid abbreviation,
use capitalization and underscores to make it easier to read long names,
use lower-case to subliminally distinguish local variables from globals,
and use namespaces to group related names.

--------------------------------

*This article is part of a series of articles about the design of the The Intel® ISA Specification Language.
For more articles, see*

- [Designing a language for readability]({{ site.baseurl }}{% post_url 2026-06-27-designing-a-language-for-readability %})
- [Naming conventions for readability]({{ site.baseurl }}{% post_url 2026-06-28-naming-conventions-for-readability %})
- [Designing syntax for readability]({{ site.baseurl }}{% post_url 2026-06-29-designing-syntax-for-readability %})
- [Designing types for readability]({{ site.baseurl }}{% post_url 2026-06-30-designing-types-for-readability %}).
- [Designing exceptions for readability]({{ site.baseurl }}{% post_url 2026-07-02-designing-exceptions-for-readability %}).
