---
layout: post
title: "Designing exceptions for readability"
author: Alastair Reid
tags: language, design, exceptions
---

I think that all the interesting behavior of an ISA lies in how it handles
problems such as illegal values (division by zero), access violations (e.g.,
illegal addresses), privilege violations, etc.  Designing an add instruction
(say) really isn't that difficult but it is a real challenge to correctly
handle an exception and, in particular cascades of exceptions (e.g., a stack
overflow generated while responding to an initial exception).

Different languages have taken different approaches to exceptions.

- In C, Pascal, etc. there is no built in support for exceptions and
  programmers use a variety of ad-hoc mechanisms to handle exceptions.
  This is usually quite error-prone (it is easy to forget that a function
  can generate an exception) and it can add a lot of clutter to the code.

- In C++, Javascript, etc. exceptions are built into the language
  and an exception is reliably propagated up the call stack to an exception
  handler.

  Unfortunately, this is still error-prone because it is often necessary to
  catch an exception, perform some local cleanup such as deallocating an
  object and then rethrow the exception.
  It is easy to forget that a function can throw an exception and that
  you need to add this catch/cleanup/rethrow code and it is particularly
  hard if a function that did not throw an exception is later modified
  so that it can throw an exception: you have to find all the functions that
  call the function and update them; then you have to find all the
  functions that call those functions and update them; ...

  It is worse for readers of the code. What looks like a straight-line 
  sequence of code that will be executed in its entirety could potentially
  be exited halfway through the sequence.
  Consider the difference between these two ways of pushing a value to the
  stack.

```
SP := SP - 4;
Memory::Write(SP, data); // write 'data' to stack
```

and

```
Memory::Write(SP-4, data); // write 'data' to stack
SP := SP - 4;
```

  They might look equivalent but, the first version always decrements the
  stack pointer whereas the second version only decrements the stack pointer
  if the memory write does not throw an exception.

- Java tries to fix the problem that exceptions are invisible by annotating
  the function definition with a list of all the exceptions that a
  function can raise including any exceptions that can be raised indirectly
  by calling another function.

  This helps you as you are writing code because you can easily
  see that a function that you are calling can throw an exception.
  But it is not very useful to the reader: they know that the function
  can raise some exception A but they may not spot all of the places
  in the function that can raise that exception.
  That is, it is not great for identifying and localizing all of the
  problem areas. 

- Functional languages like Haskell took a completely different approach
  from the conventional throw/catch mechanism found in C++, Java, Ada, etc.
  If a function is intended to return an Int (say), but it could fail for
  some reason, then the function return type would be `Maybe(Int)`[^haskell-parentheses]
  to indicate that it normally returns an Int but can exceptionally
  fail.

  When defining the function, successful return of a value 5 (say)
  is written `Just(5)` and a failed return is written `Nothing`.
  Any call to such a function needs to explicitly check whether
  the function returns `Just(...)` or `Nothing`.

  This probably sounds as if it adds a lot of clutter to your code
  but, thanks to a few helper functions and coding conventions, it
  tends not to be too bad and it has the enormous advantage that
  it is always clear whether a function could generate an exception or
  not.


[^haskell-parentheses]:
    In the sample Haskell code, I am making the syntax
    look more like conventional languages by adding some
    unnecessary parentheses to make it easier for readers who are unfamiliar
    with Haskell coding conventions.

- Rust and Swift have taken the Haskell approach to the next level by
  adding some syntactic sugar to make exception handling more convenient
  while retaining the advantages of making it clear exactly where an
  exception can occur and whether it is being handled locally or propagated
  to callers of the current function.

As the above sketch shows, exception mechanisms are tricky to design and the
most recent (and best) designs make it clear whether a function does or does
not throw an exception and make it easy to see which parts of your code
might call a function that raises an exception.

Since the state of the art is clearly the Rust/Swift approach, you
might expect us to adopt their approach.
We chose not to for two reasons

1. Rust and Swift are designed for building large, complex systems made
   up of libraries. In particular, the exception mechanisms are often
   used to use rich, complex exception handling within a library but
   then to catch those exceptions and present a simpler interface to
   users.

   The usage pattern in ISA specifications is usually much simpler:
   the response to an exception is generally to invoke a user-provided
   handler that processes a page fault, privilege violation, etc.
   and so nearly all exception catching is performed in one place:
   in the part of the specification that performs the instruction
   fetch-decode-execute loop.
   We therefore don't need all of the features provided in more general
   purpose languages.

2. Since Rust and Swift are so far ahead of other languages like
   C/C++ or Verilog, many of our expected readers will not have
   seen these exception handling mechanisms before so the
   advantage of having a concise notation must be balanced against
   the disadvantage that readers may not understand it or may
   misunderstand it.

### Exception markers

Based on all of the above, we decided that it was useful to indicate
to readers that calling a function could throw an exception but
we did not need to add any syntactic sugar to support handling
the exception at the call site.

Our initial attempt at achieving this was to add "exception markers" to
function calls and definitions that indicate that a function might not
return because it may throw an exception. We used a query symbol `?` to
indicate this uncertainty.
The same marker is applied both to the definition of the function

```
function Memory::Write8?(address : Bits(64), value : Bits(8));
```

and to calls to the function

```
Memory::Write8?(SP-1, data); // write 'data' to stack
SP := SP - 1;
```

We think that this provides a precise but subtle hint that additional control flow
could occur here without adding so much noise that it detracts from
understanding the overall flow.

As we were explaining this to other engineers though, we got a useful bit of feedback about code like this

```
if mod == 0b11 then
    Report_Illegal_Instruction?();
endif;

// rest of instruction here
```

Rich pointed out that it was useful to know that the function `Report_Illegal_Instruction` would never return
because then the reader knows that the rest of the instruction does not have to worry about the `mod == 0b11` case.
Based on this feedback, we tweaked the design to use two different exception markers.
For functions that *might* throw an exception but can return, we continue to use the `?` exception marker.
And, for functions that *always* throw an exception and never return, we use the `!` exception marker.
This changes the above code to look like this

```
if mod == 0b11 then
    Report_Illegal_Instruction!();
endif;

// rest of instruction here
```

and, of course, the definition of the `Report_Illegal_Instruction` function uses the matching `!` marker as well.


## Evaluation order

One other tricky feature of exceptions is their interaction with evaluation order.
For example, if the call `F?(x)` would generate one kind of exception and the call `G?(y)` would generate a different exception, the reader may wonder which exception would be generated by this expression

```
G?(y) + F?(x)
```

The conventional solution to this problem is to define that the evaluation order is strictly left-to-right.
The result is well-defined but it is a bit subtle and readers may
miss the importance of the order or an engineer may clean up the code
to read `F?(x) + G?(y)` without appreciating that this changes the meaning
of the code.

Of course, the problem is not just caused by exceptions.
If the function `F` modifies a global variable that the function `G` reads from, then the evaluation order would affect the behavior of the expression.

## Runtime checks

Any language that aims to be memory safe or robust will insert runtime checks to check for
issues such as exceeding array bounds.
Here are some of the checks we perform

| Where            | Check                   |
| ---------------- | ----------------------- |
| Divide/Remainder | Division by zero        |
| `/` and `%`      | Negative operands       |
| Array index      | Index out of bounds     |
| Bitslice         | Slice out of bounds     |
| Asserts          | Assertion failure       |
| Std::Unreachable | Executing this function |
| Case statement   | No pattern matched      |
| Assignment       | Integer is not in set   |
| Function call    | Integer is not in set   |
| Function return  | Integer is not in set   |

The first few checks are obvious but some of the others require some explanation

### Set type assignments

The most important runtime checks are probably the set type assignment checks
that are performed on assignments, function calls and function return.  If a
variable is declared with a set type such as `{8, 16, 32}` the set type assignment
checks check that the value assigned to the variable is one of the allowed values.

Note that these set type checks also handle many of the restrictions on standard
library functions such as requiring that left and right shift functions
don't shift by a negative distance. That is, `x << (-1)` is not allowed.


### Case behavior if no pattern is matched

The most surprising is probably the case statement check.
Consider what should happen in a case statement if none of the arms of the case
statement match. For example, consider what should happen if `size == 64`.

```isa
case size of
    when 8  => AL := Zeros(8);
    when 16 => AX := Zeros(16);
    when 32 => EAX := Zeros(32);
endcase
```

In many languages such as C, the default behavior if no arms match is to do nothing.
We think that this behavior can cause surprises: nothing in the code prompts readers
to think about the 64 case and it is also easy for authors to forget to consider a case.
We decided that it would be better if the default behavior was to report an error:
if the desired behavior is to do nothing, then the code should explicitly say so.

```isa
case size of
    when 8  => AL := Zeros(8);
    when 16 => AX := Zeros(16);
    when 32 => EAX := Zeros(32);
    when 64 => // do nothing
endcase
```

### Std::Unreachable

It is sometimes useful to emphasize to the reader that some condition cannot occur.
For example, in this function, the set type on `operand_size` is sufficient to
guarantee that the case statement will always match one of the first two arms.
But, in this case, we felt that the reader might not notice that it is impossible
and might waste time wondering what would happen so we add a call to `Std::Unreachable`
to make it clear that this case is impossible.

```
function CET::Check_Return_RIP?(operand_size : {16, 32, 64}, new_rip : Bits(64)) -> SSP_Type
begin
    case operand_size of
        when 16, 32 =>
            // 5 lines of code omitted
        when 64 =>
            // 5 lines of code omitted
        otherwise =>
            Std::Unreachable();
    endcase;
end
```

Logically, calling `Std::Unreachable();` is equivalent to `assert False;` but
the name indicates more clearly that this line should not be executed.
Also, having a dedicated function name makes it easier to generate good tooltip
messages in a website, easier to search for, easier to generate good error messages
when we test the specification, etc.

We have one other similar function that we are using while developing the specification.
We use this to indicate that we are aware that some functionality is missing which
is helpful when testing, debugging and reviewing the specification.

```
function Unimplemented_Feature(name : String);
```

Of course, this will not be called in the final specification but it is useful
during the multi-year process of developing a specification of the Intel Architecture.


### Runtime failure is not an exception

Languages with exception catching support usually make it possible to catch runtime
errors.
They do this because they want programs to be robust and to be able to recover
if an unexpected situation occurs.

We think that unexpected situations that are not handled safely indicate a gap or bug in the specification.
Attempting to recover would be the wrong thing.
Instead, use of the specification should halt and the user should report the issue
so that the specifier can clarify what should happen in that situation and issue a fix for the specification.

For this reason, we do not regard failure of a runtime check to be an exception.
It cannot be caught; it is always fatal.
Since runtime failure should halt use of the specification, we do not have the problem
seen with exceptions of hidden control flow so there is no need for exception markers
to help the reader.


## Summary

ISA specifications have a lot of privilege / access / virtualization checks
that trigger exceptions if violated.  For example, memory protection, accesses
to privileged registers, executing privileged instructions, accessing
virtualized assets, etc.
To support this, our language provides conventional exception throwing/propagation/catching
mechanisms.

Code that throws a lot of exceptions is hard to reason about because the
exception propagation mechanism introduces invisible control flow: what looks
like a straight-line sequence of operations could be terminated halfway through
if an exception is thrown.
To make it easier to reason about this, we require function calls to be decorated
with exception markers to provide an indication of this invisible control flow.
There are two types of exception marker: `?` indicates that a function could
throw an exception; and `!` indicates that a function always throws an exception
(i.e., it never returns).
Function declarations are also decorated with exception markers; the markers on
function calls and declarations are required to match; and the compiler checks that
function definitions have correct markers.

We treat runtime checks quite differently.
Runtime checks are used to enforce set type annotations, memory safety, bitvector slicing,
division by zero, and user assertions.
While many languages treat runtime failures as a kind of exception (because this improves
software robustness), we treat runtime failures as an error that indicates that the
specification has a problem and cannot be relied on to define the processor behavior.
Instead of allowing runtime failures to be caught, we encourage users to report
the failure as a bug so that the specification can be fixed.


--------------------------------

*This article is part of a series of articles about the design of the The Intel® ISA Specification Language.
For more articles, see*

- [Designing a language for readability]({{ site.baseurl }}{% post_url 2026-06-27-designing-a-language-for-readability %})
- [Naming conventions for readability]({{ site.baseurl }}{% post_url 2026-06-28-naming-conventions-for-readability %})
- [Designing syntax for readability]({{ site.baseurl }}{% post_url 2026-06-29-designing-syntax-for-readability %})
- [Designing types for readability]({{ site.baseurl }}{% post_url 2026-06-30-designing-types-for-readability %}).
- [Designing exceptions for readability]({{ site.baseurl }}{% post_url 2026-07-02-designing-exceptions-for-readability %}).
