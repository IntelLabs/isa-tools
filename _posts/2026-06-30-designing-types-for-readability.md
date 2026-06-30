---
layout: post
title: "Designing a type system for readability"
author: Alastair Reid
tags: language, design, types
---

Types serve several purposes in a programming language:

- in C, types guide allocation of storage;

- in Java, Rust, ML, Haskell, etc. types eliminate large classes of programming errors by reliably detecting errors;

- in almost all languages, types support operator overloading so that you can use the '+' symbol to mean integer addition, floating point addition, etc.;

- in some languages, types support function/method overloading so that you can define functions with the same name and rely on the type system to disambiguate function calls either at compile-time or at run-time;

- and types provide checkable documentation: knowing that a variable is declared as an 8-bit number or a 64-bit number can help the reader understand the code.


Given our goals of supporting the creation of clear, reliable documentation with
minimal potential for confusion, it should be no surprise that we
chose to eliminate large classes of errors and provide checkable documentation by strictly enforcing types: if a variable is declared as having a given type then only values of that type can be assigned to it.

We chose not to support overloading of functions based on types because ISA
specifications are generally published in large documents of thousands of pages
and PDF search facilities are usually quite bad so it is a good idea that if a
reader searches for a function then whatever definition they find actually
matches the function that they are looking for.

However, we do support overloading of builtin operators like "+" and "<=" because
the alternative (seen in some ISA specifications) is to use operator names like "+f" (for floating point addition), "+i" (for integer addition), etc.
We think that requiring explicit disambiguation of builtin operators
adds a lot of clutter without adding much clarity.

*[This article is part of a series of articles about the design of the The Intel® ISA Specification Language.
For more articles, see 
[#1]({{ site.baseurl }}{% post_url 2026-06-27-designing-a-language-for-readability %}),
[#2]({{ site.baseurl }}{% post_url 2026-06-28-naming-conventions-for-readability %}),
[#3]({{ site.baseurl }}{% post_url 2026-06-29-designing-syntax-for-readability %}),
[#4]({{ site.baseurl }}{% post_url 2026-06-30-designing-types-for-readability %}),
[#5]({{ site.baseurl }}{% post_url 2026-07-02-designing-exceptions-for-readability %}).
]*


### Integers and set types

To enhance the documentation value of types, we chose to make them more expressive by allowing types to express sets of possible values like "{ 8, 16, 32, 64 }" and ranges of values like "{ 0 .. 31 }". This approach dates back at least as far as Pascal (that could use the type information to eliminate most array bound checks).

We perform most typechecking at compile-time.  For example, we check that
integers are not used as booleans, that a field dereference like "x.f" is only
performed if "x" has a type that has a field "f", and that 32-bit values are
not used where 64-bit values (say) are required.  However, following Pascal, we
dynamically check that values are members of sets at runtime just as we
dynamically check for division by zero, that array indexes are within bounds,
that assertions are satisfied, etc.  at runtime.

In practice, normal compiler optimizations result in almost all checks being performed at compile time
and, in the future, we plan to have the compiler generate a list of all
the checks that it was not able to optimize to support review of the 
type annotations (do they need to be strengthened or weakened?) and
improvement of the optimizations.

Since we test our specifications thoroughly, the effect is similar to
performing a static compile-time check in that readers can trust that the set
types are accurate.


The [Sail ISA specification language](https://github.com/rems-project/sail)
takes a different approach here.
Like us, they support set types but, unlike us, they choose to perform
all typechecks statically using a system based on [Liquid types](https://patrickrondon.com/research/papers/liquid-types-pldi08.pdf).
This is a more sophisticated flow-based typesystem
that tracks the constraints on values
through all the different control-flow paths in a program.
For example, when typechecking "if i < 16 then x[i] := 0; endif", a flow-based
typechecker would use the knowledge that "i < 16" when statically checking that "x[i]" is legal
while a conventional *flow-insensitive* typechecker would ignore that information
and would be unable to check the array index.

We decided not to go with this approach for several reasons.
The first is that we were not sure whether we would be able to statically check
100% of our code or would we need to add hints to help the typechecker.
For example, consider a loop that counts the number of set bits in a 32-bit
bitvector `x`

```
1  var set_bits : { 0 .. 32 } := 0;
2  for i := 0 to 31 do
3      if x[i] == 0b1 then
4          set_bits := set_bits + 1;
5      endif;
6  endfor;
```

For a human, it is blindingly obvious that the assignment to `set_bits` on line 4 is
correct. Clearly, `set_bits : {0 .. i}` at the start of the loop and `i : {0..31}`
so `set_bits + 1 : {0..32}`.
But anyone who has tried to formally verify code like this knows that making this
obvious fact clear to a verifier requires you to write loop invariants, show that
the loop invariant is preserved by the loop, etc.
Any annotations used to help the typechecker to statically check the code (such as
adding a loop invariant) would not obviously make the specification easier to
understand and would add confusing clutter to the specification.
Alternatively, strengthening the typechecker to attempt to infer simple invariants
would likely result in a brittle language where insignificant looking changes are
enough to change whether the code typechecks or not.

In short, we felt that requiring static typechecking of set types
would shift the balance from writing code for humans to read to
writing code to help our tools.

A second, but equally important reason for keeping things simple
is that we are thinking about future type extensions that we might want to add.
For example, it might be useful to be able to write richer type constraints such as
"x is a power of 2" or "x + y < 16".
We did not want to box ourselves into only being able to add an extension
if we could reason 100% statically about it.

We plan to revisit this decision in the future and consider whether we should follow Sail's approach. This will be easier now that we have a large body of code to experiment on and it should not require much/any change to the code since we already have set types throughout our code.

It is worth noting that being able to declare types like "{0 .. 255}" means that there is
no need to follow C's example of defining types like "uint8_t", "int64_t", etc.
to control storage sizes
since the compiler has plenty of information to make that decision.


### Bitvector types and bitwidth-polymorphism

All the registers, memory interfaces, etc. used in an ISA are defined
in terms of fixed-width bitvectors --- typically 32 or 64 bits.
Many of the instruction encodings registers have fields that might be 1 bit,
2 bits, 5 bits, 48 bits, etc.
Types like this are found throughout an ISA specification so we
make it possible to talk about bitvector sizes
directly so we provide sized bitvector types like "Bits(1)", "Bits(2)", ... "Bits(93)", ...

Many of the functions that operate on bitvectors are specific to particular bitwidths. For example, functions that operate on addresses might be specific to "Bits(64)".
But quite a lot of functions can operate on multiple bitwidths.
This is true of standard bitvector operators like addition and bitslicing
and of basic utility functions like bit reversal that can operate on
any bitwidth and it is true of more complex functions like floating point addition that can operate on any width in "{16, 32, 64}" (say).

The Pascal language demonstrated that it is really irritating to have to define multiple versions of the same function that differ only in data size.
The C language responded to the same problem by not reasoning about the sizes of arrays (and programmers have paid the cost ever since).
But most subsequent languages have supported ways of defining functions that are parameterized by a type or value.
For example, Java generics, C++ templates, ML parametric polymorphism, etc.

In that spirit, we allow functions to be parameterized by a size argument.
For example, we can define a bit reversal function like this

```
function Bit::Reverse(size : {0..}, x : Bits(size)) -> Bits(size)
begin
    var r : Bits(size);
    for i := 0 to size-1 do
        r[i] := x[size-1 - i];
    endfor;
    return r;
end
```

or we can declare a floating point addition function like this

```
function FP::Add(size : {16, 32, 64}, left : Bits(size), right : Bits(size)) -> Bits(size);
```

### Type inference

The first languages that provided strict typechecking tended to require a lot
of annotation: every variable had to be annotated with its type, every generic
function had to be annotated with the type parameters.  This adds quite a lot
of clutter that detracts from the readability of the specification by burying
the code in repetitive detail.

Later languages like ML, Haskell, Rust, etc. have preserved strict typechecking
but avoided it getting in the way by automatically inferring most of the types
found in the code.
Following their experience, we decided that, when declaring a
local variable with an initializer, there is no need to annotate the variable
with its type since the initializer usually makes that clear.  For example, if
"left : Bits(size)" and "right : Bits(size)" then, this statement is clearly
defining a variable "result : Bits(size)" and there is no need to add a type
annotation.

```
let result := left and right;
```

Following best practice in Haskell, we decided that global variables and
function headers should be fully annotated with their types since the
types of functions provide useful documentation of what a function does
and how it will be used.

Another common source of redundancy is demonstrated in calls to the functions
`Bit::Reverse` and `FP::Add` defined above.  When using `Bit::Reverse` to
reverse a 32-bit value in `x` (say), you have to provide a size argument and
write `Bit::Reverse(32, x)`.  But the size argument `32` is redundant because
the reader already knows that `x` is 32-bits wide and the compiler could easily
infer that value from the size of the `x` argument.

This inference of missing arguments has been supported by languages
like ML and Haskell for a long time and works quite simply:
the typechecker fills in missing arguments that can be inferred from
the types of the other arguments.
In ML and Haskell, only type arguments can be inferred and
the convention is that you don't have to explicitly declare
these type arguments in the function header: they are implicitly assumed
to exist.
Initially, we followed a similar approach of omitting the implicit arguments
and we wrote the header of the Bit::Reverse function like this.

```
function Bit::Reverse(x : Bits(size)) -> Bits(size);
function FP::Add(left : Bits(size), right : Bits(size)) -> Bits(size);
```

But reviewers of our specification found this confusing because they did not
know where the `size` variable came from or what its type was.
And, the lack of a declaration limited the language because
we had to treat all size variables as having the same type because there was no way
to give more specific information such as `size : {0..}` or `size : {16, 32, 64}`.

Inspired by the [Rocq theorem prover's approach to implicit
arguments](https://rocq-prover.org/doc/v8.13/refman/language/extensions/implicit-arguments.html),
we chose to require programmers to declare all arguments in the definition of a
function but to indicate which arguments can be omitted in calls to the
function.
That is, the actual headers for these funcitons look like this

```
function Bit::Reverse(implicit size : {0..}, x : Bits(size)) -> Bits(size);

function FP::Add(implicit size : {16, 32, 64}, left : Bits(size), right : Bits(size)) -> Bits(size);
```

These implicit arguments can be used whenever the compiler can fill in the
value of the argument by looking at the sizes of the other arguments.  This
lets the person writing the specification choose how verbose they want the
specification to be: they can provide size arguments in circumstances where
they seem helpful but omit them when not useful.


### Avoiding the complexity of implicit type promotions

A recurring source of confusion and error in the C and C++ languages are the
rules for what happens when you add two integers with different sizes or
signedness.  For example, you might think that unsigned zero is greater than negative 1.
That is, you might believe that this expression is true.

```
0u > -1
```

However, [this expression is
false](https://speytech.com/insights/type-promotion-trap/) because C has
complex and sometimes surprising rules about what happens when you combine
signed and unsigned values or integers of different size.  In this case, the
value "-1" is promoted to an unsigned integer so the expression is equivalent
to

```
0u > 4294967295u
```

which is, of course, false.

We don't want readers to be confused or surprised by an expression.
We don't want readers to have to ask an expert to explain what an expression means.
We don't want people to write blog articles about the complex integer promotion rules.

To avoid the problem that C faces, we adopt the approach taken in Modula-2,
Haskell and a host of other languages of not supporting implicit type
promotions.  If two values have the same type, then you can add them, xor them,
etc. without problem.  But, if two values have different types, then you need
to explicitly convert one of them to the type of the other before combining
them.  For example, if "left : Bits(8)" and "right : Bits(64)" and you want to
add them, then your main choices are

```
Zero_Extend(left, 64) + right // fill in missing bits with zero bits
Sign_Extend(left, 64) + right // fill in missing bits with sign bits
left + right[7:0]             // shorten the excess bits in 'right'
```

Requiring the programmer to make this explicit helps the reader because they
don't have to guess what happens: they can see it in the code.

Note that one consequence of this strict typing rule is that
the reader doesn't even have to know whether or not there
are integer promotion rules in the language because no correctly typed program
would ever combine values of different types.


## Summary

Types serve many purposes in a programming language.
We have chosen to focus on the use of types to detect errors and
to act as checkable documentation that readers can trust the accuracy of.
To support this, we have added
set types like `{8, 16, 32}`,
sized bitvector types (to represent many of the values found in an ISA specification),
a limited form of bitwidth polymorphism (to enable code reuse),
and
type inference (to avoid cluttering the specification with too much type information).
And we have chosen to omit error-prone and confusing features like C's implicit type promotion
and distinguishing signed and unsigned types.


--------------------------------

*This article is part of a series of articles about the design of the The Intel® ISA Specification Language.
For more articles, see*

- [Designing a language for readability]({{ site.baseurl }}{% post_url 2026-06-27-designing-a-language-for-readability %})
- [Naming conventions for readability]({{ site.baseurl }}{% post_url 2026-06-28-naming-conventions-for-readability %})
- [Designing syntax for readability]({{ site.baseurl }}{% post_url 2026-06-29-designing-syntax-for-readability %})
- [Designing types for readability]({{ site.baseurl }}{% post_url 2026-06-30-designing-types-for-readability %}).
- [Designing exceptions for readability]({{ site.baseurl }}{% post_url 2026-07-02-designing-exceptions-for-readability %}).
