---
layout: post
title: "Designing functions for readability"
author: Alastair Reid
tags: language, design, functions, uniform access principle
---

Functions provide the major abstraction mechanism in our language: enabling us
to define a simple API into a potentially complex part of the architecture.
Functions are also the main mechanism for code reuse.

*[This article is part of a series of articles about the design of the The Intel® ISA Specification Language.
For more articles, see 
[#1]({{ site.baseurl }}{% post_url 2026-06-27-designing-a-language-for-readability %}),
[#2]({{ site.baseurl }}{% post_url 2026-06-28-naming-conventions-for-readability %}),
[#3]({{ site.baseurl }}{% post_url 2026-06-29-designing-syntax-for-readability %}),
[#4]({{ site.baseurl }}{% post_url 2026-06-30-designing-types-for-readability %}),
[#5]({{ site.baseurl }}{% post_url 2026-07-02-designing-exceptions-for-readability %}),
[#6]({{ site.baseurl }}{% post_url 2026-07-03-designing-functions-for-readability %}).
]*

## The challenge of specifying registers

One of the challenges in ISA specification is how to specify registers.
At first sight, this might seem like a simple task --- just use global variables:
bitvectors for individual registers and arrays of bitvectors for register files.
However, a closer look reveals that there are more registers than you might
immediately think, the typical  register has more complexity than you might imagine,
and there are quite a lot of atypical registers.

Here are a handful of issues that are encountered in different ISAs.

Sticky bits
: Many registers have 'sticky bits': bits that always read as a 0 or a 1 no matter
  what value is written to the bit.

  - In the Intel Architecture, the RFLAGS register has a number of sticky bits: bit 1 is always 1,
    bits 3, 5, 15 and 63:22 are always zero, ...

  - In the RISC-V Architecture, the bottom bit of the program counter is always zero.

  - Reserved bits in control and status registers are often hardwired to zero
    but may become modifiable in the future if the bit is allocated for some purpose.

  - CPUID registers that are used to read what features the processor supports are
    almost always constant. (But, to support virtualization, a few can be written in
    privileged modes.)

Irregular register files
: Many RISC architectures treat their first general purpose register as a constant:
  writes to the zero register are ignored and reads always return zero.

Mode dependent and disabled bits
: Control and status registers often cannot be modified in low privilege modes, if
  the feature that they control/report is disabled, or when a hypervisor is controlling
  that feature.
  That is, they are conditionally sticky.

Aliasing
: Within the Intel Architecture, there are many register aliases such as

  | Register      | Is an alias of  |
  | ------------- | --------------- |
  | `AL`          | `RAX[7:0]`      |
  | `AH`          | `RAX[15:8]`     |
  | `AX`          | `RAX[15:0]`     |
  | `EAX`         | `RAX[31:0]`     |
  | ...           |                 |
  | `RAX`         | `GPR[0]`        |
  | `RBX`         | `GPR[3]`        |
  | `RCX`         | `GPR[1]`        |
  | `RDX`         | `GPR[2]`        |
  | `RSP`         | `GPR[4]`        |
  | `RBP`         | `GPR[5]`        |
  | `RSI`         | `GPR[6]`        |
  | `RDI`         | `GPR[7]`        |
  | ...           |                 |
  | `XMM[i]`      | `ZMM[i][127:0]` |
  | `YMM[i]`      | `ZMM[i][255:0]` |

  While it would be possible to write `GPR[0][7:0]` (say) everywhere that we need to refer to `AL`,
  the specification would be a lot less readable if we did so.
  It would also create a confusing mismatch between the assembly syntax (where these aliases are heavily used)
  and the executable specification.
  And, while the reader may guess that `RAX` is `GPR[0]`, it would be more challenging for readers to guess,
  for example, that `RDI` is `GPR[7]` or that `RBX` is `GPR[3]`.[^register-numbering]

[^register-numbering]:
    For more on the history of register numbering in the Intel Architecture, see Ken Shirriff's fascinating
    [deep dive into the history of early Intel processors](https://www.righto.com/2023/08/datapoint-to-8086.html).

In short, while registers and register files initially seem to be just plain old storage that can be specified
entirely using global variables, many registers require some actual computation to implement
their unique features.

The most obvious way to achieve that would be to define a pair of functions to access each register.
For example, for the AL register, we could define

```isa
var _RFLAGS : RFLAGS_Type; // storage for RFLAGS register

function Get_RFLAGS() -> RFLAGS_Type
begin
    var result := _RFLAGS;

    // set/clear sticky bits
    result[1] := 0b1;
    result[3] := 0b0;
    result[5] := 0b0;
    result[15] := 0b0;
    result[63:22] := Zero(42);

    return result;
end

function Set_RFLAGS(value : RFLAGS_Type)
begin
    var rflags := value;

    // set/clear sticky bits
    rflags[1] := 0b1;
    rflags[3] := 0b0;
    rflags[5] := 0b0;
    rflags[15] := 0b0;
    rflags[63:22] := Zero(42);

    _RFLAGS := rflags;
end
```

There are two problems with following this obvious approach

- The first is that, although the model of registers as just plain old storage is not entirely
  accurate, it is a good first approximation for readers to rely on because it captures the
  gist of the architecture without getting bogged down in details.

- The second problem is that using these get/set functions adds a lot of clutter to the specification.
  Using this approach, the ADD instruction looks like this

  ```isa
  let (result, carry_out, overflow, alternative_carry) := Add(src1, src2);

  var rflags := Get_RFLAGS();
  rflags.OF := overflow;
  rflags.SF := result[operand_size-1];
  rflags.ZF := if Is_Zero(result) then 0b1 else 0b0;
  rflags.AF := alternative_carry;
  rflags.PF := if Is_Parity_Even
  Set_RFLAGS(rflags);
  ```

  If `RFLAGS` was just a variable, we would be able to write this simpler, more direct
  code instead

  ```isa
  let (result, carry_out, overflow, alternative_carry) := Add(src1, src2);

  RFLAGS.OF := overflow;
  RFLAGS.SF := result[operand_size-1];
  RFLAGS.ZF := if Is_Zero(result) then 0b1 else 0b0;
  RFLAGS.AF := alternative_carry;
  RFLAGS.PF := if Is_Parity_Even
  ```



### The Uniform Access Principle

An early design principle in many languages such as Smalltalk, Eiffel, Ruby, Javascript, [Scala](https://docs.scala-lang.org/tour/classes.html), [Python](https://docs.python.org/3/library/functions.html)
and [C#](https://learn.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/using-properties)
is the [Uniform Access Principle](https://en.wikipedia.org/wiki/Uniform_access_principle)
that states that 

> All services offered by a module should be available through a uniform
> notation, which does not betray whether they are implemented through
> storage or through computation.
> <footer>&mdash; Bertrand Meyer</footer>

In practice, this consists of providing language support for defining get/set functions
similar to those above but being able to invoke them using the same syntax as for variables
or, for object oriented languages, the same syntax as fields/attributes/properties (`x.f`)

For example, in Javascript you could define a class with a getter and setter pair called 'msg'
like this (example from [MDN's great documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/get))

```javascript
class ClassWithGetSet {
  #msg = "hello world";
  get msg() {
    return this.#msg;
  }
  set msg(x) {
    this.#msg = `hello ${x}`;
  }
}

const instance = new ClassWithGetSet();
console.log(instance.msg); // "hello world"

instance.msg = "cake";
console.log(instance.msg); // "hello cake"
```

while the same example in Scala would look like this

```scala
class ClassWithGetSet:
  private var _msg = "hello world";

  def msg: String = _msg

  def msg_= (newValue: String): Unit =
    _msg = s"hello $newValue"
end ClassWithGetSet
```

Earlier languages like Algol, Pascal, Modula-2 and Ada also provided incomplete
support for the UAP by allowing function definitions to omit argument lists.
For example, one could define a getter function like this

```function
function msg : string
begin
    return _msg;
end
```


### Applying the Uniform Access Principle

Given the strong influence of Algol, Pascal, Modula-2 and Ada on our language,
it should be no surprise that we use nullary functions (i.e., functions with
no argument list) to define getter functions

Continuing the RFLAGS example above, the getter function to model reading
a register would be defined as follows

```isa
var _RFLAGS : RFLAGS_Type; // storage for RFLAGS register

function RFLAGS -> RFLAGS_Type
begin
    var result := _RFLAGS;

    // set/clear sticky bits
    result[1] := 0b1;
    result[3] := 0b0;
    result[5] := 0b0;
    result[15] := 0b0;
    result[63:22] := Zero(42);

    return result;
end
```

For setter functions, we draw our inspiration from the Scala language
except that, instead of writing `RFLAGS_= ...`, we write
`RFLAGS := ...`.

```isa
function RFLAGS := newValue : RFLAGS_Type
begin
    var rflags := newValue;

    // set/clear sticky bits
    rflags[1] := 0b1;
    rflags[3] := 0b0;
    rflags[5] := 0b0;
    rflags[15] := 0b0;
    rflags[63:22] := Zero(42);

    _RFLAGS := rflags;
end
```

With these definitions, uses of `RFLAGS` as an r-value call the first function and
uses of `RFLAGS` as an l-value call the second function and the ADD instruction
is specified as follows.

```isa
let (result, carry_out, overflow, alternative_carry) := Add(src1, src2);

RFLAGS.OF := overflow;
RFLAGS.SF := result[operand_size-1];
RFLAGS.ZF := if Is_Zero(result) then 0b1 else 0b0;
RFLAGS.AF := alternative_carry;
RFLAGS.PF := if Is_Parity_Even
```

*Note that each flag assignment involves a read-modify-write so it calls both the getter and the setter function.
For example, `RFLAGS.OF := overflow;`
is equivalent to the following.*

```isa
var tmp := RFLAGS;
tmp.OF  := overflow;
RFLAGS  := tmp;
```


*[Documentation note:
In Scala, the `_=` suffix is part of the name of the setter function.
We chose to keep `:=` as a separate token and tend to put a space
between the name and the `:=` but, when discussing
getter/setter functions, we will often treat `:=` as a suffix and
refer to the setter function as `RFLAGS:=` function to contrast it
with the getter function `RFLAGS`.]*



### Applying the uniform access principle to register files

The idea of allowing function calls to look like variable accesses extends naturally to
allowing function calls to look like array accesses.
For example, to implement the `XMM` aliasing of `ZMM`, we can write

```isa
function XMM[i : {0..31}] -> Bits(128)
begin
    return ZMM[i][127:0];
end

function XMM[i : {0..31}] := newValue : Bits(128)
begin
    ZMM[i][127:0] := newValue;
end
```

This allows `XMM` to be accessed as if it was an array.
For example, part of the `ENCODEKEY128` instruction writes to six of the XMM registers.

```isa
XMM[0] := handle.AAD;
XMM[1] := handle.Integrity_Tag;
XMM[2] := handle.Cipher_Text;

XMM[4] := Zero(128); // reserved for future usage
XMM[5] := Zero(128); // reserved for future usage
XMM[6] := Zero(128); // reserved for future usage
```

### Exceptions and the uniform access principle

In [an earlier article]({{ site.baseurl }}{% post_url 2026-07-02-designing-exceptions-for-readability %}),
we described why we annotate function definitions and function calls with
"exception markers" `?`/`!` to help readers understand that
a function might/always throws an exception.
Something that we have not committed to yet is whether these get/set functions are
allowed to throw exceptions (and, therefore, are they allowed to have exception markers?)

The argument against allowing them to throw exceptions is that we are deliberately
trying to make calls to these functions look like variable accesses but variable accesses
cannot throw exceptions so it would be strange if what looks like a variable access
could throw an exception.

The argument for allowing them to throw exceptions is one of uniformity:
if traditional functions can throw exceptions, then get/set functions should also
be able to throw exceptions.

We have not committed to a decision yet because we have not yet found a compelling
case where we want to use variable/array syntax for a function that can raise an
exception.
Once we have a potential example (or, better, multiple examples), it will be easier
to decide what we want to do.


## Implicit size arguments

In [an earlier article]({{ site.baseurl }}{% post_url 2026-06-30-designing-types-for-readability %}),
we described the use of *implicit size arguments* to reduce the level of type clutter in
a specification.
In particular, in a function with this type

```isa
function Bit::Reverse(implicit size : {0..}, x : Bits(size)) -> Bits(size);
```

the argument `size` can be marked as implicit because, in any call to the function,
it will be possible to infer the value of the `size` argument from the type
of the `x` argument.
For example, in this code, it is clear that `size == 64` because `RAX : Bits(64)`.

```isa
let result := Bit::Reverse(RAX);
```

## Named arguments

In most function calls, the purpose/meaning of a function argument is obvious.
For example, in the specification of memory accesses, we call a function to
translate logical memory addresses to linear addresses and just from the names
of the function and the arguments, you can probably guess roughly what the
function arguments are used for.

```isa
let linear := Translate_Logical_To_Linear?(logical, alignment_type);
```

But sometimes the purpose of an argument is not obvious. In particular,
we have found that if the argument is a constant such as `True` or `False`,
then it is hard to guess what the purpose of the constant is and you will have
to consult documentation for the function to make sense of the function call.
For example, in the FPREM instruction (that performs a floating point remainder
calculation), it would be fairly obvious why you are passing the two input operands
to the function --- but what is the purpose of the mysterious third argument?

```isa
FP87::Remainder(src1, src2, False);
```

We used to get round this issue by introducing a local variable with a more suggestive
name like this --- which makes it clear that the argument relates to the [IEEE floating
point standard](https://en.wikipedia.org/wiki/IEEE_754).

```isa
let is_IEEE_compliant := False;
FP87::Remainder(src1, src2, is_IEEE_compliant);
```

But a better approach is to copy the many languages (Ada, Python, Java, C#, ...) that support
the use of [named arguments](https://en.wikipedia.org/wiki/Named_parameter): using the name of the
argument to associate the actual argument at the callsite with the formal argument in the function definition
(instead of relying on the order/position of the arguments).

```isa
FP87::Remainder(src1, src2, is_IEEE_compliant=>False);
```

*[The choice of the symbol `=>` is based on advice in the [Rationale for the
Design of the Ada Programming
Language](http://archive.adaic.com/standards/83rat/html/Welcome.html) to use
symbols consistently and, in particular, to use `=>` when creating an
association with names/labels/keys.  We often wonder whether we would have been
better using `:=` for consistency with assignment.]*


## Default arguments

There are some functions that are almost always called with the same (constant) argument.
For example, the `Add` function called in the `ADD` instruction code above is normally called with two input operands.
But, in the Add With Carry (`ADC`) instruction, we want to specify an additional input: the carry flag.

To support this, we follow languages like C++, Python, etc. in supporting
[default arguments](https://en.wikipedia.org/wiki/Default_argument):
allowing a function call to omit one or more actual arguments with the argument value
being provided by an expression in the function definition.

For example, the `carry_in` argument of the `Add` function specifies the
value to use if the function is only called with two explicit arguments.

```isa
function Add(x : Bits(N), y : Bits(N), carry_in : Bit := 0b0) -> (Bits(N), Bit, Bit, Bit)
```

In places where we want to override the default argument, we usually use
a named argument as in the following code.

```isa
let (result, carry_out, overflow, alternative_carry) := Add(src1, src2, carry_in=>RFLAGS.CF);
```

We found that default arguments are especially useful in the memory system.
For example, there are over 190 calls to the function `Logical_Mem_Read` in our specification
and these all need to specify the segment, offset within the segment,
and the size of the read.
But only a handful of the calls to the function need to perform a locked access
or override the default alignment-checking rules.
So we provide default values for the arguments that control this more obscure
functionality.

```isa
function Logical_Mem_Read?(
        segment : Segment,
        offset : Bits(64),
        size : Integer,
        access_type : Memory_Access_Type := Memory_Read,
        alignment_type : Memory_Alignment_Type := Normal_Alignment,
        acquire_lock : Boolean := False
    ) -> Bits(size)
```

Using default arguments with this function keeps most of the code simple
and it helps emphasize that there is something unusual in the few places
where more than three arguments are provided.

## Summary

Function definitions are where a lot of the features of our language come together.
Our syntax
- supports the Uniform Access Principle so that we can abstract out some of the
  complexity of registers in ISA definitions;
- function definitions use [exception markers]({{ site.baseurl }}{% post_url
  2026-07-02-designing-exceptions-for-readability %}) to make it easy to see
  whether a function can or must throw an exception;
- to reduce the clutter associated with strict typechecking, implicit size
  arguments can be [inferred from the sizes of other arguments]({{ site.baseurl
  }}{% post_url 2026-06-30-designing-types-for-readability %});
- we support named arguments in function calls to make it easier to understand
  the role of arguments;
- and we support default arguments to suppress uninteresting detail in most
  function calls and highlight interesting changes when the default does not
  apply.


--------------------------------

*This article is part of a series of articles about the design of the The Intel® ISA Specification Language.
For more articles, see*

- [Designing a language for readability]({{ site.baseurl }}{% post_url 2026-06-27-designing-a-language-for-readability %})
- [Naming conventions for readability]({{ site.baseurl }}{% post_url 2026-06-28-naming-conventions-for-readability %})
- [Designing syntax for readability]({{ site.baseurl }}{% post_url 2026-06-29-designing-syntax-for-readability %})
- [Designing types for readability]({{ site.baseurl }}{% post_url 2026-06-30-designing-types-for-readability %}).
- [Designing exceptions for readability]({{ site.baseurl }}{% post_url 2026-07-02-designing-exceptions-for-readability %}).
- [Designing functions for readability]({{ site.baseurl }}{% post_url 2026-07-03-designing-functions-for-readability %}).
