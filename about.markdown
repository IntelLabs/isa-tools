---
layout: page
title: About
permalink: /about/
---

The [Intel® ISA Specification Language](language) is an executable language for writing
clear, precise specifications of Instruction Set Architectures (ISAs).

ISA tools is an implementation of the language that can execute
specifications either in an interpreter or by compiling via C code.

You can download ISA tools from https://github.com/IntelLabs/isa-tools and
install by following the [README](https://github.com/IntelLabs/isa-tools/blob/master/README.md).
We include a small demonstration of how to use the tools to build simulators for a toy
architecture specification.

This tool is based on Arm's open source
[asl-interpreter](https://github.com/ARM-software/asl-interpreter) release with
extensive modifications to

* Change ASLi to support the Intel® ISA Specification Language
  including changes to the AST, lexer, parser and typechecker.

  (Some heavily bitrotted parts of the support for Arm's ASL
  language still remain but they will be removed shortly.)
* Addition of
  - tests,
  - C compilation option
  - a runtime library
  - etc.
* Add a demo to illustrate how to generate simulators from
  an ISA specification.

We welcome contributions following the [Contributing](https://github.com/IntelLabs/isa-tools/blob/master/CONTRIBUTING.md) instructions.

Please file issues on the GitHub project page at https://github.com/IntelLabs/isa-tools/issues
and security issues following the instructions in [the Security Policy file](https://github.com/IntelLabs/isa-tools/blob/master/Security.md).
