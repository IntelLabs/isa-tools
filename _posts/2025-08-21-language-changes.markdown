---
layout: post
title: "Forthcoming language changes"
tags: plans
---

Over the last 15 years of work on ISA specifications, my ideas on the ideal ISA
specification language have slowly changed.
I have had a chance to see how language features are used at scale;
how other specification developers use the language;
and which parts of the language people just get and which bits cause confusion.
This has changed how I think about the language and about the priorities
of the language.
Ideas that seemed to work well from the language designer's or programmer's perspective
turn out to be confusing from the perspective of somebody reading the specification.
And our expectations about what languages readers are familiar with and that may
color how they read and interpret the specification change.

Working on the (forthcoming) specification of the Intel&reg; Architecture has
provided both fresh insight into the problems of existing specification
languages and also an opportunity to fix some of the issues that we have been
experiencing.

We had been attempting to solve these problems through a number of small extensions
(see
[[a]]({{site.baseurl}}/{% post_url 2025-05-08-extension-named-parameters %}),
[[b]]({{site.baseurl}}/{% post_url 2025-05-09-extension-exception-markers %}),
[[c]]({{site.baseurl}}/{% post_url 2025-05-09-extension-parameterized-records %}),
[[d]]({{site.baseurl}}/{% post_url 2025-05-09-extension-topslice %}))
but we were limited in the changes we could make by our increasingly unsuccessful
attempts to preserve consistency with the Arm's Architecture Specification Language
which was evolving to better meet the needs of the Arm architectures and customers.

We have decided to drop our goal of source-level consistency with Arm's
Architecture Specification Language and make some more significant changes to
the language.
The new ISA specification language will maintain the same goals as before of
readability, ease of understanding, difficulty of confusion, executability, etc.
but it will have a new grammar, type-system, foreign-function interface, module-system, etc.

Most of the new language design is complete and has been tested at scale (by converting
a large specification to the new language) but we are still fine-tuning a few details
as we build experience with the new language.

The [ASLi tool](https://github.com/IntelLabs/asl-interpreter) will continue to exist but
it will change to support our new ISA specification language and it will
be renamed accordingly.
