---
title: Myriad Overview
category: tutorial
menu_order: 1
---

# Myriad

Myriad is a code generator, put plainly it takes an abstract syntax tree from a source and uses that to produce F# code.

Myriad can be used from either an MSBuild extension or from its CLI tool.

The idea behind Myriad is to un-complicate, as far as possible, the ability to generate and do meta-programming in F#. By meta-programming in F# I mean generating actual F# code like discriminated unions and records, not just IL output.

Myriad is an evolution of the ideas I developed while working with F#'s type providers and other meta-programming functionality like quotations and AST manipulation. Myriad aims to make it easy to extend the compiler via Myriad plugins rather than modifying or adjusting Type Providers and other F# improvement that would be a long time to be developed and released.  The idea is you write a Myriad plugin that works on a fragment of AST input, and the plugin supplies AST output with the final form being source code that is built into your project.  Thus the compiler can optimise and tooling can operate effectively too.