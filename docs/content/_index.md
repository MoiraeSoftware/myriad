---
title: Myriad Overview
category: tutorial
menu_order: 1
---

# Myriad

Myriad is a code generator, it can take an arbitrary file and uses it to produce actual F# code.  The file can be anything from a plain txt file to an F# file.  

Myriad can be used via MSBuild as part of a your project file, or standalone from its CLI tool.

The idea behind Myriad is to un-complicate, as far as possible, the ability to generate code and do meta-programming in F#. By meta-programming in F# I mean generating actual F# types and constructs like discriminated unions and records, not just IL output or simple classes like type providers.

Myriad is an evolution of the ideas I developed while working with F#'s type providers and other meta-programming functionality like quotations and AST manipulation. Myriad aims to make it easy to extend the compiler via Myriad plugins rather than modifying or adjusting Type Providers and other F# improvements that would take a long time to be developed and released.  

The way Myriad a plugin works is that it that works on a particular file type or input data, this could be a fragment of AST input or a proto file etc.  The plugin is supplied metadata such as file name and project information.  there are also various helpers in the library make it easier to produce the AST output with the final form being source code that is built into your project.  The advantages of this are the compiler can optimise the generated code and the tooling can operate effectively as you can navigate the generated code as normal.
