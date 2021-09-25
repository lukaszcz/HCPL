HCPL is a programming language and a proof checker based on illative
combinatory logic - a foundational system dating back to the work of
Haskell Curry and his school. In one sentence, one could (somewhat
metaphorically) say that the logic is dynamically typed. As a
programming language, HCPL is similar to Lisp but with more syntactic
sugar.

Features
========
* Functional language with pattern matching, macros, modules and
  custom syntax extensions.
* Relatively efficient interpreter, with unboxed values, etc.
* Higher-order illative logic with basic support for inductive
  datatypes.
* Rudimentary tactics and standard library.
* Syntax highlighting for Kate.

The current version is a prototype, but it is complete and stable
enough to be usable.

Examples
========

The [`examples/`](examples) subdirectory of the data directory
contains commented examples which form a tutorial introduction to
HCPL. Some knowledge of logic and functional programming is necessary
to understand them. You should read the examples in their numerical
order. Preferably, *.hcpl files should be edited in Kate as syntax
highlighting is installed for this editor. In case the highlighting
does not work, there are also html versions of the examples.

Requirements
============
* OCaml
* dune
* m4, sed, diff, GNU make
* Kate text editor

The Kate text editor is not strictly necessary, but highly
recommended, because syntax highlighting is available and
automatically installed for it.

Installation and usage
======================

To compile HCPL type:
```
make
```

To run tests type:
```
make test
```

To install HCPL type:
```
make configure
make
make install
```

During installation you will be asked for the data directory. In this
directory the examples, the standard library and other data files of
HCPL will be stored.

After installation you should be able to run HCPL with: `hcpl
file.hcpl` or `hcpl -i` for REPL.  Type `hcpl --help` for a list of
options. To remove HCPL from your system type: `uninstall-hcpl` (note:
this will remove the data directory completely).

Copyright and license
=====================

Copyright (C) 2013-2021 by Lukasz Czajka

HCPL is distributed under the MIT license. See the [LICENSE](LICENSE)
file.
