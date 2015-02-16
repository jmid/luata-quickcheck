Requirements:
-------------

- OCaml, version 4.01.0 or newer
  - ocamlp4-extra (for 'camlp4of', required by bisect below)
- OCaml libraries/tools (through OPAM):
  - ocamlfind
  - qcheck v.0.2
  - ounit v.2.0.0
  - bisect v.1.3 (for coverage reports)
- GNU Make

The web-interface further requires:
- Js_of_ocaml
- CodeMirror (included)
- TextHover addon to CodeMirror (also included)
 

Analysis instructions:
----------------------

The file doc/luata-doc.md provides a basic description of the
analysis architecture as well as an explanation of its output.

The analysis comes with both a command line interface and a web
client. 

To build the command line client, run:

    $ make

To run the analysis on a Lua program, for example (see
examples/ directory for other examples):

    $ ./luata examples/table06.lua

The result will now be printed to the console.

To reduce the amount of console output passing the option '-no-heap'
will just emit the warnings:

    $ ./luata -no-heap examples/table06.lua


Alternatively you can build the web client with

    $ make js

The web client is now available as an HTML page with overlays
in 'index.html' (view with a web browser and shrink fontsize).


Quickcheck instructions:
------------------------

 The source code for the QuickCheck LCheck module is included
 in src/lCheck.ml for convenience, but is also separately available
 from http://github.com/jmid/lcheck

 The source code applying LCheck to the Lua type analyses is
 available in src/edslcheck.ml

 To rerun our quickchecking on the type analysis:

    $ make -B edslcheck
    $ ./edslcheck

 Warning: the last step takes a full lunchbreak! You can comment out
 some tests at the bottom of src/edslcheck.ml to run fewer tests.


Report instructions:
--------------------

To reproduce the numbers in the paper's Table 1, regarding
QuickCheck, test suite, and combined coverage, follow the below
instructions.

* To reproduce the QuickCheck coverage numbers:

      $ make -B edslcheckcov
      $ make -B runedslcheckcov
      $ make -B checkreport

  (Warning: the last step takes a full lunchbreak)
  The resulting report is now available in HTML format in cov-report-qcheck/index.html


* To reproduce the original test suite coverage numbers:

      $ make -B testcov
      $ make -B runtestcov
      $ make -B testreport

  The resulting report is now available in HTML format in cov-report-test/index.html


* To reproduce the combined coverage numbers:

  First run both the above (which will generate cov-report-qcheck.out
  and cov-report-test.out). Then

      $ make -B combinedreport

  The resulting report is now available in HTML format in cov-report-combined/index.html
