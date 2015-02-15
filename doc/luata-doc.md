This document describes the basic architecture of LuaTA, the Lua Type Analysis.


Architecture:
-------------

The tool consists of a basic lexer and parser (scanner.mll,
parser.mly) built with ocamllex and ocamlyacc, respectively.

The parser creates an AST (ast.ml), which is subsequently instrumented
with labels (label.ml), leaving a labeled AST (last.ml). A number of
common functions perform these three first phases (grouped in frontend.ml)

Pretty printers for both AST types are available (ast_pprint.ml,last_pprint.ml).

A number of modules models the analysis composition
(analysislattice.ml,statelattice.ml,envlattice.ml,storelattice.ml,proplattice.ml,
absencelattice.ml,valuelattice.ml,stringlattice.ml,numberlattice.ml) which is
described in more detail below.

A separate module (transfer.ml) models the transfer of lattice values
between program points. This is accomplished by a structural traversal
of the labelled AST until a fixed point is reached. A separate module
takes care of printing the obtained analysis result
(pprint.ml). Another (preliminary) module computes a list of warnings
based on the analysis result (warnings.ml).

Common error message procedures are grouped in a separate module (error.ml).

The analysis comes with two user interfaces: a command line interface
and a web client (compiled with js_of_ocaml).

A main file binds everything together for the command line interface
(main.ml). Another file binds stuff together for the web client
(jsbridge.ml) along with a bit of glue (index.js,index.html).


 
Basic lattice composition:
--------------------------

The analysis is composed of a number of lattices. Overall,
analysislattice associates a state lattice ('an abstract state') to
each program point label. A statelattice consists of a storelattice
(modelling the heap), and an environment lattice (envlattice), which
models scope chains.

The storelattice associates to each table declaration a proplattice,
describing the shape of tables originating from this declaration
point. Concretely proplattice associates to every (string) entry a set
of type tags, a string lattice value, a set of function labels (user
declared and builtin functions), and a set of table (declaration site) labels.
Moreover, each entry of proplattice is marked with an absencelattice
bit to indicate whether we are certain of an entry's
presence. Non-string entries (k,v) are approximated in proplattice
using two additional valuelattices: one accounting for all keys and
one accounting for all values.

Overall this lattice composition is quite reminiscent of TAJS, the
Type Analysis for JavaScript.


    analysislattice : pplabel -> statelattice
    
       statelattice : storelattice x envlattice
    
         envlattice : 2^(label * label list)
    
       storelattice : tablelabel -> proplattice
    
        proplattice : (string -> absencelattice x valuelattice) x valuelattice x valuelattice
    
       valuelattice : 2^tag x stringlattice x numberlattice x 2^funlabel x 2^tablelabel
    
      stringlattice : { Bot, Const s, Top }
    
      numberlattice : { Bot, Top }
    
                tag : { Nil, Bool, Userdata }


An example
----------

Consider the output when running the type analysis on an example,
e.g., examples/table6.lua:

    $ ./luata examples/table6.lua

The command line client will pretty print the inferred memory for each
program point, finishing with a list of warnings.

Alternatively you can open index.html in your browser, load
'examples/table06.lua' by using the 'Choose file' button, and pressing
'Analyze'. 

The left side of the web client page contains the original program
text whereas the right side of the page contains a desugared, labelled
version of the input program. Warnings are highlighted on the
left-side with an exclamation mark in front of the relevant line. The
inferred memory for each program point is available as an overlay over
the desugared program lines on the right.


Before execution of the first line, labelled 1, the inferred memory
looks as follows (either scroll up in the command line output or hover
over line 0, labelled 1, in the right hand side of the page): 

    1: { store:   { -7       -> { concat       -> ! { funs:     { [builtin:tblconcat] } } }
                    -6       -> { byte         -> ! { funs:     { [builtin:strbyte] } }
                                  char         -> ! { funs:     { [builtin:strchar] } }
                                  format       -> ! { funs:     { [builtin:format] } }
                                  len          -> ! { funs:     { [builtin:strlen] } }
                                  lower        -> ! { funs:     { [builtin:strlower] } }
                                  sub          -> ! { funs:     { [builtin:strsub] } }
                                  upper        -> ! { funs:     { [builtin:strupper] } } }
                    -5       -> {  }
                    -4       -> { abs          -> ! { funs:     { [builtin:abs] } }
                                  ceil         -> ! { funs:     { [builtin:ceil] } }
                                  floor        -> ! { funs:     { [builtin:floor] } }
                                  huge         -> ! { number:   Top }
                                  random       -> ! { funs:     { [builtin:random] } }
                                  sqrt         -> ! { funs:     { [builtin:sqrt] } } }
                    -3       -> { exit         -> ! { funs:     { [builtin:exit] } }
                                  write        -> ! { funs:     { [builtin:write] } } }
                    -2       -> { default key  ->   { number:   Top }
                                  default      ->   { strings:  Top } }
                    -1       -> { _G           -> ! { tables:   { -1 } }
                                  _VERSION     -> ! { strings:  Top }
                                  arg          -> ! { tables:   { -2 } }
                                  error        -> ! { funs:     { [builtin:error] } }
                                  getmetatable -> ! { funs:     { [builtin:getmetatable] } }
                                  io           -> ! { tables:   { -3 } }
                                  ipairs       -> ! { funs:     { [builtin:ipairs] } }
                                  math         -> ! { tables:   { -4 } }
                                  next         -> ! { funs:     { [builtin:next] } }
                                  os           -> ! { tables:   { -5 } }
                                  pairs        -> ! { funs:     { [builtin:pairs] } }
                                  print        -> ! { funs:     { [builtin:print] } }
                                  rawget       -> ! { funs:     { [builtin:rawget] } }
                                  rawset       -> ! { funs:     { [builtin:rawset] } }
                                  setmetatable -> ! { funs:     { [builtin:setmetatable] } }
                                  string       -> ! { tables:   { -6 } }
                                  table        -> ! { tables:   { -7 } }
                                  tonumber     -> ! { funs:     { [builtin:tonumber] } }
                                  tostring     -> ! { funs:     { [builtin:tostring] } }
                                  type         -> ! { funs:     { [builtin:type] } } }
                    0        -> {  }}
         env:     { (0,[-1]) } }


More interestingly, the inferred abstract state after the program is
available at label 12 (again either scroll the command line output or
hover over line 7, labelled 12):

    12: { store:   { -7       -> { concat       -> ! { funs:     { [builtin:tblconcat] } } }
                     -6       -> { byte         -> ! { funs:     { [builtin:strbyte] } }
                                   char         -> ! { funs:     { [builtin:strchar] } }
                                   format       -> ! { funs:     { [builtin:format] } }
                                   len          -> ! { funs:     { [builtin:strlen] } }
                                   lower        -> ! { funs:     { [builtin:strlower] } }
                                   sub          -> ! { funs:     { [builtin:strsub] } }
                                   upper        -> ! { funs:     { [builtin:strupper] } } }
                     -5       -> {  }
                     -4       -> { abs          -> ! { funs:     { [builtin:abs] } }
                                   ceil         -> ! { funs:     { [builtin:ceil] } }
                                   floor        -> ! { funs:     { [builtin:floor] } }
                                   huge         -> ! { number:   Top }
                                   random       -> ! { funs:     { [builtin:random] } }
                                   sqrt         -> ! { funs:     { [builtin:sqrt] } } }
                     -3       -> { exit         -> ! { funs:     { [builtin:exit] } }
                                   write        -> ! { funs:     { [builtin:write] } } }
                     -2       -> { default key  ->   { number:   Top }
                                   default      ->   { strings:  Top } }
                     -1       -> { _G           -> ! { tables:   { -1 } }
                                   _VERSION     -> ! { strings:  Top }
                                   arg          -> ! { tables:   { -2 } }
                                   error        -> ! { funs:     { [builtin:error] } }
                                   getmetatable -> ! { funs:     { [builtin:getmetatable] } }
                                   io           -> ! { tables:   { -3 } }
                                   ipairs       -> ! { funs:     { [builtin:ipairs] } }
                                   math         -> ! { tables:   { -4 } }
                                   next         -> ! { funs:     { [builtin:next] } }
                                   os           -> ! { tables:   { -5 } }
                                   pairs        -> ! { funs:     { [builtin:pairs] } }
                                   print        -> ! { funs:     { [builtin:print] } }
                                   rawget       -> ! { funs:     { [builtin:rawget] } }
                                   rawset       -> ! { funs:     { [builtin:rawset] } }
                                   setmetatable -> ! { funs:     { [builtin:setmetatable] } }
                                   string       -> ! { tables:   { -6 } }
                                   table        -> ! { tables:   { -7 } }
                                   tonumber     -> ! { funs:     { [builtin:tonumber] } }
                                   tostring     -> ! { funs:     { [builtin:tostring] } }
                                   type         -> ! { funs:     { [builtin:type] } }
                                   x            -> ? { tags:     { Nil, Bool }
                                                       number:   Top
                                                       strings:  Const "hello"
                                                       funs:     { 3 } } }
                     0        -> {  }
                     1        -> { t            -> ! { tables:   { 2 } } }
                     2        -> { default key  ->   { number:   Top }
                                   default      ->   { tags:     { Bool }
                                                       number:   Top
                                                       strings:  Const "hello"
                                                       funs:     { 3 } } }
                     3        -> { scopechain   ->   { [ 0, -1 ] } }
                     6        -> { i            -> ! { number:   Top } }}
          env:     { (6,[1, 0, -1]) } }

For example, 

  - the environment inferred is the scopechain (6,[1,0,-1]), where
    each number represents an allocation-site of an environment
    allocation:

      - '6' is the local declaration of variable 'i' in line 3,
      	inferred to be a number 'number: Top'
      - '1' is the local declaration of variable 't' in line 0,
      	inferred to be a table, itself allocated at label 2 (line 0).
      - '-1' is the initial, global environment containing a variable
      	'x' (assigned in line 5).

  - the store tells us that the entries of the table value '2' all
    have a number key (default key), and that their value (default) may be
    Boolean, number, the constant string "hello", or a function value
    originating from the declaration site labelled '3'

  - since x is the result of reading an entry from t, the type of the
    global variable x may also be one of the above.

  - the analysis conservatively assumes that the loop main run zero or
    more iterations, and hence concludes that 'x' is not guaranteed to
    be present (the 'x' entry in the global environment '-1' is marked
    '?', meaning "maybe absent").


Finally as the inferred memory for the body of the anonymous function
"function (x) return x end" in table t's third entry is bottom
(meaning unreachable) both clients will emit a warning to that effect.



References:

 Type Analysis for JavaScript
 Simon Holm Jensen, Anders Møller and Peter Thiemann
 SAS'09
 http://cs.au.dk/~amoeller/papers/tajs/paper.pdf


 QuickChecking Static Analysis Properties
 Jan Midtgaard and Anders Møller
 ICST'15
 http://janmidtgaard.dk/papers/Midtgaard-Moeller:ICST15.pdf
