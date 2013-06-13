This repository contains implementation of diff/longest common subsequence(LCS) algorithm, for use in Q. There are two ways to use it:

- Use pure q code. In that case, use q/miller.q. Pure q code has the advantage that you can pass an arbitrary comparison function for LCS. The disadvantage is that it can be quite slow when the number of diffs are large. Also, it can't be peach'ed because of global arrays.

- Use c code. In that case, compile c code in c/* using Makefile, and load it using c/lcs.q. This code is much faster than pure Q code because the equality function is hard-coded, and vectorized, with type checking and bounds-checking done as early as possible. It often runs 100-200 times faster than Q code (depending on number of comparisons), and can be peach'ed.

Note: 
1) Compiling the C code will require gcc, preferably v4.6+ because of gcc specific attributes in the code. I have tested it with gcc 4.7.2.
2) Make sure KXVER is correct in lcs.h and utils.h before compiling. It is set to 3 for KDB 3.0. For older versions, it should be set to 2.

In both cases, you can pass general lists for comparison, but they shouldn't be nested by depth of more than 1. For example, following list is ok as an argument:
(til 10;til 10)

This one is not acceptable because of deeper nesting - LCS function will return null in c code:
enlist (til 10;til 10)

Illustration using c/lcs.q:

q)a:(til 10;til 10)
q)lcs[a;a] //this is ok - both elements match
0 1
0 1
q)lcs[enlist a;enlist a] //deeper nesting - not ok - returns (`int$();`int$())


q)null each lcs[enlist ;enlist a]
11b

Example of how to use:
----------------------
Pure Q code: Load q/miller.q
-----------
q)lcs["HUMAN";"CHIMPANZEE";=]
0 2 3 4
1 3 5 6
q)"HUMAN" first lcs["HUMAN";"CHIMPANZEE";=]
"HMAN"
q)"CHIMPANZEE" last lcs["HUMAN";"CHIMPANZEE";=]
"HMAN"


C code: Load c/lcs.q (after compiling lcs.so, and making sure it is available in LD_LIBRARY_PATH)
-------
q)lcs["HUMAN";"CHIMPANZEE"]
0 2 3 4
1 3 5 6
q)"HUMAN" first lcs["HUMAN";"CHIMPANZEE"]
"HMAN"
q)"CHIMPANZEE" last lcs["HUMAN";"CHIMPANZEE"]
"HMAN"

Performance comparison:
----------------------

Pure C code:
------------
q)a: til 2000
q)b: 1000 _ til 3000
q)\t lcs[a;b]
17

Q code:
q)a: til 2000
q)b: 1000 _ til 3000
q)\t lcs[a;b;=]
3324
