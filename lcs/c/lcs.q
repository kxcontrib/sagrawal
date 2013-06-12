//Implementation of "An O(NP) Sequence Comparison Algorithm" 
//by Sun Wu, Udi Manber, Gene Myers and Web Miller.
//Note: P=(D-(M-N))/2, D is shortest edit distance, M>=N

//Returns indices in a and b for longest common sequence given equality function f
//Example: lcs["ABCABA";"CABBA"]
//In Miller-Myers diff algorithm, m>n - so, function flips arguments if this is not the case

if[null libpth:`$ -3 _ string first pth where count each key each pth:{(`$":",x,"/lcs.so")} each ":" vs getenv `LD_LIBRARY_PATH; -1"lcs.so not found...Aborting"; -1"Checked following LD_LIBRARY_PATH locations:"; -1 (1_) each string each pth; exit 1];
lcs: libpth 2:(`lcs;2);

//Wrapper to find mutations between two tables given s in sym column. c columns are used
//for mutation check - for example, price and size columns have very good quality signal
//for trade mutation check
//Example: diffTables[t1;t2;`ABC;`price`size] - returns indices in t1 and t2 which differ
diffTables:{[t1;t2;s;c]
  i1: exec i from t1 where sym in s;
  i2: exec i from t2 where sym in s;
  a:flip (t1 i1) c;
  b:flip (t2 i2) c;
  il:lcs[a;b]; /replace with call to lcsopt instead if optimizing for big tables with long common prefix/suffixes
  dela: (til count a) except il[0]; //return delta indices, i.e., not a common subsequence  in a
  delb: (til count b) except il[1]; //return delta indices, i.e., not a common subsequence  in b
  :(i1 dela; i2 delb) //return the delta indices in original table
  }
