//Implementation of "An O(NP) Sequence Comparison Algorithm" 
//by Sun Wu, Udi Manber, Gene Myers and Web Miller.
//Note: P=(D-(M-N))/2, D is shortest edit distance, M>=N

//Returns indices in a and b for longest common sequence given equality function f
//Example: lcs["ABCABA";"CABBA";=]
//In Miller-Myers diff algorithm, m>n - so, function flips arguments if this is not the case
lcs:{[a;b;f] $[flipped;{(x[1];x[0])};(::)] $[flipped:(count a)>count b;lcsh[b;a;f];lcsh[a;b;f]]}

//Optimized version of lcs function - chops off common suffix and prefix before running
//LCS algorithm - helps in case of long prefix/suffix overlap
lcsopt:{[a;b;f]
  //find common prefix and common suffixes first - this is an optimization to reduce
  //time for LCS - you may discard this if you prefer simplicity
  psl:{[x;y;f] pi: where not f'[x;(count x)#y]; pl: (count x)^first pi;c1:neg (count x)-   pl; si: where not f'[c1#x;c1#y]; sl: (neg c1)-0^1 +    last si;(pl;sl)} . $[(count a)>count b; (b;a;f);(a;b;f)];
  a1: (neg psl[1]) _ psl[0] _ a; /chop off common prefixes and suffixes
  b1: (neg psl[1]) _ psl[0] _ b;
  //if a1 or b1 is empty after prefix/suffix chop-off,  the empty one is subsequence
  //of other. The non-empty one is delta. So, lcs is prefix+suffix - return - don't
  //call lcs function as it works only on non-empty sequences
  $[(0=count a1) or 0=(count b1);:({[p;s;x] (p#x),(neg s)#x}[psl[0];psl[1];] each (til count a;til count b));
    idx:psl[0]+lcs[a1;b1;f]]; /offset the indices by length of chopped prefix
  :((pi, (first idx),((neg psl[1])+count a) + si);((pi:til psl[0]), (last idx),((neg psl[1])+count b) + si:til psl[1]));
  }


lcsh:{[a;b;f]
  n:count a;m:count b;delta:m-n; offset:n+1;
  @[`.;;:;(m+n+3)#(-1)] each `snodes`fp;p:-1; /snake nodes, further points, p value
  @[`.;`snakearr;:;()]; /array of snakes. How does that sound?
  snakef:snakes[;;;f;];
  while[fp[delta+offset] < m;
    p+:1;
    ct:delta+p; k:neg p; snakef[a;b;k;ct]; /vertical traversal
    ct:p;k:delta+p; snakef[a;b;k;ct]; /horizontal traversal
    snakef[a;b;delta;1]; /find furthest point in delta diagonal
    ];
  @[`.;`paths;:;()];
  //iterate backwards starting from last snake on delta (i.e., destination (n,m)) and work backwards to (0,0) where 
  //snode is -1. So, condition for iterate is set to check for snode value
  {[d;i] if[d[i][3]>0;@[`.;`paths;,;enlist (d[i][1]+i2;d[i][2] + i2:til d[i][3])]]; d[i][0]}[snakearr;]/[-1<;-1+ count snakearr];
  idx: {(raze x[;0];raze x[;1])} reverse paths;
  ![`.;();0b;`fp`snodes`snakearr`paths]; /delete the global arrays
  :idx;
  }

//Snakes are as in snakes-and-ladders board game. To get from top of the board
//to bottom of the board in shortest path, you need longest sequence of diagonals.
//Every match here is marked with a diagonal path. Hence, the term snake used by 
//Myers et al, I guess.
//Note: index x in the function corresponds to position x+1 in the figure 1 of paper - 
//so (3,2) is (4,3) in the paper
snakes:{[a;b;k;f;ct]
  n:count a;m:count b;offset:n+1;
  vert:$[k <(m-n);1b;0b]; /if below diagonal, vertical else horizontal
  do[ct;
    $[(fp[k+offset-1]+1)>fp(k+offset+1);[kp:k+offset-1;yp:fp[kp]+1];[kp:k+offset+1;yp:fp[kp]]];
    xp:x:(y:yp)-k; 
    while[(x<n) and (y<m) and f[a[x];b[y]]; x+:1; y+:1];
    fp[k+offset]::y;
    @[`.;`snakearr;,;enlist (snodes[kp];xp;yp;(y-yp))];
    snodes[k+offset]:: -1 + count snakearr;
    $[vert;k+:1;k-:1]];
  }

//Wrapper to find mutations between two tables given s in sym column. c columns are used
//for mutation check - for example, price and size columns have very good quality signal
//for trade mutation check
//Example: diffTables[t1;t2;`ABC;`price`size] - returns indices in t1 and t2 which differ
diffTables:{[t1;t2;s;c]
  i1: exec i from t1 where sym in s;
  i2: exec i from t2 where sym in s;
  a:flip (t1 i1) c;
  b:flip (t2 i2) c;
  il:lcs[a;b;{[x;y] all x=y}]; /replace with call to lcsopt instead if optimizing for big tables with long common prefix/suffixes
  dela: (til count a) except il[0]; //return delta indices, i.e., not a common subsequence  in a
  delb: (til count b) except il[1]; //return delta indices, i.e., not a common subsequence  in b
  :(i1 dela; i2 delb) //return the delta indices in original table
  }
