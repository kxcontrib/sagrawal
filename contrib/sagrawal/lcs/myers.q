//Implementation of Eugene Myer's "O(ND) Difference Algorithm and Its Variations"
//D is number of differences between two sequences - when the sequences are mostly
//similar, this algorithm is fast

//Note: Call lcs function to calculate longest common subsequence

//Function to calculate longest common subsequence backwards: as proven in Myer's O(ND)
// paper to find the LCS, it suffices to start on diagonal (n-m) and shortest edit 
//distance d, and traverse backwards (basically, inverse of path building, but with 
//only one particular path traversed back - that path has only one unique way of reaching 
//back to diagonal 0(remember diagonal k=x-y=0-0=0 at origin). All the diagonals in the
//path are matches and their indices in a and b provide us indices of respective longest 
//common subsequence. Horizontal and vertical paths are deltas

//Returns indices in a and b for longest common sequence given equality function f
//Example: lcs["ABCABA";"CABBA";=]
lcs:{[a;b;f]
  snakearr: snakes[a;b;f];
  mdiff.paths:(); /global list of paths - need it to avoid create temps
  {[d;i] if[d[i][3]>0;@[`mdiff;`paths;,;enlist (d[i][1];d[i][2];d[i][3])]]; d[i][0]}[snakearr;]/[-1<;-1+ count snakearr];
  idx: { (x[0]+i;x[1]+i:til x[2])} each reverse mdiff.paths;
  ![`mdiff;();0b;enlist `paths];
  :(raze idx[;0];raze idx[;1]);}

//Calculate shortest edit sequence from a to b
//Returns a list of snakes
// If the return is empty list, things are very, very wrong!
snakes:{[a;b;f]
  offset:maxv:(n:count a)+m:count b;
  v: (1+2*maxv)#0; //initialize array of V
  xp:x:0;yp:y:0;
  snodes:(1+2*maxv)#-1; 
  snakearr:(); /array of snakes - at most maxv entries since D in Myer's diff algorithm can't exceed maxv
  d:-1;
  while[maxv >= d+:1; //Constraint: when d is maxv, kmin is 0, kmax is 2*maxv => array access of v is safe since v is 1+2*maxv length
    kmin:(neg d) + offset; kmax: d + offset; k:-2 + kmin;
    while[kmax >= k+:2;
      $[(k = kmin) or ((k < kmax) and v[-1+k] < v[k+1]);
        x: v[kp:k+1];
        x: 1 + v[kp:-1+k]];
      yp:y:(xp:x)-(k - offset); //note "original diagonal k" is always (k - offset) because of array offsets here - to calculate y, we need original diagonal k
      //now find matches for a[x:] and b[y:]
      while[(x<n) and (y<m) and f[a[x];b[y]];x+:1;y+:1]; //f is boolean function on a[i],b[j]
      v[k]:x;
      if[(x >= n) and (y >= m);:snakearr,:enlist (snodes[kp];xp;yp;(y-yp))];
      snakearr,:enlist (snodes[kp];xp;yp;(y-yp)); 
      snodes[k]:-1 + count snakearr;
      ];
    ];
  :() //shouldn't ever get here - it is a logical bug if it gets here
  }

//Optimized version of lcs function - chops off common suffix and prefix before running
//LCS algorithm
lcsopt:{[a;b;f] 
  //find common prefix and common suffixes first - this is an optimization to reduce 
  //time for LCS - you may discard this if you prefer simplicity
  psl:{[x;y;f] pi: where not f'[x;(count x)#y]; pl: (count x)^first pi;c1:neg (count x)-   pl; si: where not f'[c1#x;c1#y]; sl: (neg c1)-0^1 + last si;(pl;sl)} . $[(count a)>count b; (b;a;f);(a;b;f)];
  a1: (neg psl[1]) _ psl[0] _ a; /chop off common prefixes and suffixes
  b1: (neg psl[1]) _ psl[0] _ b;
  //if a1 or b1 is empty after prefix/suffix chop-off,  the empty one is subsequence
  //of other. The non-empty one is delta. So, lcs is prefix+suffix - return - don't 
  //call lcs function as it works only on non-empty sequences
  $[(0=count a1) or 0=(count b1);:({[p;s;x] (p#x),(neg s)#x}[psl[0];psl[1];] each (til count a;til count b));
    idx:psl[0]+lcs[a1;b1;f]]; /offset the indices by length of chopped prefix 
  :((pi, (first idx),((neg psl[1])+count a) + si);((pi:til psl[0]), (last idx),((neg psl[1])+count b) + si:til psl[1]));
  }

//Wrapper to find mutations between two tables given s in sym column. c columns are used
//for mutation check - for example, price and size columns have very good quality signal 
//for trade mutation check
//Example: diffTables[t1;t2;`ABC;`price`size]
diffTables:{[t1;t2;s;c]
  i1: exec i from t1 where sym in s;
  i2: exec i from t2 where sym in s;
  a:flip (t1 i1) c;
  b:flip (t2 i2) c;
  il:lcs[a;b;{[x;y] all x=y}]; /replace with call to lcsopt instead if optimizing for big tables with long common prefix/suffixes
  dela: (til count a) except il[0]; //return delta indices, i.e., not a common subsequence in a
  delb: (til count b) except il[1]; //return delta indices, i.e., not a common subsequence in b
  :(i1 dela; i2 delb) //return the delta indices in original table
  }
