\p 2001
.u.insert:{[x;y] t1:.z.P; insert[x;y]; t2:.z.P; 0N!"Time elapsed (s): ", string (`int$(t2-t1))%xexp[10;9];}
t:([] a:`int$();b:`int$();c:`int$());
