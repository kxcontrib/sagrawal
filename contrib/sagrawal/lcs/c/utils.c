#include <inttypes.h>
#include <string.h>
#include "utils.h"

/* Function to validate two nested lists x and y of depth 1 have same element count for
** x[0],y[0] etc.
*/
C validate(K x,K y){
  if(x->t!=0 || y->t!=0) R 0;
  I i=0; 
  //if here, general lists - validate for shape
  for(;i<-1+x->n;i++)
    if(kK(x)[i]->n != kK(x)[i+1]->n) R 0;
  for(i=0;i<-1+y->n;i++)
    if(kK(y)[i]->n != kK(y)[i+1]->n) R 0;
  i=0; if(kK(x)[i]->n != kK(y)[i]->n) R 0;
  R 1; //since lists are of same shape, return 1
}

K test(K x,K y){
  R ki(validate(x,y));
}

//quick vector equality comparison - assumption is x,y length are very small, typically 2 or 3
static inline uint8_t __attribute__((always_inline)) equalv(K x,K y,I t){
  I i=0;
  switch(t){
    case KB: case KC:
    case KG: for(;i<x->n && (kG(x)[i] == kG(y)[i]);i++);
             break;
    case KH: for(;i<x->n && (kH(x)[i] == kH(y)[i]);i++);
             break;
    case KM: case KD: case KU: case KV: case KT: case KI:
             for(;i<x->n && (kI(x)[i] == kI(y)[i]);i++);
             break;
    case KP: case KN:
    case KJ: for(;i<x->n && (kJ(x)[i] == kJ(y)[i]);i++);
             break;
    case KE: for(;i<x->n && (kE(x)[i] == kE(y)[i]);i++);
             break;
    case KF: for(;i<x->n && (kF(x)[i] == kF(y)[i]);i++);
             break;
    case KS: for(;i<x->n && (kS(x)[i] == kS(y)[i]);i++);
             break;
    #if KXVER>=3
    case UU: for(;i<x->n && (memcmp(x->G0 + i*sizeof(U),y->G0 + i*sizeof(U),sizeof(U)) == 0);i++);
             break;
    #endif
    default: R 0;
  }
  R (i==x->n);
}

//this function does the job of filtering out non-conforming lists by returning 0 for non-atoms
static inline uint8_t __attribute__((always_inline)) equala(K x,K y,I t){
  switch(t){
    case -KB: case -KC: 
    case -KG: R (x->g == y->g);
    case -KH: R (x->h == y->h);
    case -KM: case -KD: case -KU: case -KV: case -KT: case -KI: R (x->i == y->i);
    case -KP: case -KN: case -KJ: R (x->j == y->j);
    case -KE: R (x->e == y->e);
    case -KF: R (x->f == y->f);
    case -KS: R (x->s == y->s);
    #if KXVER>=3
    case -UU: R (memcmp(x->G0,y->G0,sizeof(U)) == 0);
    #endif
  }
  R 0;
}

//NOTE: This function works only for flat lists, not nested lists - on nested lists, it will
// return 0 - see equala (atomic equality) function that is called
static inline uint8_t __attribute__((always_inline)) equalh(K x,K y,I tx,I ty){
  if(tx!=ty) R 0;
  if(tx>0) R equalv(x,y,tx); //vectorized equality test
  else if(tx<0) R equala(x,y,tx); //atomic equality test
  //if here, it is a general list of atoms - we don't allow list of lists
  I i=0;
  uint8_t equal=1;
  //if here, type 0 list - must check for type-equality before comparison
  for(;i<x->n;i++) equal = equal && kK(x)[i]->t == kK(y)[i]->t && equala(kK(x)[i],kK(y)[i],kK(x)[i]->t); 
  R equal;
}

//general list comparison
I cmpg(K x,K y,I i,I j){
  I ip=i;
  for(;(i<x->n) && (j<y->n) && equalh(kK(x)[i],kK(y)[j],kK(x)[i]->t,kK(y)[i]->t);i++,j++);
  R i-ip;
}

//Vectorized comparison
I cmpv(K x,K y,I i,I j){
  I ip=i;
  switch(x->t){
    case KB: case KC: case KG: 
             for(;i<x->n && j<y->n && (kG(x)[i] == kG(y)[j]);i++,j++);
             break;
    case KH: for(;i<x->n && j<y->n && (kH(x)[i] == kH(y)[j]);i++,j++);
             break;
    case KM: case KD: case KU: case KV: case KT: case KI: 
             for(;i<x->n && j<y->n && (kI(x)[i] == kI(y)[j]);i++,j++);
             break;
    case KP: case KN: case KJ: for(;i<x->n && j<y->n && (kJ(x)[i] == kJ(y)[j]);i++,j++);
             break;
    case KE: for(;i<x->n && j<y->n && (kE(x)[i] == kE(y)[j]);i++,j++);
             break;
    case KF: for(;i<x->n && j<y->n && (kF(x)[i] == kF(y)[j]);i++,j++);
             break;
    case KS: for(;i<x->n && j<y->n && (kS(x)[i] == kS(y)[j]);i++,j++);
             break;
    #if KXVER>=3  
    case UU: for(;i<x->n && j<y->n && (memcmp(x->G0 + i*sizeof(U),y->G0 + j*sizeof(U),sizeof(U)) == 0);i++,j++);
             break;
    #endif
    default: R 0;
   }
  R i-ip;
}
