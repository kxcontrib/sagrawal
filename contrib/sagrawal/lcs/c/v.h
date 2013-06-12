#include <inttypes.h>
#include <stdlib.h>

#define SCALING 1.5
#define CONCAT(x,y) x ## y
#define DARRAY(name, type)\
typedef struct {\
  size_t size;\
  size_t cap;\
  type* vec;\
} name;\
\
static size_t CONCAT(name,init)(name *v, size_t size){\
  v->size = 0;\
  v->cap = 0;\
  v->vec = (type*) malloc(size*(sizeof(type)));\
  if( v->vec == NULL) return 0;\
  v->cap = size;\
  return size;\
}\
\
static void CONCAT(name,insert)(name *v, type e){\
  if(v->vec == NULL) return;\
  if(v->size == v->cap){\
    v->cap *= SCALING;\
    v->vec = (type*) realloc(v->vec, v->cap * sizeof(type));\
    if(v->vec == NULL){\
      v->size=0;\
      v->cap=0;\
      return;\
      }\
  }\
  v->vec[v->size++] = e;\
}\
\
static int inline CONCAT(name,size)(name *v) {\
  if(v->vec == NULL) return 0;\
  else return v->size;\
}\
\
static int inline CONCAT(name,cap)(name *v){\
  if(v->vec == NULL) return 0;\
  else return v->cap;\
}\
\
static void CONCAT(name,free)(name *v){\
  v->size=0;\
  v->cap=0;\
  free(v->vec);\
  v->vec=NULL;\
}
