/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 * crazy xmacro header
 *
 ***************************************************************************/

#ifndef TYPE_T
#error "TYPE_T not defined"
#endif

#ifndef TYPE_FUNC_PREFIX
#define TYPE_FUNC_PREFIX TYPE_T
#endif

// don't ask, but:
// 1. TYPE_FUNC_PREFIX => TFP (an argument)
// 2. pass TFP into 3 evals the argument, turning it into e.g. foo
// 3. foo concatted with func
#ifndef ADECL
#define ADECL3(FUNC,PREFIX) PREFIX ## _ ## FUNC
#define ADECL2(FUNC,PREFIX) ADECL3(FUNC,PREFIX)
#define ADECL(FUNC) ADECL2(FUNC,TYPE_FUNC_PREFIX)
#define S_ADECL(FUNC) ADECL(FUNC) ## _s
#endif

typedef void ADECL(destroyelt_fp)(TYPE_T *helt);
typedef int ADECL(cmp_fp)(TYPE_T *hlhs,TYPE_T *hrhs, void *ctxt);
typedef void ADECL(foreach_ctxt_fp)(TYPE_T *ht, void *ctxt);
typedef void ADECL(foreach_fp)(TYPE_T *ht);

TYPE_T* ADECL(init)(void *mem, size_t mem_size, U32 flags);
TYPE_T* ADECL(create)(int capacity);
void ADECL(destroy)(TYPE_T** ha, ADECL(destroyelt_fp) *fp);
int ADECL(size)(TYPE_T const * const * ha);
void ADECL(setsize)(TYPE_T** ha, int size);
TYPE_T* ADECL(push)(TYPE_T **ha);
TYPE_T* ADECL(pushn)(TYPE_T **ha, int n);
TYPE_T* ADECL(pushfront)(TYPE_T **ha);
int ADECL(push_by_cp)(TYPE_T **ha, TYPE_T b);
TYPE_T* ADECL(pop)(TYPE_T **ha);
TYPE_T* ADECL(top)(TYPE_T **ha);
void ADECL(cp)(TYPE_T **hdest,TYPE_T const * const *hsrc,int n);
void ADECL(cp_raw)(TYPE_T **hdest,TYPE_T const *s,int n); // second arg may not be an Array
void ADECL(insert)(TYPE_T **hdest, TYPE_T *src, int i, int n);
void ADECL(append)(TYPE_T **hdest, TYPE_T *src, int n);
void ADECL(rm)(TYPE_T **ha, int offset, int n);
int ADECL(find)(TYPE_T **ha, TYPE_T *b, ADECL(cmp_fp) *cmp, void *ctxt);
void ADECL(foreach_ctxt)(TYPE_T **ha, ADECL(foreach_ctxt_fp) *fp, void *ctxt);
void ADECL(foreach)(TYPE_T **ha, ADECL(foreach_fp) *fp);

