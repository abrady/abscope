/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#ifndef ABARRAY_H
#define ABARRAY_H

#include "abutil.h"

// ----------------------------------------
// void*

#define TYPE_T void*
#define TYPE_FUNC_PREFIX ap
#include "abarrayx.h"
#undef TYPE_T
#undef TYPE_FUNC_PREFIX

#define ap_push ap_push_by_cp // i.e. copy the pointer

// ----------------------------------------
// char*

#define TYPE_T char*
#define TYPE_FUNC_PREFIX astr
#include "abarrayx.h"
#undef TYPE_T
#undef TYPE_FUNC_PREFIX

#define astr_push astr_push_by_cp // i.e. copy the pointer

// ----------------------------------------
// int

#define TYPE_T int
#define TYPE_FUNC_PREFIX aint
#include "abarrayx.h"
#undef TYPE_T
#undef TYPE_FUNC_PREFIX

#define aint_push aint_push_by_cp // i.e. copy the pointer
#define aint_destroy(P) aint_destroy(P,NULL)

#endif //ABARRAY_H
