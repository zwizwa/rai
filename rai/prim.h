#ifndef _PRIM_H_
#define _PRIM_H_

#include <stdio.h>

#define INLINE static inline __attribute__((always_inline))

// Primitive declaration
#define OP INLINE _

// Numeric type
#ifndef _
#define _ float
#endif

#ifndef i_
#define i_ signed int
#endif

// Arrays of arrays of numeric type
#define aa_ _ ** // restrict

// Branch predition hints
#ifndef unlikely
#define unlikely(x) __builtin_expect((x),0)
#endif

#ifndef likely
#define likely(x) __builtin_expect((x),1)
#endif


// PRIMITIVES

// Some ops can be float or int type, so use macros
#define p_add(a,b) ((a) + (b))
#define p_sub(a,b) ((a) - (b))
#define p_mul(a,b) ((a) * (b))
#define p_div(a,b) ((a) / (b))

#define p_and(a,b) ((a) & (b))
#define p_or(a,b)  ((a) | (b))
#define p_xor(a,b) ((a) ^ (b))
#define p_mod(a,b) ((a) % (b))

#define p_lt(a,b)  ((a) < (b))

#define p_if(c,a,b) ((c) ? (a) : (b))


/* Simple move.  Sometimes necessary to bridge type boundaries or
   allow room for coupling 2 binding through assignment parts
   (i.e. accumulator results that are also outputs). */
#define p_copy(a) (a)


#define p_floor_approx

/* Note that this is not correct for negative integers.
   for n < 0, it maps n -> n - 1

   The purpose of this function is to provide a basis for floating
   point modulo, i.e. splitting a number into integer + remainder \in
   [0,1].  Most of these algorithms perform properly if remainder = 1. */


INLINE i_ p_ifloor(_ a) {
    i_ i_truncate = (i_)a;
    i_ i_negative = a < 0;
    i_ i_floor = i_truncate - i_negative;
    return i_floor;
}

OP p_floor(_ a) {
    _ f_floor = (_)p_ifloor(a);
    return f_floor;
}


OP p_debug(_ a) {
    fprintf(stderr, "%f\n", (float)a);
    return a;
}



/* Side effects. */
#if 0
void p_swap (void ** restrict a, void ** restrict b) {
    void *tmp = *a;
    *a = *b;
    *b = tmp;
}
#endif

/* Elementary functions.  This requires dependency on math library,
   which doesn't work for the raw binary .sp format. */
#ifndef P_MATH
#include "math.h"
#define P_MATH(tag, ...) tag(__VA_ARGS__)
#endif

/* Non-primitive ops.  currently disabled. */
OP p_exp(_ a) { return P_MATH(exp, a); }
OP p_log(_ a) { return P_MATH(log, a); }
OP p_sin(_ a) { return P_MATH(sin, a); }
OP p_cos(_ a) { return P_MATH(cos, a); }
OP p_pow(_ a, _ b) { return P_MATH(pow, a, b); }

/* Tuple casting. */
// #define p_cast(dst,src,x) ((dst)(x))


#define p_size(a) ((sizeof(a))/(sizeof(a[0])))

#endif
