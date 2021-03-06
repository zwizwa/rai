#ifndef _PRIM_H_
#define _PRIM_H_

#include <stdio.h>

#ifndef INLINE
#define INLINE static inline __attribute__((always_inline))
#endif

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


#define p_integer(op,a,b) (((i_ )(a)) op ((i_)(b)))

// logic
#define p_and(a,b)  p_integer(&&,a,b)
#define p_or(a,b)   p_integer(||,a,b)
#define p_not(a)    (!((i_)(a)))

// bitwise
#define p_band(a,b)  p_integer(&,a,b)
#define p_bor(a,b)   p_integer(|,a,b)
#define p_bxor(a,b)  p_integer(^,a,b)

#define p_mod(a,b)  p_integer(%,a,b)
#define p_quot(a,b) p_integer(/,a,b)
#define p_sal(a,b)  p_integer(<<,a,b)
#define p_sar(a,b)  p_integer(>>,a,b)


#define p_lt(a,b)  ((a) < (b))

#define p_if(c,a,b) ((c) ? (a) : (b))


/* Simple move.  Sometimes necessary to bridge type boundaries or
   allow room for coupling 2 binding through assignment parts
   (i.e. accumulator results that are also outputs). */
#define p_copy(a) (a)


#define p_floor_approx

/* Note that this is based on float to int truncation, so is not the
   same as the floor() function from math.h

   For negative integers, the map is n -> n - 1.

   In practical use in numerical algorithms this difference is OK.
   Think of it this way: in the presence of noise, there is no real
   difference between > and >=.
*/

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
