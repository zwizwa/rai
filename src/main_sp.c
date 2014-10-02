// Stand-alone loadable module.

// .sp modules don't have a libc dependency, so need a local def
//#define fast_fmod local_fmod

#define _ float

//static inline _ local_fmod(_ x, _ range);
#include "prim.h"  // primitive functions
#include "proc.h"
#include PROC_FILE // generated code

//static inline _ local_fmod(_ x, _ range) {
//    _ mapped = x / range;
//    _ frac = mapped - ((si_)mapped) + (x<0);
//    return frac * range;
//}

#define PROC_HEADER_ATTRIBUTE __attribute__((section(".header")))
#include "main_proc.h" // data structure instantiation

