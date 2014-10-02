#ifndef _PD_UTIL_
#define _PD_UTIL_

/* The PD bindings use two approaches:
   - Inline processor / meta code in a single object
   - A sp_host object for loading .sp modules

   The .sp module approach is very useful for testing, but is fairly
   ad-hoc, so for distribution we keep the ability to perform static
   linking, building one pd object per synth object.

   To enable better library packaging in creb, shared functionality is
   collected here, leaving it up to the implementer to use static
   inline or proper library functions.
*/

#ifndef RAI_PD_INLINE
#define RAI_PD_INLINE static inline
#endif


#ifdef PROC_FILE
#define HAVE_STATIC 1
#include PROC_FILE // generated code
/* Assume the prefix is default "proc_"
   We just work with flat arrays, not structured grid data. */
#define NB_PARAM  proc_size_param
#define NB_IN     proc_size_in
#define NB_OUT    proc_size_out
#define NB_STATE  proc_size_state
/* Create synth voice allocator if freq and gate params are defined as
   1-dim arrays.  */
#if defined(proc_param_voice_freq) && \
    defined(proc_param_voice_gate) && \
    proc_param_voice_freq == 1 && \
    proc_param_voice_gate == 1
#define HAVE_SYNTH 1
#else
#define HAVE_SYNTH 0
#endif

#else
#define HAVE_STATIC 0
#define HAVE_SYNTH 1
#endif

#include "rai.h"



#include "m_pd.h"
#include "g_canvas.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define FLOAT(f)      {.a_type = A_FLOAT,  .a_w = {.w_float  = (f)}}
#define SYMBOL(s)     {.a_type = A_SYMBOL, .a_w = {.w_symbol = (s)}}
#define nb_el(x) (sizeof(x)/sizeof(x[0]))



RAI_PD_INLINE t_symbol *gensym_n(const char *name, int n) {
    char sname[strlen(name)+20];
    snprintf(sname, sizeof(sname), "%s%d", name, n);
    sname[sizeof(sname)-1] = 0;
    return gensym(sname);
}

RAI_PD_INLINE t_symbol *slider_label(struct rai_info_control *p) {
    int buflen = 2 + strlen(p->desc) + strlen(p->unit);
    char buf[buflen];
    strcpy(buf, p->desc);
    strcat(buf, "/");
    strcat(buf, p->unit);
    /* Symbols can't have whitespace. */
    for (int j=0; j<buflen; j++) {
        if (isspace(buf[j])) buf[j] = '_';
    }
    return gensym(buf);
}


#endif //  _PD_UTIL_
