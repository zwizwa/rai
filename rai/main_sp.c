// Stand-alone loadable module.

/* CONTACT AUTHOR IF YOU DEPEND ON THE BINARY REPRESENTATION.
   IMPLEMENTATION IS IN FLUX - VERSIONS ARE NOT YET TAGGED PROPERLY.

   Once requirements are clear, this should stabilize. */

#define fast_fmod local_fmod
#define _ float

static inline _ local_fmod(_ x, _ range);
#include "prim.h"  // primitive functions
#include "rai.h"
#include PROC_FILE // generated code
static inline _ local_fmod(_ x, _ range) {
    _ mapped = x / range;
    _ frac = mapped - ((i_)mapped) + (x<0);
    return frac * range;
}






#define GEN_DIM_ARRAY(name, type, kind, size, ...) static const uintptr name##_dims[kind+1] = {__VA_ARGS__};
PROC(for_in)    (GEN_DIM_ARRAY)
PROC(for_param) (GEN_DIM_ARRAY)
PROC(for_out)   (GEN_DIM_ARRAY)
PROC(for_si)    (GEN_DIM_ARRAY)
PROC(for_store) (GEN_DIM_ARRAY)

// FIXME: type
#define GEN_INFO(__name, __type, kind, size, ...) {   \
        .name = (void*)#__name,                       \
        .dims = (void*)&__name##_dims[0],             \
        .type = rai_type_##__type                     \
},
const struct rai_info_param info_in[]    = { PROC(for_in)    (GEN_INFO) {} };
const struct rai_info_param info_param[] = { PROC(for_param) (GEN_INFO) {} };
const struct rai_info_param info_out[]   = { PROC(for_out)   (GEN_INFO) {} };
const struct rai_info_param info_state[] = { PROC(for_si)    (GEN_INFO) {} };
const struct rai_info_param info_store[] = { PROC(for_store) (GEN_INFO) {} };

#define GEN_CONTROL(_param, _desc, _unit, _min, _max, _range, _curve) { \
            _desc,                                                      \
            _unit,                                                      \
            offsetof(struct proc_param,_param)/sizeof(float),           \
            _min,                                                       \
            _max,                                                       \
            _range,                                                     \
            rai_scale_##_curve,                                         \
            },
const struct rai_info_control info_control[] = { PROC(for_control)(GEN_CONTROL) {} };


/* Start of binary file. */
struct rai_info __attribute__((section(".header"))) PROC(info) = {
    .magic      = RAI_MAGIC,
    .entry      = (void*)PROC(loop),

    /* Detailed meta info */
    .info_state = (void*)info_state,
    .info_in    = (void*)info_in,
    .info_param = (void*)info_param,
    .info_out   = (void*)info_out,
    .info_store = (void*)info_store,

    .info_control = (void*)info_control,

#if defined(RAI_BUILD_STAMP)
    .build_stamp = RAI_BUILD_STAMP,
#endif

#if defined(RAI_VERSION)
    .version = RAI_VERSION,
#endif

};





