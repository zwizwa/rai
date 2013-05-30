// Stand-alone loadable module.

#define fast_fmod local_fmod
#define _ float

static inline _ local_fmod(_ x, _ range);
#include "prim.h"  // primitive functions
#include PROC_FILE // generated code
static inline _ local_fmod(_ x, _ range) {
    _ mapped = x / range;
    _ frac = mapped - ((i_)mapped) + (x<0);
    return frac * range;
}

#include "rai.h"




#define GEN_DIM_ARRAY(name, kind, size, ...) static const u32 name##_dims[kind+1] = {__VA_ARGS__};
PROC(for_in)    (GEN_DIM_ARRAY)
PROC(for_param) (GEN_DIM_ARRAY)
PROC(for_out)   (GEN_DIM_ARRAY)
PROC(for_si)    (GEN_DIM_ARRAY)
PROC(for_store) (GEN_DIM_ARRAY)

#define GEN_INFO(name, kind, size, ...) {(void*)#name, (void*)&name##_dims[0]},
const struct rai_info_param info_in[]    = { PROC(for_in)    (GEN_INFO) {} };
const struct rai_info_param info_param[] = { PROC(for_param) (GEN_INFO) {} };
const struct rai_info_param info_out[]   = { PROC(for_out)   (GEN_INFO) {} };
const struct rai_info_param info_state[] = { PROC(for_si)    (GEN_INFO) {} };
const struct rai_info_param info_store[] = { PROC(for_store) (GEN_INFO) {} };

#define GEN_CONTROL(_param, _desc, _unit, _min, _max, _range, _curve) { \
            offsetof(struct proc_param,_param)/sizeof(float),           \
            _desc,                                                      \
            _unit,                                                      \
            rai_scale_##_curve,                                         \
            _min,                                                       \
            _max,                                                       \
            _range,                                                     \
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





