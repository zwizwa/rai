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


#define GEN_DIM_ARRAY(name, type, kind, size, ...) static const word_t name##_dims[kind+1] = {__VA_ARGS__};
proc_for_in    (GEN_DIM_ARRAY)
proc_for_param (GEN_DIM_ARRAY)
proc_for_out   (GEN_DIM_ARRAY)
proc_for_si    (GEN_DIM_ARRAY)
proc_for_store (GEN_DIM_ARRAY)





#define GEN_INFO(__name, __type, kind, size, ...) {   \
        .name = (void*)#__name,                       \
        .dims = (void*)&__name##_dims[0],             \
        .type = rai_type_##__type,                    \
},

const struct rai_info_param info_in[]    = { proc_for_in    (GEN_INFO) {} };
const struct rai_info_param info_out[]   = { proc_for_out   (GEN_INFO) {} };
const struct rai_info_param info_state[] = { proc_for_si    (GEN_INFO) {} };
const struct rai_info_param info_store[] = { proc_for_store (GEN_INFO) {} };

/* To allow element ref by name, info_param is a struct instead of an array. */
#define PARAM_BY_NAME(__name, ...) const struct rai_info_param __name;
struct rai_info_param_by_name {
    proc_for_param(PARAM_BY_NAME)
    const struct rai_info_param _end_;
};
const struct rai_info_param_by_name info_param = {
    proc_for_param (GEN_INFO)  {}
};


#define GEN_INIT(__name, ...) .__name = proc_##__name##_init,
const struct proc_param init_param = { proc_for_param (GEN_INIT) };
const struct proc_si    init_state = { proc_for_si    (GEN_INIT) };
const struct proc_store init_store = { proc_for_store (GEN_INIT) };


#define GEN_CONTROL(_param, _desc, _unit, _min, _max, _range, _curve) { \
            .desc = _desc,                                              \
            .unit = _unit,                                              \
            .param = &info_param._param,                                \
            .map = { .s0 = _min,                                        \
                     .s1 = _max,                                        \
                     .range = _range,                                   \
                     .scale = rai_scale_##_curve }                      \
            },
const struct rai_info_control info_control[] = { proc_for_control (GEN_CONTROL) {} };


/* Start of binary file. */
struct rai_info __attribute__((section(".header"))) proc_info = {
    .magic      = RAI_MAGIC,
    .entry      = (void*)PROC(loop),

    /* Detailed meta info */
    .info_state = (void*)info_state,
    .info_in    = (void*)info_in,
    .info_param = (void*)&info_param,
    .info_out   = (void*)info_out,
    .info_store = (void*)info_store,

    .info_control = (void*)info_control,

    .init_param = (void*)&init_param,
    .init_state = (void*)&init_state,
    .init_store = (void*)&init_store,


#if defined(RAI_BUILD_STAMP)
    .build_stamp = RAI_BUILD_STAMP,
#endif

#if defined(RAI_VERSION)
    .version = RAI_VERSION,
#endif

};





