#ifndef _RAI_MAIN_PROC_H_
#define _RAI_MAIN_PROC_H_

/* Instantiate const data structures defined in proc.h */


#define GEN_DIM_ARRAY(name, type, kind, size, ...) static const word_t name##_dims[kind+1] = {__VA_ARGS__};
proc_for_in    (GEN_DIM_ARRAY)
proc_for_param (GEN_DIM_ARRAY)
proc_for_out   (GEN_DIM_ARRAY)
proc_for_si    (GEN_DIM_ARRAY)
proc_for_store (GEN_DIM_ARRAY)

#define GEN_INFO(__name, __type, __kind, __size, ...) {   \
        .name = (void*)#__name,                           \
        .dims = (void*)&__name##_dims[0],                 \
        .type = proc_type_##__type,                       \
},

static const struct proc_class_param info_in[]    = { proc_for_in    (GEN_INFO) {} };
static const struct proc_class_param info_out[]   = { proc_for_out   (GEN_INFO) {} };
static const struct proc_class_param info_state[] = { proc_for_si    (GEN_INFO) {} };
static const struct proc_class_param info_store[] = { proc_for_store (GEN_INFO) {} };

/* To allow element ref by name, info_param is a struct instead of an array. */
#define PARAM_BY_NAME(__name, ...) const struct proc_class_param __name;
struct proc_class_param_by_name {
    proc_for_param(PARAM_BY_NAME)
    const struct proc_class_param _end_;
};
static const struct proc_class_param_by_name info_param = {
    proc_for_param (GEN_INFO)  {}
};

#define GEN_INIT(__name, ...) .__name = proc_##__name##_init,
static const struct proc_si    init_state = { proc_for_si    (GEN_INIT) };
static const struct proc_store init_store = { proc_for_store (GEN_INIT) };

#define GEN_CONTROL(_param, _desc, _unit, _min, _max, _range, _curve) { \
            .desc = _desc,                                              \
            .unit = _unit,                                              \
            .param = &info_param._param - (const struct proc_class_param *)&info_param, \
            .map = { .s0 = _min,                                        \
                     .s1 = _max,                                        \
                     .range = _range,                                   \
                     .scale = proc_scale_##_curve }                     \
            },
static const struct proc_class_control info_control[] = { proc_for_control (GEN_CONTROL) {} };

#ifndef PROC_HEADER_ATTRIBUTE
#define PROC_HEADER_ATTRIBUTE
#endif

#ifndef PROC_HEADER_NAME
#define PROC_HEADER_NAME proc_info
#endif

#define PROC_STRINGIFY(x) #x

/* Start of binary file. */
const struct proc_class PROC_HEADER_ATTRIBUTE PROC_HEADER_NAME = {
    .magic      = PROC_MAGIC,
    .entry      = (void*)proc_loop,

    /* Detailed meta info */
    .info_state = (void*)info_state,
    .info_in    = (void*)info_in,
    .info_param = (void*)&info_param,
    .info_out   = (void*)info_out,
    .info_store = (void*)info_store,

    .info_control = (void*)info_control,

    .init_state = (void*)&init_state,
    .init_store = (void*)&init_store,

#if defined(PROC_NAME)
    .name = PROC_NAME,
#endif

#if defined(PROC_BUILD_STAMP)
    .build_stamp = PROC_BUILD_STAMP,
#endif

#if defined(PROC_VERSION)
    .version = PROC_VERSION,
#endif

};




#endif
