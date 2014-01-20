#ifndef _RAI_H_
#define _RAI_H_

#include <stdbool.h>
#include <math.h>     // host side depends on math.h, but prim.h does not.
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define PROC_CMD_INFO -1

typedef float  float32_t;
typedef double float64_t;
typedef unsigned long word_t; // pointer-sized int

// Used for run-time loading
#define RAI_MAGIC_SIZE 16
#define RAI_MAGIC "rai/x86-64/1    "

#define RAI_VERSION_SIZE 16


/* Processor metadata.

   The idea is to keep the description dumb and non-reduntant.
   Interpretation of the format can be abstracted in a library.

   proc_class is represented as structured data, where all pointers (to
   strings and substructures) are represented by a u64 file offset.

   // FIXME: separate .sp loading from in-memory access
*/


/* Lists in rai.h are implemented as sentinel-terminated arrays, where
   the sentinel is a 0-filled field the size of a native pointer. */

static inline int rai_list_end(const void *x) {
    return *((const void**)x) == NULL;
}

/* Implementation note: Compile time (type) lists are implemented as
   "functor macros".  This makes it easier to construct a collection
   of types and functions that abstract over types. */

/* Iterate a macro m over types. */
#define RAI_TYPES_FOR(m)                        \
    m(float32_t)                                \
    m(uint32_t)                                 \
    m(int32_t)

enum rai_type {
#define RAI_DEFINE_TYPE(T) rai_type_##T,
    RAI_TYPES_FOR(RAI_DEFINE_TYPE)
};




/* Bare-bones parameter. */
struct proc_class_param {
    const char *name;
    const word_t *dims;
    enum rai_type type;
};

/* Params that have an associated proc_class_control are GUI worthy, and
   contain more meta info. */
enum rai_scale {
    rai_scale_lin = 0,  // s0 + (s1 - s0) * v
    rai_scale_log = 1,  // s0 * (s1 / s0) ^ v
    rai_scale_slog = 2, // s0 * (v/(1-v)) ^ s1   "squeezed log / stretched exp"
};
struct proc_class_control_map {
    double s0; // minimum | center
    double s1; // maximum | exponent
    double range;
    enum rai_scale scale;
};
struct proc_class_control {
    const char *desc;
    const char *unit;
    const struct proc_class_param *param;
    struct proc_class_control_map map;
};


/* Defined in .g.h */
struct proc_si;
struct proc_in;
struct proc_param;
struct proc_out;
struct proc_store;

typedef void (*proc_class_run)(
    struct proc_si    * restrict state,  /* double buffer */
    struct proc_in    * restrict in,
    struct proc_param * restrict param,
    struct proc_out   * restrict out,
    struct proc_store * restrict store,
    unsigned int nb_samples
);


/* Reified signal processor class.
   defined in main_sp.c from proc_ macros in a .g.h
   The .g.h is generated from stream code by ai-array-c.rkt */

struct proc_class {
    uint8_t magic[RAI_MAGIC_SIZE];
    uint8_t version[RAI_VERSION_SIZE];
    proc_class_run entry;

    /* Node type info. */
    const struct proc_class_param *info_state;
    const struct proc_class_param *info_in;
    const struct proc_class_param *info_param;
    const struct proc_class_param *info_out;
    const struct proc_class_param *info_store;

    /* param UI info. */
    const struct proc_class_control *info_control;

    /* Initial values. */
    const struct proc_state *init_state;
    const struct proc_store *init_store;

    /* Misc info */
    uint32_t build_stamp;
    uint32_t __reserved;

};





/* To keep things simple, all casts are to/from a single numeric type. */
#define RAI_NUMBER_T double
RAI_NUMBER_T rai_get_number(enum rai_type t, const void *rai_src);
void         rai_set_number(enum rai_type t, void *rai_dst, RAI_NUMBER_T val);






int proc_class_param_nb_elements(const struct proc_class_param *pi);
int proc_class_param_list_size(const struct proc_class_param *pi);
int proc_class_param_alloc_size(const struct proc_class_param *pi);



struct proc_class_preset {
    uint8_t  magic[RAI_MAGIC_SIZE];
    uint32_t header_bytes;
    uint32_t timestamp;
    uint32_t payload_bytes;
};


/* Maps v \in [0,1] to the control parameter's user feedback scale. */
float proc_class_control_interpolate(const struct proc_class_control_map *p, float v);

/* Load .sp class */
struct proc_class *rai_load_sp(const char *filename);


struct proc_instance;

struct proc_instance *proc_instance_new(const struct proc_class *info,
                                        const struct proc_instance *proto);

void proc_instance_free(struct proc_instance *p);

void proc_instance_run(struct proc_instance *p,
                       struct proc_in  * restrict in,
                       struct proc_out * restrict out,
                       int n);

int proc_instance_nb_control(struct proc_instance *p);


void proc_instance_preset_save(const struct proc_instance *p, const char *filename);
void proc_instance_preset_load(const struct proc_instance *p, const char *filename, int index);

void proc_instance_reset_state(struct proc_instance *p);
void proc_instance_reset_param(struct proc_instance *p);

int proc_instance_find_param(struct proc_instance *p, const char *name);
int proc_instance_find_control(struct proc_instance *p, int c);
void proc_instance_set_param(struct proc_instance *p, int index, RAI_NUMBER_T val);

RAI_NUMBER_T proc_instance_get_param(struct proc_instance *p, int index);

/* Synth stuff */
struct rai_voice {
    uint32_t nb;
    float32_t *gate;
    float32_t *freq;
    uint32_t next;
};


// for convenience
void rai_voice_init(struct rai_voice *v, int nb, float *gate, float *freq);
int rai_voice_init_from_proc(struct proc_instance *p, struct rai_voice *v);
void rai_voice_off(struct rai_voice *v, float freq);
void rai_voice_on(struct rai_voice *v, float freq);
float rai_midi_to_freq(int midi);





/* DEBUG */
typedef void (*rai_log)(const char *msg, ...);
void rai_print_info(const struct proc_class *ri, rai_log log);

#ifdef __cplusplus
}
#endif


#endif


