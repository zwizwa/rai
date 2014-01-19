#include "rai.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>


int rai_info_param_nb_elements(const struct rai_info_param *pi) {
    int nb_elements = 1;
    for (int i=0; !rai_list_end(&(pi->dims[i])); i++) {
        nb_elements *= pi->dims[i];
    }
    return nb_elements;
}

int rai_info_param_list_size(const struct rai_info_param *pi) {
    int i;
    for (i = 0; !rai_list_end(&pi[i]); i++);
    return i;
}
static int rai_info_control_list_size(const struct rai_info_control *ci) {
    int i;
    for (i = 0; !rai_list_end(&ci[i]); i++);
    return i;
}



static int rai_min(int x, int y) { return x < y ? x : y; }

static int rai_type_sizeof(enum rai_type t) {
#define RAI_TYPE_CASE_SIZEOF(_t) case rai_type_##_t: return sizeof(_t);
    switch(t) {
        RAI_TYPES_FOR(RAI_TYPE_CASE_SIZEOF)
    default: return 0;
    }
}

int rai_info_param_alloc_size(const struct rai_info_param *pi) {
    int bytes = 0;
    for (int i = 0; !rai_list_end(&pi[i]); i++) {
        bytes += rai_info_param_nb_elements(&pi[i])
            *rai_type_sizeof(pi[i].type);
    }
    return bytes;
}

RAI_NUMBER_T rai_get_number(enum rai_type t, const void *p) {
#define RAI_CASE_GET_NUMBER(_RAI_T_) case rai_type_##_RAI_T_: return (RAI_NUMBER_T)*((_RAI_T_*)p);
    switch(t) {
        RAI_TYPES_FOR(RAI_CASE_GET_NUMBER)
    default:
        return (RAI_NUMBER_T)0;
    }
}
void rai_set_number(enum rai_type t, void *p, RAI_NUMBER_T val) {
#define RAI_CASE_SET_NUMBER(_RAI_T_) case rai_type_##_RAI_T_: *((_RAI_T_*)p) = (_RAI_T_)val; break;
    switch(t) {
        RAI_TYPES_FOR(RAI_CASE_SET_NUMBER)
    }
}


#if 0
static inline void * _memcpy(void *dst, void *src, int bytes) {
    printf("memcpy(%p,%p,%d)\n", dst, src, bytes);
    return memcpy(dst, src, bytes);
}
#endif



/* Initialize from prototype if available, else use defaults.  This is
   useful for state-preserving code updates.  Note that consistency of
   the structs cannot be verified here, only size. */
static void rai_param_init(const struct rai_info_param *pi,
                           void *new, const void *old, int old_max_bytes) {
    int byte_offset = 0;
    for (int i = 0; !rai_list_end(&pi[i]); i++) {
        enum rai_type t = pi[i].type;
        int nb_elements =  rai_info_param_nb_elements(&pi[i]);
        int byte_size = rai_type_sizeof(t) * nb_elements;

        if (byte_size + byte_offset <= old_max_bytes) {
            memcpy(new + byte_offset, old + byte_offset, byte_size);
        }
        byte_offset += byte_size;
    }
}

struct rai_proc *rai_proc_new(const struct rai_info *info,
                              const struct rai_proc *proto) {
    struct rai_proc *p = malloc(sizeof(*p));
    p->info = info;

    /* Allocate and build indexing data. */
    p->nb_param   = rai_info_param_list_size(info->info_param);
    p->nb_control = rai_info_control_list_size(info->info_control);
    p->param_offset = malloc(p->nb_param * sizeof(p->param_offset[0]));
    p->param_nb_el  = malloc(p->nb_param * sizeof(p->param_nb_el[0]));

    for (int i = 0, byte_offset = 0; i < p->nb_param; i++) {
        const struct rai_info_param *pi = &info->info_param[i];
        p->param_offset[i] = byte_offset;
        p->param_nb_el[i]  = rai_info_param_nb_elements(pi);
        int byte_size = rai_type_sizeof(pi->type) * p->param_nb_el[i];
        byte_offset += byte_size;
    }

    /* Allocate in 1 chunk to improve locality. */
    int size_state = rai_info_param_alloc_size(info->info_state);
    int size_param = rai_info_param_alloc_size(info->info_param);
    int size_store = rai_info_param_alloc_size(info->info_store);

    int buf_size = 2 * size_state + size_param + size_store;
    void *buf = calloc(1, buf_size);
    p->param    = buf; buf += size_param;
    p->state    = buf; buf += (2 * size_state);
    p->store    = buf;

    /* Initialize from defaults. */
    rai_param_init(info->info_param, p->param, info->init_param, size_param);
    rai_param_init(info->info_store, p->store, info->init_store, size_store);
    rai_param_init(info->info_state, p->state, info->init_state, size_state);

    /* Copy from proto or init struct. */
    if (proto) {
        int proto_size_state = 0;
        int proto_size_param = 0;
        int proto_size_store = 0;
        if (proto) {
            const struct rai_info *pri = proto->info;
            proto_size_state = rai_info_param_alloc_size(pri->info_state);
            proto_size_param = rai_info_param_alloc_size(pri->info_param);
            proto_size_store = rai_info_param_alloc_size(pri->info_store);
        }
        rai_param_init(info->info_param, p->param, proto->param, rai_min(size_param, proto_size_param));
        rai_param_init(info->info_store, p->store, proto->store, rai_min(size_store, proto_size_store));
        rai_param_init(info->info_state, p->state, proto->state, rai_min(size_state, proto_size_state));
    }
    return p;
}
void rai_proc_free(struct rai_proc *p) {
    if (p->param) {
        free(p->param);
        p->param = NULL;
        p->state = NULL;
    }
    free(p);
}

void rai_proc_reset_state(struct rai_proc *p) {
    rai_param_init(p->info->info_state, p->state, NULL, 0);
}
void rai_proc_reset_param(struct rai_proc *p) {
    rai_param_init(p->info->info_param, p->param, NULL, 0);
}

void rai_proc_preset_save(const struct rai_proc *p, const char *filename) {
    FILE *f = fopen(filename, "a");
    if (!f) return;
    struct rai_info_preset hdr = {
        .magic = "preset",
        .header_bytes = sizeof(hdr),
        .timestamp = time(NULL),
        .payload_bytes = rai_info_param_alloc_size(p->info->info_param),
    };
    fwrite(&hdr, 1, sizeof(hdr), f);
    fwrite(p->param, 1, hdr.payload_bytes, f);
    fclose(f);
}


void rai_proc_preset_load(const struct rai_proc *p, const char *filename, int index) {
    FILE *f = fopen(filename, "r");
    if (!f) return;
    struct rai_info_preset hdr;
    do {
        int nb_bytes = rai_info_param_alloc_size(p->info->info_param);
        if (sizeof(hdr) != fread(&hdr, 1, sizeof(hdr), f)) goto error;
        if (nb_bytes != hdr.payload_bytes) goto error;
        if (nb_bytes != fread(p->param, 1, nb_bytes, f)) goto error;
    } while(index--);
  error:
    fclose(f);
}

int rai_proc_find_param(struct rai_proc *p, const char *name) {
    for (int i = 0; !rai_list_end(&p->info->info_param[i]); i++) {
        if (!strcmp(p->info->info_param[i].name, name)) return i;
    }
    return -1;
}

int rai_proc_find_control(struct rai_proc *p, int c) {
    if ((c < 0) || (c >= p->nb_control)) return -1;
    return p->info->info_control[c].param - p->info->info_param;
}

/* Set will set the entire grid.  Get will only pick the first element. */
void rai_proc_set_param(struct rai_proc *p, int index, RAI_NUMBER_T val) {
    if ((index < 0) || (index >= p->nb_param)) {
        // printf("! p_%d\n", index);
        return;
    }
    void *param_buf = (void*)p->param + p->param_offset[index];
    enum rai_type t = p->info->info_param[index].type;
    // printf("param_buf %p\n", param_buf);

    rai_set_number(t, param_buf, val);
    return;

#if 0
    int nb = p->param_nb_el[index];
    int base_bytes = rai_type_sizeof(t);
    for (int i=0; i<nb; i++) {
        // printf("p_%d <- %f\n", index, val); 
        rai_set_number(t, param_buf, val);
        param_buf += base_bytes;
    }
#endif
}
RAI_NUMBER_T rai_proc_get_param(struct rai_proc *p, int index) {
    if ((index < 0) || (index >= p->nb_param)) {
        // printf("! p_%d\n", index);
        return 0;
    }
    void *param_buf = (void*)p->param + p->param_offset[index];
    // printf("param_buf %p\n", param_buf);
    return rai_get_number(p->info->info_param[index].type, param_buf);
}


int rai_voice_init_from_proc(struct rai_proc *p, struct rai_voice *v) {
    const struct rai_info_param *ip = p->info->info_param;
    int gate_index = rai_proc_find_param(p, "voice_gate"); if (gate_index < 0) return -1;
    int freq_index = rai_proc_find_param(p, "voice_freq"); if (freq_index < 0) return -1;
    if (ip[gate_index].type != rai_type_float32_t) return -1;
    if (ip[freq_index].type != rai_type_float32_t) return -1;
    int nb =  p->param_nb_el[gate_index];
    if (nb != p->param_nb_el[freq_index]) return -1;
    rai_voice_init(v, nb,
                   (void*)p->param + p->param_offset[gate_index],
                   (void*)p->param + p->param_offset[freq_index]);
    return 0;
}
void rai_voice_on(struct rai_voice *v, float freq) {
    v->gate[v->next] = 1;
    v->freq[v->next] = freq;
    v->next = (v->next + 1) % v->nb;
}
void rai_voice_off(struct rai_voice *v, float freq) {
    /* Turn off 0 or 1 notes, start from oldest. */
    for (int i=1; i<=v->nb; i++) {
        int j = (v->nb + v->next - i) % v->nb;
        if (v->freq[j] == freq) {
            v->gate[j] = 0;
            break;
        }
    }
}
float rai_midi_to_freq(int midi) {
    // 69 -> 440
    float fmidi = midi-69;
    return 440 * pow(2, fmidi/12);
}


/* DEBUG */
static void rai_print_info_param(const char *tag,
                                 const struct rai_info_param *pi,
                                 rai_log log) {
    log("%s %d:\n", tag, rai_info_param_alloc_size(pi));
    for (int i = 0; !rai_list_end(&pi[i]); i++) {
        log("\t%d %s", i, pi[i].name);
        const word_t *dims = pi[i].dims;
        int size = 1;
        for (int d = 0; dims[d]; d++) {
            log(" %d", dims[d]);
            size *= dims[d];
        }
        log(" (size=%d)\n", size);
    }
}
static void rai_print_info_control(const char *tag,
                                   const struct rai_info_control *pi,
                                   rai_log log) {
    log("%s\n", tag);
    for (int i = 0; !rai_list_end(&pi[i]); i++) {
        log("\t%s(%s) [%f,%f]\n",
            pi[i].desc,
            pi[i].unit,
            pi[i].map.s0,
            pi[i].map.s1);
    }
}
void rai_print_info(const struct rai_info *ri, rai_log log) {
    if (!log) log = (rai_log)printf;
    rai_print_info_param("param", ri->info_param, log);
    rai_print_info_param("in",    ri->info_in, log);
    rai_print_info_param("out",   ri->info_out, log);
    rai_print_info_param("state", ri->info_state, log);

    rai_print_info_control("control", ri->info_control, log);
}


