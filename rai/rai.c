#include "rai.h"
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/* Relocation: every date structure pointer needs to be patched from
   rai_info relative to absolute memory location. */
static void link_ptr(struct rai_info *ri, void *pointer) {
    long offset = *(long*)pointer;
    char *linked = (char*)ri + offset;
    *(void **)pointer = linked;
}
static void link_info_param(struct rai_info *ri, struct rai_info_param **rpp) {
    link_ptr(ri, rpp);
    struct rai_info_param *rp = *rpp;
    for (int i = 0; !rai_list_end(&rp[i]); i++) {
        link_ptr(ri, &(rp[i].name));
        link_ptr(ri, &(rp[i].dims));
    }
}
static void link_info_control(struct rai_info *ri, struct rai_info_control **rpp) {
    link_ptr(ri, rpp);
    struct rai_info_control *rp = *rpp;
    for (int i = 0; !rai_list_end(&rp[i]); i++) {
        link_ptr(ri, &(rp[i].desc));
        link_ptr(ri, &(rp[i].unit));
    }
}
static void link_header(struct rai_info *ri) {
    link_ptr(ri, &ri->entry);
    link_info_param(ri, &ri->info_state);
    link_info_param(ri, &ri->info_in);
    link_info_param(ri, &ri->info_param);
    link_info_param(ri, &ri->info_out);
    link_info_param(ri, &ri->info_store);
    link_info_control(ri, &ri->info_control);
}

struct rai_info *rai_load_bin(const char *filename) {
    FILE *f = NULL;
    void *buf = NULL;
    if (NULL == (f = fopen(filename, "r"))) goto error;
    fseek(f, 0L, SEEK_END);
    size_t size = ftell(f);
    if (0 == size) goto error;
    fseek(f, 0L, SEEK_SET);
    if (0 != posix_memalign(&buf, getpagesize(), size)) goto error;
    if (0 != mprotect(buf, size, PROT_READ | PROT_WRITE | PROT_EXEC)) goto error;
    if (size != fread(buf, 1, size, f)) goto error;
    fclose(f);
    link_header((void*)buf);
    // if (0 != mprotect(buf, size, PROT_READ | PROT_EXEC)) goto error;
    return buf;
  error:
    // fprintf(stderr, "ERROR loading %s\n", filename);
    if (f) fclose(f);
    if (buf) free(buf);
    return NULL;
}


bool rai_info_find(const struct rai_info_param *pi,
                   const char *name, int *offset, uintptr **dims) {
    *offset = 0;
    for (int i = 0; !rai_list_end(&pi[i]); i++) {
        *dims = pi[i].dims;
        if (!strcmp(pi[i].name, name)) return true;
        int size = 1;
        for (int j = 0; !rai_list_end(&(pi[i].dims[j])); j++) {
            size *= pi[i].dims[j];
        }
        *offset += size;
    }
    return false;
}

int rai_info_size(const struct rai_info_param *pi) {
    int size;
    uintptr *dims;
    rai_info_find(pi, "", &size, &dims);
    return size;
}

struct rai_proc *rai_proc_new(const struct rai_info *info,
                              const struct rai_proc *proto) {
    struct rai_proc *p = malloc(sizeof(*p));
    p->info = info;

    /* Allocate in 1 chunk to improve locality. */
    int size_state = rai_info_size(info->info_state);
    int size_param = rai_info_size(info->info_param);
    int size_store = rai_info_size(info->info_store);

    int buf_size = sizeof(float) * (2 * size_state + size_param + size_store);
    char *buf = calloc(1, buf_size);
    p->param    = (void*)buf; buf += size_param       * sizeof(float);
    p->state    = (void*)buf; buf += (2 * size_state) * sizeof(float);
    p->store    = (void*)buf;

    /* Copy from proto */
    int proto_size_state = 0;
    int proto_size_param = 0;
    int proto_size_store = 0;
    if (proto) {
        const struct rai_info *pri = proto->info;
        proto_size_state = rai_info_size(pri->info_state);
        proto_size_param = rai_info_size(pri->info_param);
        proto_size_store = rai_info_size(pri->info_store);
    }
    for (int i=0; i<size_param; i++) {
        p->param[i] = (i < proto_size_param) ?
            proto->param[i] : 0;
    }
    for (int i=0; i<size_store; i++) {
        p->store[i] = (i < proto_size_store) ?
            proto->store[i] : 0;
    }
    for (int i=0; i<size_state; i++) {
        p->state[i] = (i < proto_size_state) ?
            proto->state[i] : 0;
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

void rai_proc_preset_save(const struct rai_proc *p, const char *filename) {
    FILE *f = fopen(filename, "a");
    if (!f) return;
    struct rai_info_preset hdr = {
        .magic = "preset",
        .header_bytes = sizeof(hdr),
        .timestamp = time(NULL),
        .payload_bytes = sizeof(float) * rai_info_size(p->info->info_param),
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
        int nb_bytes = rai_info_size(p->info->info_param);
        if (sizeof(hdr) != fread(&hdr, 1, sizeof(hdr), f)) goto error;
        if (nb_bytes != hdr.payload_bytes) goto error;
        if (nb_bytes != fread(p->param, 1, nb_bytes, f)) goto error;
    } while(index--);
  error:
    fclose(f);
}


/* DEBUG */
static void rai_print_info_param(const char *tag,
                                 const struct rai_info_param *pi,
                                 rai_log log) {
    int offset = 0;
    log("%s %d:\n", tag, rai_info_size(pi));
    for (int i = 0; !rai_list_end(&pi[i]); i++) {
        log("\t%d %s", i, pi[i].name);
        uintptr *dims = pi[i].dims;
        int size = 1;
        for (int d = 0; dims[d]; d++) {
            log(" %d", dims[d]);
            size *= dims[d];
        }
        log(" (size=%d, offset=%d)\n", size, offset);
        offset += size;
    }
}
static void rai_print_info_control(const char *tag,
                                   const struct rai_info_control *pi,
                                   rai_log log) {
    log("%s\n", tag);
    for (int i = 0; !rai_list_end(&pi[i]); i++) {
        log("\t%d %s(%s) [%f,%f]\n",
            pi[i].index,
            pi[i].desc,
            pi[i].unit,
            pi[i].s0,
            pi[i].s1);
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


