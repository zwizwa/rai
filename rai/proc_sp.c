#include "proc.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>


/* Relocation: every date structure pointer needs to be patched from
   proc_class relative to absolute memory location. */
static void link_ptr(struct proc_class *ri, void *pointer) {
    long offset = *(long*)pointer;
    char *linked = (char*)ri + offset;
    *(void **)pointer = linked;
}
static void link_info_param(struct proc_class *ri, struct proc_class_param **rpp) {
    link_ptr(ri, rpp);
    struct proc_class_param *rp = *rpp;
    for (int i = 0; !proc_list_end(&rp[i]); i++) {
        link_ptr(ri, &(rp[i].name));
        link_ptr(ri, &(rp[i].dims));
    }
}
static void link_info_control(struct proc_class *ri, struct proc_class_control **rpp) {
    link_ptr(ri, rpp);
    struct proc_class_control *rp = *rpp;
    for (int i = 0; !proc_list_end(&rp[i]); i++) {
        link_ptr(ri, &(rp[i].desc));
        link_ptr(ri, &(rp[i].unit));
    }
}
static void link_header(struct proc_class *ri) {
    link_ptr(ri, &ri->entry);
    /* cast is needed to remove const */
    link_info_param(ri, (struct proc_class_param**)&ri->info_state);
    link_info_param(ri, (struct proc_class_param**)&ri->info_in);
    link_info_param(ri, (struct proc_class_param**)&ri->info_param);
    link_info_param(ri, (struct proc_class_param**)&ri->info_out);
    link_info_param(ri, (struct proc_class_param**)&ri->info_store);
    link_info_control(ri, (struct proc_class_control**)&ri->info_control);
    link_ptr(ri, &ri->init_state);
    link_ptr(ri, &ri->init_store);
}

struct proc_class *proc_load_sp(const char *filename) {
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
