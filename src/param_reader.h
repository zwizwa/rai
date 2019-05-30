#ifndef PARAM_READER_H
#define PARAM_READER_H

/* stdin reader for stand-alone processes: jack, pulse 
   NEEDS proc_for_param from .g.h
*/


static char readline_buf[4096];
static inline void readline(void) {
    int i=0;
    int c=0;
    while (c != '\n' && i < sizeof(readline_buf)-1) {
        c = getchar();
        if (EOF == c) ERROR("EOF");
        // LOG("read %d\n", c);
        readline_buf[i++] = c;
    }
    readline_buf[i] = 0;
}

typedef void (*param_set1_t)(const char *var, float val1);
typedef void (*param_set2_t)(const char *var, float val1, float val2);

static inline void param_set1(const char *var, float val) {
#define CASE_PARAM(p_name, ...)                                         \
    if (!strcmp(#p_name, var)) { *(float *)(&param.p_name) = val; goto ok; }
    proc_for_param(CASE_PARAM)
    LOG("? %s = %f\n", var, val);
    return;
  ok:
    LOG("! %s = %f\n", var, val);
}

static inline void param_reader_loop(param_set1_t param_set1, param_set2_t param_set2) {
    char var[100];
    float val1=0, val2=0;
    while(1) {
        readline();
        if (3 == sscanf(readline_buf, "%s %f %f;\n", (char*)&var, &val1, &val2)) {
            if (param_set2) { param_set2(var, val1, val2); }
        }
        else if (2 == sscanf(readline_buf, "%s %f;\n", (char*)&var, &val1)) {
            if (param_set1) { param_set1(var, val1); }
        }
        else {
            ERROR("scanf()\n");
        }
    }
}
static inline void param_reader(void) {
    param_reader_loop(&param_set1, NULL);
}

static inline void param_vu(float **a_out, int proc_size_out, int nframes) {
    float max = 0;
    for (int j=0; j < nframes; j++) {
        for (int i=0; i < proc_size_out; i++) {
            float v = fabs(a_out[i][j]);
            max = v > max ? v : max;
        }
    }
    LOG("\r%0.5f\r", max);
}

#endif
