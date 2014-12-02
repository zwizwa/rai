#ifndef PARAM_READER_H
#define PARAM_READER_H


// originally from main_jack.c
// used in main_pulse.c
// NEEDS proc_for_param from .g.h

static char readline_buf[4096];
static inline void readline(void) {
    int i=0;
    char c=0;
    while (c != '\n' && i < sizeof(readline_buf)-1) {
        if (1 != read(0, &c, 1)) ERROR("Read error.\n");
        // LOG("read %d\n", c);
        readline_buf[i++] = c;
    }
    readline_buf[i] = 0;
}

static inline void param_set(char *var, int val) {
#define CASE_PARAM(p_name, ...)                                         \
    if (!strcmp(#p_name, var)) { *(float *)(&param.p_name) = val; goto ok; }
    proc_for_param(CASE_PARAM)
    LOG("? %s = %d\n", var, val);
    return;
  ok:
    LOG("! %s = %d\n", var, val);
}

static inline void param_reader(void) {
    char var[100];
    int val;
    while(1) {
        readline();
        if (2 != sscanf(readline_buf, "%s %d;\n", (char*)&var, &val)) ERROR("scanf()\n");
        // LOG("%s = %d\n", var, val);
        param_set(var, val);
    }
}

#endif
