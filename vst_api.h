#ifndef _VST_API_H_
#define _VST_API_H_

/* Binary VST API.  This does not depend on the VST SDK.

   FIXME: It doesn't look like this is a good idea.  C++ virtual
   methods defined in the .dll are accessed, which is not something C
   can do properly?

*/
typedef union {
    unsigned long i;  // integer, size of pointer
    void *p;
} intp;

typedef unsigned int       int32;  // Fixed size ints
typedef unsigned long long int64;

struct plugin {
    int32 magic; // 'VstP'
    void *dispatcher;
    void *_depr_0;
    void *set_parameter;
    void *get_parameter;
    int32 num_programs;
    int32 num_params;
    int32 num_inputs;
    int32 num_outputs;
    int32 flags;
    void *_res_0[2];
    int32 initial_delay;
    int32 _depr_1[3];
    void *object;
    void *user;
    int32 id;
    int32 version;
    void *process_replacing;
    void *process_replacing_d;
    char _res_1[56];
};

enum plugin_opcode {
    p_open = 0,
    p_close,
};

typedef struct plugin plugin;

typedef intp (*host)(plugin* e, int32 opcode, int32 index, intp value, void* ptr, float opt);

typedef plugin* (*init_plugin)(host);



#endif // _VST_API_H_
