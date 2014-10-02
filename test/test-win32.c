#include <windows.h>
#include <stdio.h>
#include "vst_api.h"

static intp call_host(plugin* e, int32 o, int32 i, intp v, void* p, float f) {
    printf("H: %p %d %d %d %p %f\n", e, o, i, v, p, f);
    intp rv = {};
    switch((enum plugin_opcode)o) {
    case p_open:
        break;
    default:
        printf("unknown opcode %d\n", o);
        break;
    }
    return rv;
}


void *sym(HMODULE lib, char *name) {
    void *addr = GetProcAddress(lib, name);
    printf("%p %s\n", addr, name);
    return addr;
}
void print_plugin(plugin *p) {
    char id[8] = {};
    memcpy(id, &p->id, 4);
    printf("%08x(%s) %d\n", p->id, id, p->version);
    printf("p: %d\n", p->num_params);
    printf("i: %d\n", p->num_inputs);
    printf("o: %d\n", p->num_outputs);
    printf("f: %08x\n", p->flags);
}

plugin *open_vst(char *file) {
    HMODULE lib = LoadLibrary(file);
    if (lib == NULL) {
        printf("can't load %s\n", file);
    } else {
        printf("load %s OK\n", file);
        init_plugin init_plugin = sym(lib, "main");
        plugin *plugin = init_plugin(call_host);
        printf("%p plugin\n", plugin);
        return plugin;
    }
}

int main(int argc, char **argv) {
    printf("%s\n", argv[0]);
    char *file = (argc > 1) ? argv[1] : "plugin.dll";
    plugin *p = open_vst(file);
    print_plugin(p);
    return 0;
}
