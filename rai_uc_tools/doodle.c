#define PROC_FILE "doodle.g.h"
#define CLIENT_NAME "doodle"

#include "mod_jack_tag_u32.c"

/* root map */
int map_root(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"param", "map", 0, map_param},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}

/* Protocol handler entry point. */
int handle_tag_u32(struct tag_u32 *req) {
    return map_root(req);
}


