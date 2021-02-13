/* FIXME: I don't like to make rai depend on uc_tools, but for some
   things it is just so convenient.  For now, I'll keep this file in
   the rai archive.  It is not used by the default build system, which
   builds jack processors that use the Pd protocol instead. */

#ifndef MOD_JACK_TAG_U32
#define MOD_JACK_TAG_U32

/* Module needs these two parameters before #include, and
   handle_tag_u32() defined somewhere in the image. */
#ifndef PROC_FILE
#error need PROC_FILE
#endif
#ifndef CLIENT_NAME
#error need CLIENT_NAME
#endif

// See doodle.c example


/* Create a RAI Jack processor using some UC_TOOLS communication
   infrastructure. */

/* RAI */

#include <strings.h>
#pragma GCC diagnostic ignored "-Wunused-variable"
#define PARAM_READER_LOOP reader_loop
#include "../src/main_jack.c"

struct param_meta {
    const char *name;
    float32_t *slot;
};
#define PARAM_META(n, t, k, s, ...) \
    {#n,&param.n},
const struct param_meta param_meta[] = {
proc_for_param(PARAM_META)
};

/* UC_TOOLS */

#include "tag_u32.h"
int handle_tag_u32(struct tag_u32*);

// Pick one of two implementations.
//#include "mod_jack_tag_u32_packet_bridge.c"
#include "mod_jack_tag_u32_stream.c"

#include <string.h>
#include <stdarg.h>

#include "mod_send_tag_u32.c"


/* FIXME: This needs to be replaced!

   The idea is to build a library that can handle streaming of large
   data stuctures such as delay lines, so this only parses the header,
   then handles the payload from direct stream reads.
*/



/* Some command IDs (indices) are hard coded.  This avoids having to
   resolve them every time.  It seems ok to do this for the indices
   that are part of the core API. */
#define ID_PARAM 0
#define ID_TYPE  1
#define ID_SET 0
#define ID_GET 1
#define ID_META 2

int param_set(struct tag_u32 *req) {
    TAG_U32_UNPACK(req, -2, m, cmd_nb, op) {
        if (m->cmd_nb >= ARRAY_SIZE(param_meta)) return -1;
        if (req->nb_bytes != 4) return -1;
        const union { uint32_t i; float32_t f; } w = {
            .i = read_be_32(req->bytes, 4)
        };
        *param_meta[m->cmd_nb].slot = w.f;

        LOG("param_set: %d <- %f\n", m->cmd_nb, w.f);

        send_reply_tag_u32_ok(req);
        return 0;
    }
    return -1;
}

int param_get(struct tag_u32 *req) {
    TAG_U32_UNPACK(req, -2, m, cmd_nb, op) {
        if (m->cmd_nb >= ARRAY_SIZE(param_meta)) return -1;
        const union { uint32_t i; float32_t f; } w = {
            .f = *param_meta[m->cmd_nb].slot
        };
        send_reply_tag_u32_ok_u32(req, w.i);
        return 0;
    }
    return -1;
}

/* A note about high level protocol.
   Name and node type are used here in the following way:

   - the type "f32" refers to the data payload in the binary buffer,
     which contains 4 bytes representing a floating point number in
     the protocol's endianness, which for now is always big endian.

   - "set" indicates this is a set method, which takes no extra tag
     arguments, takes a binary payload and returns an ok tag [0].

   - "get" indicates this is a get method, which takes no extra tag
     arguments, no binary payload, and produces an ok tag [0] and a
     binary payload.
*/

/* The name of the map refers to the handler function. */
#define DEF_MAP DEF_TAG_U32_CONST_MAP_HANDLE

DEF_MAP(
    map_param_meta,
    {"type", "cmd"},
    {"min",  "cmd"},
    {"max",  "cmd"},
    )

DEF_MAP(
    map_param_ops,
    [ID_SET]  = {"set",  "cmd", param_set},
    [ID_GET]  = {"get",  "cmd", param_get},
    [ID_META] = {"meta", "param_meta", map_param_meta},
    )

int map_param_entry(struct tag_u32 *req, void *no_ctx,
                    struct tag_u32_entry *entry) {
    TAG_U32_UNPACK(req, 0, m, cmd_nb) {
        if (m->cmd_nb >= ARRAY_SIZE(param_meta)) return -1;
        const struct tag_u32_entry e = {
            .name = param_meta[m->cmd_nb].name,
            .type = "param",
        };
        *entry = e;
        return 0;
    }
    return -1;
}
int map_param(struct tag_u32 *req) {
    return handle_tag_u32_map_dynamic(req, map_param_ops, map_param_entry, NULL);
}


/* This map describes types: every name listed in this map can be used
   in the type field of another node.

   Currenlty only one type of type (kind?) is supported: "sub".

   When "param" or "param_meta" are used as types in other nodes, it
   means that they behave as "map" nodes, but guarantee that the
   substructure is the same if the type name is the same.

   This is used by the host side indexing mechanism to perform common
   subexpression elimination on the tree structure, i.e. allow some of
   the "class structure" to be recovered by encoding the tree as a
   graph. */

DEF_MAP(
    map_type,
    {"param",      "sub"},
    {"param_meta", "sub"},
    )

DEF_MAP(
    map_root,
    [ID_PARAM] = {"param", "map", map_param},
    [ID_TYPE]  = {"type",  "map", map_type},
    )

/* Protocol handler entry point. */
int handle_tag_u32(struct tag_u32 *req) {
    int rv = map_root(req);
    if (rv) {
        /* Always send a reply when there is a from address. */
        LOG("map_root() returned %d\n", rv);
        send_reply_tag_u32_status_cstring(req, 1, "bad_ref");
    }
    return 0;
}

#endif
