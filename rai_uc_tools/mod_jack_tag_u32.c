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
    void *ptr;
};
#define PARAM_META(n, t, k, s, ...) \
    {#n,(void*)&param.n},
const struct param_meta param_meta[] = {
proc_for_param(PARAM_META)
};

/* UC_TOOLS */

#include "packet_bridge.h"
#include "byteswap.h"
#include "tag_u32.h"
#include <string.h>
#include <stdarg.h>

struct port *port;
#include "mod_send_tag_u32_pbuf.c"

int handle_tag_u32(struct tag_u32*);

/* Once jack is set up, the main thread handles the communication. */
void reader_loop(param_set1_t param_set1, param_set2_t param_set2) {
#if 0
    /* Pd protocol from the main_jack.c wrapper. */
    param_reader_loop(param_set1, param_set2);
#else
    /* Framing protocol is hardcoded to {packet,4} */
    const char *spec = "-:4";
    ASSERT(port = port_open(spec));
    for(;;) {
        uint8_t buf[1024*64];
        int timeout_ms = 100;
        ssize_t rv = packet_next(port, timeout_ms, buf, sizeof(buf));
        if (!rv) {
            /* This is a place to hook a polling operation. */
        }
        else {

            //LOG("packet: "); for(uint32_t i=0; i<rv; i++) { LOG(" %02x", buf[i]); } LOG("\n");


            /* Process TAG_U32 protocol. */
            if ((rv >=4 ) && (TAG_U32 == read_be(buf, 2))) {
                tag_u32_dispatch(
                    handle_tag_u32,
                    send_reply_tag_u32,
                    NULL, buf, rv);
            }
            /* Ignore evertying else. */
            else {
                ERROR("bad protocol\n");
            }
        }
    }
#endif
}


int map_param_op(struct tag_u32 *req) {
    uint32_t nb_replies = (req->nb_args > 0) ? req->args[0] : 0;
    uint32_t cmd_nb = req->args[-1];

    if (cmd_nb >= ARRAY_SIZE(param_meta)) return -1;
    uint32_t args[] = {123}; // dummy
    const struct tag_u32 s = {
        .args = args, .nb_args = ARRAY_SIZE(args)
    };
    send_reply_tag_u32_maybe(req, &s);
    return 0;
}
int map_param_entry(struct tag_u32 *req, void *no_ctx,
                  struct tag_u32_entry *entry) {
    TAG_U32_UNPACK(req, 0, m, cmd_nb) {
        if (m->cmd_nb >= ARRAY_SIZE(param_meta)) return -1;
        const struct tag_u32_entry e = {
            .name = param_meta[m->cmd_nb].name,
            .type = "cmd",
        };
        *entry = e;
        return 0;
    }
    return -1;
}
int map_param(struct tag_u32 *req) {
    return handle_tag_u32_map_dynamic(req, map_param_op, map_param_entry, NULL);
}



#endif
