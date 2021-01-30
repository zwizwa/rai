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

#define USE_PACKET_BRIDGE 0

#if USE_PACKET_BRIDGE

#include "packet_bridge.h"
struct port *port;
#define SEND_TAG_U32_BUF_WRITE(...) port->write(port, __VA_ARGS__)

#else

#include "assert_write.h"
#include "assert_read.h"
#include "byteswap.h"
void write_packet(const uint8_t *buf, uint32_t buf_size) {
    uint8_t hdr[4] = {U32_BE(buf_size)};
    assert_write(1, hdr, 4);
    assert_write(1, buf, buf_size);
}
#define SEND_TAG_U32_BUF_WRITE(...) write_packet( __VA_ARGS__)
static inline uint32_t read_uint(uint32_t nb) {
    uint8_t buf[nb];
    assert_read(0, buf, nb);
    return read_be(buf, nb);
}
#define READ_VAR(type,var) \
        type var = read_uint(sizeof(var))

#endif

#include "tag_u32.h"
#include <string.h>
#include <stdarg.h>




#include "mod_send_tag_u32_pbuf.c"

int handle_tag_u32(struct tag_u32*);

/* FIXME: This needs to be replaced!

   The idea is to build a library that can handle streaming of large
   data stuctures such as delay lines, so this only parses the header,
   then handles the payload from direct stream reads.
*/



/* Once jack is set up, the main thread handles the communication. */
void reader_loop(param_set1_t param_set1, param_set2_t param_set2) {
#if USE_PACKET_BRIDGE
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
#else
    /* Streaming parser. */
    for(;;) {
        READ_VAR(uint32_t,len);
        READ_VAR(uint16_t,tag); ASSERT(TAG_U32 == tag);
        READ_VAR(uint8_t,nb_from);
        READ_VAR(uint8_t,nb_args);
        uint32_t nb_tags = nb_from + nb_args;
        uint32_t nb_bytes = len - 4 - 4 * nb_tags;
        //LOG("%d %d %d %d %d:", len, tag, nb_from, nb_args, nb_bytes);
        uint32_t tags[nb_tags];
        for (uint32_t i=0; i<nb_tags; i++) {
            tags[i] = read_uint(4);
            //LOG(" %d", tags[i]);
        }
        //LOG("\n");

        struct tag_u32 s = {
            .reply = send_reply_tag_u32,
            .from = tags,         .nb_from = nb_from,
            .args = tags+nb_from, .nb_args = nb_args,
            .nb_bytes = nb_bytes
        };
        // FIXME: how to decide?
        if (1) {
            uint8_t buf[nb_bytes + 1];
            s.bytes = buf;
            assert_read(0,buf,nb_bytes);
            buf[nb_bytes] = 0;
            //LOG("buf = %s\n", buf);
            handle_tag_u32(&s);
        }
    }

#endif
}

/* Some command IDs (indices) are hard coded.  This avoids having to
   resolve them every time.  It seems ok to do this for the indices
   that are part of the core API. */
#define ID_PARAM 0
#define ID_SET 0
#define ID_GET 1

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

int map_param_ops(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        [ID_SET] = {"set", "f32", 0, param_set},
        [ID_GET] = {"get", "f32", 0, param_get},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}

int map_param_entry(struct tag_u32 *req, void *no_ctx,
                  struct tag_u32_entry *entry) {
    TAG_U32_UNPACK(req, 0, m, cmd_nb) {
        if (m->cmd_nb >= ARRAY_SIZE(param_meta)) return -1;
        const struct tag_u32_entry e = {
            .name = param_meta[m->cmd_nb].name,
            .type = "map",
        };
        *entry = e;
        return 0;
    }
    return -1;
}
int map_param(struct tag_u32 *req) {
    return handle_tag_u32_map_dynamic(req, map_param_ops, map_param_entry, NULL);
}

/* root map */
int map_root(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        [ID_PARAM] = {"param", "map", 0, map_param},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}

/* Protocol handler entry point. */
int handle_tag_u32(struct tag_u32 *req) {
    return map_root(req);
}

#endif
