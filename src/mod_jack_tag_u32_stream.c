#ifndef MOD_JACK_TAG_U32_STREAM
#define MOD_JACK_TAG_U32_STREAM

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

/* Once jack is set up, the main thread handles the communication. */
void reader_loop(param_set1_t param_set1, param_set2_t param_set2) {
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
}



#endif
