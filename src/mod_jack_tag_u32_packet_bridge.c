#ifndef MOD_JACK_TAG_U32_PACKET_BRIDGE
#define MOD_JACK_TAG_U32_PACKET_BRIDGE

#include "packet_bridge.h"
#include "tag_u32.h"
struct port *port;
#define SEND_TAG_U32_BUF_WRITE(...) port->write(port, __VA_ARGS__)

/* Once jack is set up, the main thread handles the communication. */
void reader_loop(param_set1_t param_set1, param_set2_t param_set2) {

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
}

#endif

