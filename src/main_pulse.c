#include <stdio.h>
#include <string.h>
#include <pulse/simple.h>
#include <pulse/error.h>

int main(int argc, char*argv[]) {
    /* The Sample format to use */
    static const pa_sample_spec ss = {
        .format = PA_SAMPLE_S16LE,
        .rate = 44100,
        .channels = 2
    };
    pa_simple *s = NULL;
    int ret = 1;
    int error;
    /* Create a new playback stream */
    if (!(s = pa_simple_new(NULL, argv[0], PA_STREAM_PLAYBACK, NULL, "playback", &ss, NULL, NULL, &error))) {
        fprintf(stderr, "pa_simple_new() failed: %d\n", error);
        goto exit;
    }
    int count = 50;
    while(count--) {
        uint8_t buf[1024];
        bzero(buf, sizeof(buf));
        buf[0] = 127;
        buf[1] = 127;
        if (pa_simple_write(s, buf, (size_t) sizeof(buf), &error) < 0) goto exit;
    }
  exit:
    pa_simple_drain(s, &error); // ignore error
    if (s) pa_simple_free(s);
    return 0;
}
