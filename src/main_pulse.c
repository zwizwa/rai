/* PulseAudio wrapper for .sp modules. */

#include "prim.h"
#include "proc.h"
#ifndef PROC_FILE
#error PROC_FILE not defined
#else
#include PROC_FILE
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <pulse/simple.h>
#include <pulse/error.h>



#define ERROR(...) {fprintf (stderr, __VA_ARGS__); exit(1); }
#define LOG(...)   {fprintf (stderr, __VA_ARGS__); }



/* Only build a static wrapper. */
struct proc_si state[2];
struct proc_param param;
struct proc_in in;
struct proc_out out;
struct proc_store store;

// Bound to some of the names above.
#include "param_reader.h"


/* Access to i/o as float arrays.
   NOTE that for a_in, a_out, this supports both:
   - named scalar nodes
   - a single vector node.
   Other configurations are not supported and will probably cause a crash. */

float **a_param  = (float**)&param;
float **a_in     = (float**)&in;
float **a_out    = (float**)&out;


// FIXME: old float-only interface
#define PROC_NB_EL(x,t) ((sizeof(x) / sizeof(t)))
#define proc_size_param   PROC_NB_EL(struct proc_param, float)
#define proc_size_in      PROC_NB_EL(struct proc_in,    float*)
#define proc_size_out     PROC_NB_EL(struct proc_out,   float*)


int main(int argc, char*argv[]) {
    /* The Sample format to use */
    static const pa_sample_spec ss = {
        .format = PA_SAMPLE_S16LE,
        .rate = 44100,
        .channels = proc_size_out
    };

    LOG("channels = %d\n", ss.channels);

    pa_simple *s = NULL;
    int ret = 1;
    int error;

#if defined(proc_param_samplerate) && 0 == proc_param_samplerate
    LOG("samplerate = %d\n", ss.rate);
    param.samplerate = ss.rate;
#endif

#if defined(proc_param_timestep) && 0 == proc_param_timestep
    param.timestep = 1 / (float)ss.rate;
    LOG("timestep = %f\n", param.timestep);
#endif

    /* Output buffers.  FIXME: a_in: currently no inputs supported. */
    int nframes = 1024; // does this matter?
    for (int i=0; i < ss.channels; i++) a_out[i] = calloc(nframes, sizeof(float));
    uint16_t buf[nframes * ss.channels];

    /* Create a new playback stream */
    if (!(s = pa_simple_new(NULL, argv[0], PA_STREAM_PLAYBACK, NULL, "playback", &ss, NULL, NULL, &error))) {
        fprintf(stderr, "pa_simple_new() failed: %d\n", error);
        goto exit;
    }


    for(;;) {  // FIXME: exit?

        proc_loop((void*)&state, &in, &param, &out, &store, nframes);

        for (int i=0; i<nframes; i++) {
            for(int c=0; c<ss.channels; c++) {
                buf[i*ss.channels+c] = ((float)0x7FFF) * a_out[c][i];
            }
        }

        if (pa_simple_write(s, buf, (size_t) sizeof(buf), &error) < 0) goto exit;
    }
  exit:
    pa_simple_drain(s, &error); // ignore error
    if (s) pa_simple_free(s);
    return 0;
}
