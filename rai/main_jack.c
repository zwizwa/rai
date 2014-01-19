/**
 * Jack wrapper for .sp modules.
 * Adapted from simple_client.c */

#include "prim.h"
#include "rai.h"
#ifndef PROC_FILE
#error PROC_FILE not defined
#else
#include PROC_FILE
#endif

#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <jack/jack.h>

#define ERROR(...) {fprintf (stderr, __VA_ARGS__); exit(1); }
#define LOG(...)   {fprintf (stderr, __VA_ARGS__); }

/* Only build a static wrapper. */
struct proc_si state[2];
struct proc_param param;
struct proc_in in;
struct proc_out out;
struct proc_store store;

/* Access to i/o as float arrays. */
float **a_param  = (float**)&param;
float **a_in     = (float**)&in;
float **a_out    = (float**)&out;


// FIXME: old float-only interface
#define PROC_NB_EL(x,t) ((sizeof(x) / sizeof(t)))
#define proc_size_param   PROC_NB_EL(struct proc_param, float)
#define proc_size_in      PROC_NB_EL(struct proc_in,    float*)
#define proc_size_out     PROC_NB_EL(struct proc_out,   float*)


/* Jack port objects */
jack_port_t *input_port[proc_size_in];
jack_port_t *output_port[proc_size_out];
jack_client_t *client;




/**
 * The process callback for this JACK application is called in a
 * special realtime thread once for each audio cycle.
 */

int
process (jack_nframes_t nframes, void *arg)
{
    /* Get port buffers. */
    for (int i=0; i < proc_size_in; i++)  a_in[i]  = jack_port_get_buffer (input_port[i],  nframes);
    for (int i=0; i < proc_size_out; i++) a_out[i] = jack_port_get_buffer (output_port[i], nframes);

    proc_loop((void*)&state, &in, &param, &out, &store, nframes);
    return 0;
}

/**
 * JACK calls this shutdown_callback if the server ever shuts down or
 * decides to disconnect the client.
 */
void
jack_shutdown (void *arg)
{
    free(arg);
    exit (1);
}

/* Map .g.h macros to functions. */

void create_input(const char *name, void* base, int kind, int size) {
    static int p;
    if (p >= proc_size_param) ERROR("proc_size_param\n");
    LOG("\t%d: %s %d %d\n", p, name, kind, size);
    if (NULL == (input_port[p] =
                 jack_port_register(client, name, JACK_DEFAULT_AUDIO_TYPE,
                                    JackPortIsInput, 0))) {
        ERROR("no more JACK ports available\n");
    }
    p++;
}
void create_output(const char *name, void* base, int kind, int size) {
    static int p;
    if (p >= proc_size_out) ERROR("proc_size_out\n");
    LOG("\t%d: %s %d %d\n", p, name, kind, size);
    if (NULL == (output_port[p] =
                 jack_port_register(client, name, JACK_DEFAULT_AUDIO_TYPE,
                                    JackPortIsOutput, 0))) {
        ERROR("no more JACK ports available\n");
    }
    p++;
}
void create_param(const char *name, void*  base, int kind, int size) {
    static int p;
    if (p >= proc_size_param) ERROR("proc_size_param\n");
    LOG("\t%d: %s %d %d\n", p, name, kind, size);
    p++;
}

#define CREATE_INPUT(  n, t, k, s, ...) create_input  (#n,(void*)&in.n,   k,s);
#define CREATE_OUTPUT( n, t, k, s, ...) create_output (#n,(void*)&out.n,  k,s);
#define CREATE_PARAM(  n, t, k, s, ...) create_param  (#n,(void*)&param.n,k,s);
void create_ports(jack_nframes_t sr) {

    LOG("parameters:\n"); proc_for_param (CREATE_PARAM);
    LOG("inputs:\n");     proc_for_in    (CREATE_INPUT);
    LOG("outputs:\n");    proc_for_out   (CREATE_OUTPUT);

#if defined(proc_param_samplerate) && 0 == proc_param_samplerate
    LOG("samplerate = %d\n", sr);
    param.samplerate = sr;
#endif

#if defined(proc_param_timestep) && 0 == proc_param_timestep
    param.timestep = 1 / (float)sr;
    LOG("timestep = %f\n", param.timestep);
#endif

}


void go() {
    const char **ports;

    /* Tell the JACK server that we are ready to roll.  Our
     * process() callback will start running now. */
    if (jack_activate (client)) {
        ERROR("cannot activate client");
    }

    /* Connect the ports.  We can't do this before the client is
     * activated, because we can't make connections to clients that
     * aren't running.  Note the confusing (but necessary) orientation
     * of the driver backend ports: playback ports are "input" to the
     * backend, and capture ports are "output" from it.
     */

#if 0
    ports = jack_get_ports (client, NULL, NULL, JackPortIsPhysical|JackPortIsOutput);
    if (ports == NULL) {
        ERROR("no physical capture ports\n");
    }
    if (jack_connect (client, ports[0], jack_port_name (input_port[0]))) {
        LOG("cannot connect input ports\n");
    }
    jack_free (ports);
#endif

    ports = jack_get_ports (client, NULL, NULL, JackPortIsPhysical|JackPortIsInput);
    if (ports == NULL) {
        ERROR("no physical playback ports\n");
    }
    if (jack_connect (client, jack_port_name (output_port[0]), ports[0])) {
        LOG("cannot connect output ports\n");
    }
    jack_free (ports);

}

void init_jack() {
    const char *client_name = CLIENT_NAME;
    const char *server_name = NULL;
    jack_options_t options = JackNullOption;
    jack_status_t status;

    /* Open a client connection to the JACK server */
    client = jack_client_open (client_name, options, &status, server_name);
    if (client == NULL) {
        if (status & JackServerFailed) {
            LOG("Unable to connect to JACK server\n");
        }
        ERROR("jack_client_open() failed, status = 0x%2.0x\n", status);
    }
    if (status & JackServerStarted) {
        LOG("JACK server started\n");
    }
    if (status & JackNameNotUnique) {
        client_name = jack_get_client_name(client);
        LOG("unique name `%s' assigned\n", client_name);
    }
}

void init_processor() {

    /* Create process object's context data structure. */
    float *state = malloc(sizeof(float) * 1);
    state[0] = 0;

    /* Register work & cleanup callbacks. */
    jack_set_process_callback (client, process, state);
    jack_on_shutdown (client, jack_shutdown, state);

    /* Obtain engine SR. */
    jack_nframes_t sr = jack_get_sample_rate (client);
    // LOG("engine sample rate: %" PRIu32 "\n", sr);

    create_ports(sr);

}


char buf[4096];
void readline(void) {
    int i=0;
    char c=0;
    while (c != '\n' && i < sizeof(buf)-1) {
        if (1 != read(0, &c, 1)) ERROR("Read error.\n");
        // LOG("read %d\n", c);
        buf[i++] = c;
    }
    buf[i] = 0;
}

void param_set(char *var, int val) {
#define CASE_PARAM(p_name, ...)                                         \
    if (!strcmp(#p_name, var)) { *(float *)(&param.p_name) = val; goto ok; }
    proc_for_param(CASE_PARAM)
    LOG("? %s = %d\n", var, val);
    return;
  ok:
    LOG("! %s = %d\n", var, val);
}

void param_reader(void) {
    char var[100];
    int val;
    while(1) {
        readline();
        if (2 != sscanf(buf, "%s %d;\n", (char*)&var, &val)) ERROR("scanf()\n");
        // LOG("%s = %d\n", var, val);
        param_set(var, val);
    }
}

int
main (int argc, char *argv[])
{
    init_jack();
    init_processor();
    go();

    /* By default: keep running. */
    int sleep_count = -1;

    if (argc >= 2) {
        sleep_count = atoi(argv[1]);
        LOG("Running for %d second(s).\n", sleep_count);
        sleep(sleep_count);
    }
    else {
        LOG("Starting param_reader on stdin.\n");
        param_reader();
    }


    /* This is never reached but if the program had some other way to
       exit besides being killed, they would be important to call. */
    jack_client_close (client);
    exit (0);
}
