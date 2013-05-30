/* Bridge rai and VST APIs */

// Uncomment to disable logging
// #define LOG(...)

/* VST */
#include <audioeffect.h>
#include <audioeffectx.h>
#include <audioeffect.cpp>
#include <audioeffectx.cpp>


/* PLATFORM */
#ifdef _WIN32
#include "windows.h"
#include "winsock.h"

#include "win_debug.h"  // defines LOG
#endif
#include <string.h>
#include <stdio.h>

/* Software licence check */
#ifdef   LICENCE
#include LICENCE
#endif

/* RAI */

#define restrict // FIXME: ?? problem in C++ ?

extern "C" {
#include "prim.h"
#ifndef PROC_FILE
#error PROC_FILE not defined
#else
#include PROC_FILE
#endif
#include "rai.h"
}

#ifndef LOG
#define LOG(...)
#endif


/* Create synth voice allocator if freq and gate params are defined as
   1-dim arrays.  */
#if defined(proc_param_voice_freq) && \
    defined(proc_param_voice_gate) && \
    proc_param_voice_freq == 1 && \
    proc_param_voice_gate == 1
#define HAVE_SYNTH 1
//#warning SYNTH
#else
#define HAVE_SYNTH 0
//#warning NO SYNTH
#endif


/* Parameter control subsampling rate. */
#define control_block 32


#define VST_PARAM(_param, _desc, _unit, _min, _max, _range, _curve) {   \
            offsetof(struct proc_param,_param)/sizeof(float),           \
            _desc,                                                      \
            _unit,                                                      \
            rai_scale_##_curve,                                         \
            _min,                                                       \
            _max,                                                       \
            _range,                                                     \
            },
const struct rai_info_control param[] = {
    proc_for_control(VST_PARAM) 
};

#define nb_vst_param (sizeof(param)/sizeof(param[0]))


/* proc_ state wrapper. */
struct plugin_state {
    struct proc_si state[2];
    struct proc_param param;
    struct proc_store store;
#if HAVE_SYNTH
    struct rai_voice voice;
#endif
};


class Plugin : public AudioEffectX
{
private:
    struct plugin_state *state;
    VstEvents* events;
    void MIDI(VstMidiEvent *e);
public:
    Plugin (audioMasterCallback audioMaster, VstInt32 numPrograms, VstInt32 numParams);
    ~Plugin ();
    void processReplacing (float **i, float **o, VstInt32 n);
    VstIntPtr dispatcher(VstInt32 opCode, VstInt32 index, VstIntPtr value, void *ptr, float opt);
    void setParameter (VstInt32 index, float value);
    float getParameter (VstInt32 index);
    void getParameterName (VstInt32 index, char *text);
    void getParameterUnit (VstInt32 index, char *text);
    void getParameterDisplay (VstInt32 index, char *text);
    void setSampleRate (float sampleRate);
    VstInt32 processEvents (VstEvents* events);
};

/* LIB ENTRY */
#ifdef _WIN32
HINSTANCE hInstance;
BOOL APIENTRY DllMain( HINSTANCE  hModule, DWORD  reason, LPVOID lpReserved) { 
    hInstance = hModule; 
    return TRUE;
}
#define main_rv int
main_rv main (audioMasterCallback audioMaster);
#endif

#ifdef __linux__ 
AEffect* main_plugin (audioMasterCallback audioMaster) asm ("main");
#define main main_plugin
#define main_rv AEffect*
#endif



main_rv main (audioMasterCallback audioMaster) { 
    LOG("main_vst.cpp: %s\n", PROC_FILE);
    Plugin* effect = new Plugin (audioMaster, 1, nb_vst_param);
    if (!effect) return 0;
    return (main_rv)effect->getAeffect ();
}



/* AudioEffectX subclass */
Plugin :: Plugin(audioMasterCallback audioMaster, VstInt32 numPrograms, VstInt32 numParams)
    : AudioEffectX(audioMaster, numPrograms, numParams) {
    state = (struct plugin_state *)calloc(1, sizeof(struct plugin_state));
    events = NULL;
    setUniqueID (0x41424344); // ABCD
    setNumInputs(proc_size_in);    
    setNumOutputs(proc_size_out);
#if HAVE_SYNTH
    isSynth();
    rai_voice_init(&state->voice,
                   PROC_PARAM_DIM(voice_gate),
                   (float*)&state->param.voice_gate,
                   (float*)&state->param.voice_freq);
#endif
    canProcessReplacing ();
#define VST_PARAM_INIT(_param, ...) state->param._param = 0.5;
    proc_for_control(VST_PARAM_INIT)

#if defined(LICENCE)
    int lic_ok = check_license();
#if defined(proc_param_license)
    state->param.license = lic_ok;
#endif
#endif

};


Plugin :: ~Plugin() {
    free(state);
}


/* Events are valid during the following process. */
VstInt32 Plugin :: processEvents (VstEvents* e) {
    events = e;
    return 1;
}


void Plugin :: MIDI(VstMidiEvent *e) {
    u8 d0 = e->midiData[0];
    u8 d1 = e->midiData[1];
    u8 d2 = e->midiData[2];
    
    int tag     = d0 & 0xF0;
    int channel = d0 & 0x0F;
    float freq  = rai_midi_to_freq(d1);

    LOG("midi %02x %02x %02x (%d)\n", d0, d1, d2);
    switch(tag) {
#if HAVE_SYNTH
    case 0x90:
        LOG("Note on %f\n", freq);
        rai_voice_on(&state->voice, freq);
        break;
    case 0x80:
        LOG("Note off %f\n", freq);
        rai_voice_off(&state->voice, freq);
        break;
#endif
    default:
        break;
    }
}

void Plugin :: processReplacing  (float **in0, float **out0, VstInt32 total) {
    int offset = 0;  // index into sample arrays for current event
    int event  = 0;  // index into event array


    /* Run processor internal loop at fixed control rate to keep it
       simpler.  Before running, process all events related to next
       control block. */

    // LOG("loop %d\n", total);
    while (offset < total) {

        int chunk = total - offset;
        if (chunk >= control_block) chunk = control_block;
        int control_endx = offset + chunk;

        /* Execute events corresponding to next control block. */
        while (events && (event < events->numEvents)) {
            VstMidiEvent *e = (VstMidiEvent*) events->events[event];
            if (e->type == kVstMidiType) {
                if (e->deltaFrames >= control_endx) break;
                MIDI(e);
            }
            event++;
        }

        /* Perform next control chunk. */
        // LOG(" chunk %d %d\n", offset, chunk);
        float *in [proc_size_in];  for (u32 i = 0; i < proc_size_in;  i++) in[i]  = in0[i]  + offset;
        float *out[proc_size_out]; for (u32 i = 0; i < proc_size_out; i++) out[i] = out0[i] + offset;
        proc_loop((struct proc_si *)&state->state,
                  (struct proc_in *)(void *)in,
                  &state->param,
                  (struct proc_out *)(void *)out,
                  &state->store,
                  chunk);
        if (chunk & 1) {
            state->state[0] = state->state[1];
        }
        offset += chunk;
    }
    events = NULL;

}

void Plugin :: setSampleRate (float sr) {
    LOG("setSampleRate(%f)\n", sr);
#if defined(proc_param_samplerate) && 0 == proc_param_samplerate
        state->param.samplerate = sr;
        LOG("samplerate = %d\n", state->param.samplerate);
#endif
#if defined(proc_param_timestep) && 0 == proc_param_timestep
        state->param.timestep = 1.0 / sr;
        LOG("timestep = %f\n", state->param.timestep);
#endif
}

// FIXME: Don't use this.  Override AudioEffectX methods instead.
VstIntPtr Plugin :: dispatcher (VstInt32 opCode, VstInt32 index,
                                VstIntPtr value, void *ptr, float opt) {
    int result = 0;
    VstEvents* events;
    switch (opCode) {
    default: 
        // LOG("AudioEffectX::dispatcher(%d,%d,%d,%p,%f)\n", opCode, index, value, ptr, opt);
        result = AudioEffectX::dispatcher(opCode, index, value, ptr, opt);
        break;
    }
    return result;
}



/* Only unit-annotated parameters are accessible through the VST
   parameter interface to avoid exposure of system parameters such as
   `samplerate'. */
void Plugin :: setParameter (VstInt32 index, float value) {
    LOG("setParameter(%d,%f)\n",index,value);
    float *p = (float*)&state->param;
    p[param[index].index] = value;
}
float Plugin :: getParameter (VstInt32 index) {
    LOG("getParameter(%d)\n",index);
    float *p = (float*)&state->param;
    return p[param[index].index];
}

void Plugin :: getParameterName (VstInt32 index, char *text) {
    strcpy(text, param[index].desc);
    LOG("getParameterName(%d) -> %s\n",index,text);
}
void Plugin :: getParameterUnit (VstInt32 index, char *text) {
    strcpy(text, param[index].unit);
    LOG("getParameterUnit(%d) -> %s\n",index,text);
}
void Plugin :: getParameterDisplay (VstInt32 index, char *text) { 
    const struct rai_info_control *p = &param[index];
    float v = getParameter(index);
    float out_v = rai_info_control_interpolate(p, v);
    sprintf(text, "%f", out_v);
    LOG("getParameterDisplay(%d) -> %s (%f)\n",index,text,v);
}


