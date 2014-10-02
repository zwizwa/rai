
// Currently, the minimal invasive approach is:
// - concatenate with .g.h 
// - copy to AxoStudio/patch/xpatch.cpp
// - make -C AxoStudio/patch
// - axocl.py --load AxoStudio/patch/xpatch.bin

// FIXES
#define restrict  // not C++

extern "C"
{
#include "../firmware/patch.h"
#include "../firmware/axoloti.h"
#include "../firmware/parameter_functions.h"
#include "prim.h"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-parameter"
#include "xpatch.g.h"
}

void xpatch_init2 (void);
extern "C" __attribute__ ((section (".boot")))
void xpatch_init (void) {
  xpatch_init2 ();
}

float32_t b_out[BUFSIZE];

struct proc_si p_state[2];
struct proc_param p_param;
struct proc_store p_store;


void PatchProcess (int32_t * inbuf, int32_t * outbuf) {

  struct proc_in  p_in = {};
  struct proc_out p_out = {&b_out[0]};

  proc_loop(&p_state[0],
            &p_in,
            &p_param,
            &p_out,
            &p_store,
            BUFSIZE);
  
  // Convert to fixed point
  int i;
  for (i = 0; i < BUFSIZE; i++) {
    float32_t f_v = ((float32_t)0x40000000) * b_out[i];
    int32_t i_v = (int32_t)f_v;
    outbuf[i * 2]     = i_v;
    outbuf[i * 2 + 1] = i_v;
  }
}

void PatchDispose() {}

void PatchMidiInHandler (uint8_t status, uint8_t data1, uint8_t data2) {} 

void xpatch_init2 (void) {
  patchMeta.npresets = 0;
  patchMeta.npreset_entries = 0;
  patchMeta.pPresets = NULL;
  patchMeta.pPExch = NULL;
  patchMeta.pDisplayVector = NULL;
  patchMeta.numPEx = 0;
  patchMeta.fptr_dsp_process = PatchProcess;
  patchMeta.fptr_MidiInHandler = PatchMidiInHandler;
  patchMeta.fptr_patch_dispose = PatchDispose;
  patchMeta.fptr_applyPreset = NULL; //ApplyPreset;
  memset(&p_state, 0, sizeof(p_state));
}
