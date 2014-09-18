extern "C"
{
#include "../firmware/patch.h"
#include "../firmware/axoloti.h"
#include "../firmware/parameter_functions.h"
}
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-parameter"
#define MIDICHANNEL 0		// DEPRECATED!
void xpatch_init2 (void);
extern "C" __attribute__ ((section (".boot")))
void xpatch_init (void) {
  xpatch_init2 ();
}

void PatchMidiInHandler (uint8_t status, uint8_t data1, uint8_t data2);

void PatchProcess (int32_t * inbuf, int32_t * outbuf) {
  int i;
  for (i = 0; i < BUFSIZE; i++) {
    outbuf[i * 2]     = i;
    outbuf[i * 2 + 1] = i;
  }
}

void ApplyPreset (int32_t i) {}
void PatchDispose () {}
void PatchMidiInHandler (uint8_t status, uint8_t data1, uint8_t data2) {} 

void xpatch_init2 (void) {
  patchMeta.npresets = 0;
  patchMeta.npreset_entries = 0;
  patchMeta.pPresets = NULL;
  patchMeta.pPExch = NULL;
  patchMeta.pDisplayVector = NULL;
  patchMeta.numPEx = 0;
  patchMeta.fptr_dsp_process = PatchProcess;
  patchMeta.fptr_patch_dispose = PatchDispose;
  patchMeta.fptr_MidiInHandler = PatchMidiInHandler;
  patchMeta.fptr_applyPreset = ApplyPreset;
}
