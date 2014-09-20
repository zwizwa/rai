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
     void
     xpatch_init (void)
{
  xpatch_init2 ();
}

void PatchMidiInHandler (uint8_t status, uint8_t data1, uint8_t data2);

int32buffer AudioInputLeft;
int32buffer AudioInputRight;
int32buffer AudioOutputLeft;
int32buffer AudioOutputRight;
class rootc
{
public:
  static const int NPEXCH = 1;
  ParameterExchange_t PExch[NPEXCH];
  int displayVector[5];
  static const int NPRESETS = 8;
  static const int NPRESET_ENTRIES = 32;
  static const int NMODULATIONSOURCES = 8;
  static const int NMODULATIONTARGETS = 8;
  PExModulationTarget_t
    PExModulationSources[NMODULATIONSOURCES][NMODULATIONTARGETS];
/* modsource defines */
/* parameter instance indices */
  static const int PARAM_INDEX_sine__1_pitch = 0;
/* object classes */
  class instancesine__1
  {
  public:			// v1
    rootc * parent2;
    KeyValuePair KVP_instancesine__1_pitch;
    uint32_t Phase;
  public:void Init (rootc * parent)
    {
      parent2 = parent;
      parent2->PExch[0].pfunction = pfun_signed_clamp;
      parent2->PExch[0].pfunction (&parent2->PExch[0]);
      SetKVP_IPVP (&KVP_instancesine__1_pitch, ObjectKvpRoot, "sine_1",
		   &parent2->PExch[0], -134217728, 133169152);
      KVP_RegisterObject (&KVP_instancesine__1_pitch);
      Phase = 0;
    }
  public:void Dispose ()
    {
    }
  public:void dsp (const int32_t inlet_pitchm,
	      const int32buffer inlet_fm,
	      const int32buffer inlet_pm, int32buffer & outlet_wave)
    {
      int32_t freq;
      MTOFEXTENDED (parent2->PExch[0].finalvalue + inlet_pitchm, freq);

      int buffer_index;
      for (buffer_index = 0; buffer_index < BUFSIZE; buffer_index++)
	{
	  Phase += freq + inlet_fm[buffer_index];
	  int32_t r;
	  int32_t p2 = Phase + (inlet_pm[buffer_index] << 4);
	  SINE2TINTERP (p2, r) outlet_wave[buffer_index] = (r >> 4);

	}
    }
  }
  ;
  class instancedac__1
  {
  public:			// v1
    rootc * parent2;
  public:void Init (rootc * parent)
    {
      parent2 = parent;
      parent2->displayVector[3] = 0;
      parent2->displayVector[4] = 0;
    }
  public:void Dispose ()
    {
    }
  public:void dsp (const int32buffer inlet_left,
	      const int32buffer inlet_right)
    {
      int j;
      for (j = 0; j < BUFSIZE; j++)
	{
	  AudioOutputLeft[j] += __SSAT (inlet_left[j], 28);
	  AudioOutputRight[j] += __SSAT (inlet_right[j], 28);
	}
      parent2->displayVector[3] = inlet_left[0];
      parent2->displayVector[4] = inlet_right[0];

    }
  }
  ;				/* object instances */
  instancesine__1 instancesine__1_i;
  instancedac__1 instancedac__1_i;
/* net latches */
  static const int32_t *GetInitParams (void)
  {
    static const int32_t p[1] = {
      0
    };
    return &p[0];
  }
  static const int32_t *GetPresets (void)
  {
    static const int32_t p[NPRESETS][NPRESET_ENTRIES][2] = {
      {
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0}
       },
      {
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0}
       },
      {
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0}
       },
      {
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0}
       },
      {
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0}
       },
      {
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0}
       },
      {
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0}
       },
      {
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0},
       {-1, 0}
       }
    };
    return &p[0][0][0];
  };
  void ApplyPreset (int index)
  {
    index--;
    if (index < NPRESETS)
      {
	PresetParamChange_t *pa = (PresetParamChange_t *) (GetPresets ());
	PresetParamChange_t *p = &pa[index * NPRESET_ENTRIES];
	int i;
	for (i = 0; i < NPRESET_ENTRIES; i++)
	  {
	    PresetParamChange_t *pp = &p[i];
	    if ((pp->pexIndex >= 0) && (pp->pexIndex < NPEXCH))
	      {
		PExParameterChange (&PExch[pp->pexIndex], pp->value, 0xFFEF);
	      }
	    else
	      break;
	  }
      }
  }
/* init */
  void Init ()
  {
    int i;
    int j;
    const int32_t *p;
    p = GetInitParams ();
    for (j = 0; j < 1; j++)
      {
	PExch[j].value = p[j];
	PExch[j].modvalue = p[j];
	PExch[j].signals = 0;
	PExch[j].pfunction = 0;
	PExch[j].finalvalue = p[j];
      }
    displayVector[0] = 0x446F7841;
    displayVector[1] = 0;
    displayVector[2] = 2;
    instancesine__1_i.Init (this);
    instancedac__1_i.Init (this);
  }

/* dispose */
  void Dispose ()
  {
    instancedac__1_i.Dispose ();
    instancesine__1_i.Dispose ();
  }

/* krate */
  void dsp (void)
  {
    int i;
    for (i = 0; i < BUFSIZE; i++)
      AudioOutputLeft[i] = 0;
    for (i = 0; i < BUFSIZE; i++)
      AudioOutputRight[i] = 0;
//--------- <nets> -----------//
    int32buffer net0;
//--------- </nets> ----------//
//--------- <zero> ----------//
    int32_t UNCONNECTED_OUTPUT;
    static const int32_t UNCONNECTED_INPUT = 0;
    static const int32buffer zerobuffer =
      { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    int32buffer UNCONNECTED_OUTPUT_BUFFER;
//--------- </zero> ----------//
//--------- <object calls> ----------//
    instancesine__1_i.dsp (0, zerobuffer, zerobuffer, net0);
    instancedac__1_i.dsp (net0, net0);
//--------- </object calls> ----------//
//--------- <net latch copy> ----------//
//--------- </net latch copy> ----------//
  }

  void MidiInHandler (uint8_t status, uint8_t data1, uint8_t data2)
  {
  }

};

static rootc root;
void
PatchProcess (int32_t * inbuf, int32_t * outbuf)
{
  int i;
  for (i = 0; i < BUFSIZE; i++)
    {
      AudioInputLeft[i] = inbuf[i * 2] >> 4;
      AudioInputRight[i] = inbuf[i * 2 + 1] >> 4;
    }
  root.dsp ();
  for (i = 0; i < BUFSIZE; i++)
    {
      outbuf[i * 2] = __SSAT (AudioOutputLeft[i], 28) << 4;
      outbuf[i * 2 + 1] = __SSAT (AudioOutputRight[i], 28) << 4;
    }
}

void
ApplyPreset (int32_t i)
{
  root.ApplyPreset (i);
}

void
PatchDispose ()
{
  root.Dispose ();
}

void
PatchMidiInHandler (uint8_t status, uint8_t data1, uint8_t data2)
{
  root.MidiInHandler (status, data1, data2);
}

void
xpatch_init2 (void)
{
  patchMeta.npresets = 8;
  patchMeta.npreset_entries = 32;
  patchMeta.pPresets = (PresetParamChange_t *) root.GetPresets ();
  patchMeta.pPExch = &root.PExch[0];
  patchMeta.pDisplayVector = &root.displayVector[0];
  patchMeta.numPEx = 1;
  patchMeta.fptr_dsp_process = PatchProcess;
  patchMeta.fptr_patch_dispose = PatchDispose;
  patchMeta.fptr_MidiInHandler = PatchMidiInHandler;
  patchMeta.fptr_applyPreset = ApplyPreset;
  root.Init ();
}
