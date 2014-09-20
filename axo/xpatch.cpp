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
  static const int PARAM_INDEX_square__1_pitch = 0;
/* object classes */
  class instancesquare__1
  {
  public:			// v1
    rootc * parent2;
    KeyValuePair KVP_instancesquare__1_pitch;
    int32_t osc_p;
    static const int blepvoices = 4;
    int16_t *oscp[blepvoices];
    uint32_t nextvoice;

  public:void Init (rootc * parent)
    {
      parent2 = parent;
      parent2->PExch[0].pfunction = pfun_signed_clamp;
      parent2->PExch[0].pfunction (&parent2->PExch[0]);
      SetKVP_IPVP (&KVP_instancesquare__1_pitch, ObjectKvpRoot, "square_1",
		   &parent2->PExch[0], -134217728, 133169152);
      KVP_RegisterObject (&KVP_instancesquare__1_pitch);
      int j;
      for (j = 0; j < blepvoices; j++)
	  oscp[j] = &blept[BLEPSIZE - 1];
        nextvoice = 0;
    }
  public:void Dispose ()
    {
    }
  public:void dsp (const int32_t inlet_pitchm, int32buffer & outlet_wave)
    {
      int32_t freq;
      MTOFEXTENDED (parent2->PExch[0].finalvalue + inlet_pitchm, freq);
      int j;
      int16_t *lastblep = &blept[BLEPSIZE - 1];
      for (j = 0; j < BUFSIZE; j++)
	{
	  int i;
	  int p;
	  p = osc_p;
	  osc_p = p + freq;
	  if ((osc_p > 0) && !(p > 0))
	    {			// dispatch
	      nextvoice = (nextvoice + 1) & (blepvoices - 1);
	      int32_t x = osc_p / (freq >> 6);
	      oscp[nextvoice] = &blept[x];
	    }
	  int32_t sum = 0;
	  for (i = 0; i < blepvoices; i++)
	    {			// sample
	      int16_t *t = oscp[i];
	      sum += *t;
	      t += 64;
	      if (t >= lastblep)
		t = lastblep;
	      oscp[i] = t;
	    }
	  sum = (16384 * blepvoices) - sum - 8192;
	  uint32_t g = osc_p;
	  sum = (g >> 5) + (sum << 13);
	  outlet_wave[j] = sum;
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
  instancesquare__1 instancesquare__1_i;
  instancedac__1 instancedac__1_i;
/* net latches */
  static const int32_t *GetInitParams (void)
  {
    static const int32_t p[1] = {
      -50331648
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
    instancesquare__1_i.Init (this);
    instancedac__1_i.Init (this);
  }

/* dispose */
  void Dispose ()
  {
    instancedac__1_i.Dispose ();
    instancesquare__1_i.Dispose ();
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
    instancesquare__1_i.dsp (0, net0);
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
