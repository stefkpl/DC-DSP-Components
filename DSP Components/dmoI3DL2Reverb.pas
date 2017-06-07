
    (*********************************************************************
     *  dmoI3DL2Reverb.pas                                               *
     *                                                                   *
     *  This unit is Part of the DC-DSP Component Pack v1.0              *
     *                                                                   *
     *  author    : Milenko Mitrovic                                     *
     *  email     : dcoder@dsp-worx.de                                   *
     *  web       : http://dsp-worx.de                                   *
     *  date      : 24-07-2003                                           *
     *                                                                   *
     *  The contents of this file are used with permission, subject to   *
     *  the Mozilla Public License Version 1.1 (the "License"); you may  *
     *  not use this file except in compliance with the License. You may *
     *  obtain a copy of the License at                                  *
     *  http://www.mozilla.org/MPL/MPL-1.1.html                          *
     *                                                                   *
     *  Software distributed under the License is distributed on an      *
     *  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   *
     *  implied. See the License for the specific language governing     *
     *  rights and limitations under the License.                        *
     *                                                                   *
     *  (C) 2003, 2004 Milenko Mitrovic <dcoder@dsp-worx.de>             *
     *                                                                   *
     *********************************************************************)
{
  @abstract(DMO - very good 3D Reverb Filter.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dmoI3DL2Reverb;

interface

uses
  Classes, dmoUtils, dmoConst, MMSystem, ActiveX, dspConst, Forms, DirectShow9,
  DirectSound, dmoBaseFilter, dmoInterfaces, Windows;

type
  { TDCDMOI3DL2Reverb - An easy to use Wrapper Component for Microsofts DMO
    I3DL2 Reverb Filter. }
  TDCDMOI3DL2Reverb = class(TDCDMOBaseFilter, IDCDMOI3DL2Reverb)
  private
    {@exclude}
    pInt : IDirectSoundFXI3DL2Reverb;
    {@exclude}
    fPresetCount : Cardinal;
    {@exclude}
    procedure SetRoom(Room : Longint);
    {@exclude}
    function GetRoom : Longint;
    {@exclude}
    procedure SetRoomHF(RoomHF : Longint);
    {@exclude}
    function GetRoomHF : Longint;
    {@exclude}
    procedure SetRoomRolloffFactor(RoomRolloffFactor : Single);
    {@exclude}
    function GetRoomRolloffFactor : Single;
    {@exclude}
    procedure SetDecayTime(DecayTime : Single);
    {@exclude}
    function GetDecayTime : Single;
    {@exclude}
    procedure SetDecayHFRatio(DecayHFRatio : Single);
    {@exclude}
    function GetDecayHFRatio : Single;
    {@exclude}
    procedure SetReflections(Reflections : Longint);
    {@exclude}
    function GetReflections : Longint;
    {@exclude}
    procedure SetReflectionsDelay(ReflectionsDelay : Single);
    {@exclude}
    function GetReflectionsDelay : Single;
    {@exclude}
    procedure SetReverb(Reverb : Longint);
    {@exclude}
    function GetReverb : Longint;
    {@exclude}
    procedure SetReverbDelay(ReverbDelay : Single);
    {@exclude}
    function GetReverbDelay : Single;
    {@exclude}
    procedure SetDiffusion(Diffusion : Single);
    {@exclude}
    function GetDiffusion : Single;
    {@exclude}
    procedure SetDensity(Density : Single);
    {@exclude}
    function GetDensity : Single;
    {@exclude}
    procedure SetHFReference(HFReference : Single);
    {@exclude}
    function GetHFReference : Single;
    {@exclude}
    procedure Set_Quality(Quality : Longint);
    {@exclude}
    function Get_Quality : Longint;
    {@exclude}
    procedure Set_Preset(fPreset : TDCDMOI3DL2ReverbPreset);
    {@exclude}
    function Get_Preset : TDCDMOI3DL2ReverbPreset;
    {@exclude}
    function GetPresetName(Index : integer) : String;
  public
    { Creates an Instance of TDCDMOI3DL2Reverb. }
    constructor Create(AOwner : TComponent); override;
    { Destroys the Instance. }
    destructor Destroy; override;
    {@exclude}
    function Init(SampleRate : integer; Bits,Channels : Byte; Float : Boolean) : HRESULT;
    { Retrieves the Name of the selected Preset Index. If the Index is out of
      the Range, an empty String is returned. }
    property PresetName[Index : integer] : String read GetPresetName;
    {@exclude}
    procedure DoDSP(Buffer : Pointer; Size : Integer);
    {@exclude}
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function SetAllParameters(const pcDsFxI3DL2Reverb: TDSFXI3DL2Reverb): HResult; stdcall;
    {@exclude}
    function GetAllParameters(out pDsFxI3DL2Reverb: TDSFXI3DL2Reverb): HResult; stdcall;
    {@exclude}
    function SetPreset(dwPreset: DWORD): HResult; stdcall;
    {@exclude}
    function GetPreset(out pdwPreset: DWORD): HResult; stdcall;
    {@exclude}
    function SetQuality(lQuality: Longint): HResult; stdcall;
    {@exclude}
    function GetQuality(out plQuality: Longint): HResult; stdcall;
  published
    { Specifys the Room Value for the Filter.
      Minimum (DSFX_I3DL2REVERB_ROOM_MIN) = -10000.
      Maximum (DSFX_I3DL2REVERB_ROOM_MAX) = 0.
      Default (DSFX_I3DL2REVERB_ROOM_DEFAULT) = -1000. }
    property Room : Longint read GetRoom write SetRoom;
    { Specifys the RoomHF Value of the Filter.
      Minimum (DSFX_I3DL2REVERB_ROOMHF_MIN) = -10000.
      Maximum (DSFX_I3DL2REVERB_ROOMHF_MAX) = 0.
      Default (DSFX_I3DL2REVERB_ROOMHF_DEFAULT) = -100. }
    property RoomHF : Longint read GetRoomHF write SetRoomHF;
    { Specifys the Room Rolloff Factor for the Filter.
      Minimum (DSFX_I3DL2REVERB_ROOMROLLOFFFACTOR_MIN) = 0.0.
      Maximum (DSFX_I3DL2REVERB_ROOMROLLOFFFACTOR_MAX) = 10.0.
      Default (DSFX_I3DL2REVERB_ROOMROLLOFFFACTOR_DEFAULT) = 0.0. }
    property RoomRolloffFactor : Single read GetRoomRolloffFactor write SetRoomRolloffFactor;
    { Specifys the Decay Time for the Filter.
      Minimum (DSFX_I3DL2REVERB_DECAYTIME_MIN) = 0.1.
      Maximum (DSFX_I3DL2REVERB_DECAYTIME_MAX) = 20.0.
      Default (DSFX_I3DL2REVERB_DECAYTIME_DEFAULT) = 1.49. }
    property DecayTime : Single read getDecayTime write SetDecayTime;
    { Specifys the Decay HF Ratio for the Filter.
      Minimum (DSFX_I3DL2REVERB_DECAYHFRATIO_MIN) = 0.1.
      Maximum (DSFX_I3DL2REVERB_DECAYHFRATIO_MAX) = 2.0.
      Default (DSFX_I3DL2REVERB_DECAYHFRATIO_DEFAULT) = 0.83. }
    property DecayHFRatio : Single read GetDecayHFRatio write SetDecayHFRatio;
    { Specifys the Reflactions Value for the Filter.
      Minimum (DSFX_I3DL2REVERB_REFLECTIONS_MIN) = 10000.
      Maximum (DSFX_I3DL2REVERB_REFLECTIONS_MAX) = -1000.
      Default (DSFX_I3DL2REVERB_REFLECTIONS_DEFAULT) = -2602. }
    property Reflections : Longint read GetReflections write SetReflections;
    { Specifys the Reflections Delay for the Filter.
      Minimum (DSFX_I3DL2REVERB_REFLECTIONSDELAY_MIN) = 0.0.
      Maximum (DSFX_I3DL2REVERB_REFLECTIONSDELAY_MAX) = 0.3.
      Default (DSFX_I3DL2REVERB_REFLECTIONSDELAY_DEFAULT) = 0.007. }
    property ReflectionsDelay : Single  read GetReflectionsDelay write SetReflectionsDelay;
    { Specifys the Reverb Value for the Filter.
      Minimum (DSFX_I3DL2REVERB_REVERB_MIN) = -10000.
      Maximum (DSFX_I3DL2REVERB_REVERB_MAX) = 2000.
      Default (DSFX_I3DL2REVERB_REVERB_DEFAULT) = 200. }
    property Reverb : Longint read GetReverb write SetReverb;
    { Specifys the Reverb Delay for the Filter.
      Minimum (DSFX_I3DL2REVERB_REVERBDELAY_MIN) = 0.0.
      Maximum (DSFX_I3DL2REVERB_REVERBDELAY_MAX) = 0.1.
      Default (DSFX_I3DL2REVERB_REVERBDELAY_DEFAULT) = 0.011. }
    property ReverbDelay : Single read GetReverbDelay write SetReverbDelay;
    { Specifys the Diffusion Value for the Filter.
      Minimum (DSFX_I3DL2REVERB_DIFFUSION_MIN) = 0.0.
      Maximum (DSFX_I3DL2REVERB_DIFFUSION_MAX) = 100.0.
      Default (DSFX_I3DL2REVERB_DIFFUSION_DEFAULT) = 100.0. }
    property Diffusion : Single read GetDiffusion write SetDiffusion;
    { Specifys the Density Value for the Filter.
      Minimum (DSFX_I3DL2REVERB_DENSITY_MIN) = 0.0.
      Maximum (DSFX_I3DL2REVERB_DENSITY_MAX) = 100.0.
      Default (DSFX_I3DL2REVERB_DENSITY_DEFAULT) = 100.0. }
    property Density : Single read getDensity write SetDensity;
    { Specifys the HF Reference for the Filter.
      Minimum (DSFX_I3DL2REVERB_HFREFERENCE_MIN) = 20.0.
      Maximum (DSFX_I3DL2REVERB_HFREFERENCE_MAX) = 20000.0.
      Default (DSFX_I3DL2REVERB_HFREFERENCE_DEFAULT) = 5000.0. }
    property HFReference : Single read GetHFReference write SetHFReference;
    { Specifys the Quality of the Filter.
      Minimum (DSFX_I3DL2REVERB_QUALITY_MIN) = 0.
      Maximum (DSFX_I3DL2REVERB_QUALITY_MAX) = 3.
      Default (DSFX_I3DL2REVERB_QUALITY_DEFAULT) = 2. }
    property Quality : Longint read Get_Quality write Set_Quality;
    { Specifys the Preset Index of The Filter. }
    property Preset : TDCDMOI3DL2ReverbPreset read Get_Preset write Set_Preset;
    { Specifys the Presetcount of the Filter. (30) }
    property PresetCount : Cardinal read fPresetCount;
  end;

var
  DCCW : WORD;

implementation

constructor TDCDMOI3DL2Reverb.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  // Set the FPU State to Double Precision.
  Set8087CW((DCCW and $FCFF) or (512));
  InitDMO(GUID_DSFX_STANDARD_I3DL2Reverb,IID_IDirectSoundFXI3DL2Reverb,pInt);
  fPresetCount := 30;
end;

destructor TDCDMOI3DL2Reverb.Destroy;
begin
  pInt := nil;
  inherited Destroy;
end;

function TDCDMOI3DL2Reverb.GetPresetName(Index : integer) : String;
begin
  Result := '';
  case Index of
    0 : Result := 'Alley';
    1 : Result := 'Arena';
    2 : Result := 'Auditorium';
    3 : Result := 'Bathroom';
    4 : Result := 'Carpeted Hallway';
    5 : Result := 'Cave';
    6 : Result := 'City';
    7 : Result := 'Concert Hall';
    8 : Result := 'Default';
    9 : Result := 'Forrest';
    10: Result := 'Generic';
    11: Result := 'Hallway';
    12: Result := 'Hangar';
    13: Result := 'Large Hall';
    14: Result := 'Large Room';
    15: Result := 'Living Room';
    16: Result := 'Medium Hall';
    17: Result := 'Medium Room';
    18: Result := 'Mountains';
    19: Result := 'Padded Cell';
    20: Result := 'Parking Lot';
    21: Result := 'Plane';
    22: Result := 'Plate';
    23: Result := 'Quarry';
    24: Result := 'Room';
    25: Result := 'Sewer Pipe';
    26: Result := 'Small Room';
    27: Result := 'Stone Corridor';
    28: Result := 'Stone Room';
    29: Result := 'Under Water';
  end;
end;

procedure TDCDMOI3DL2Reverb.SetRoom(Room : Longint);
var
  val : Longint;
  opt : TDSFXI3DL2Reverb;
begin
  if Room > DSFX_I3DL2REVERB_ROOM_MAX then val := DSFX_I3DL2REVERB_ROOM_MAX
  else if Room < DSFX_I3DL2REVERB_ROOM_MIN then val := DSFX_I3DL2REVERB_ROOM_MIN
  else val := Room;
  pInt.GetAllParameters(opt);
  opt.lRoom := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOI3DL2Reverb.GetRoom : Longint;
var
  opt : TDSFXI3DL2Reverb;
begin
  pInt.GetAllParameters(opt);
  Result := opt.lRoom;
end;

procedure TDCDMOI3DL2Reverb.SetRoomHF(RoomHF : Longint);
var
  val : Longint;
  opt : TDSFXI3DL2Reverb;
begin
  if RoomHF > DSFX_I3DL2REVERB_ROOMHF_MAX then val := DSFX_I3DL2REVERB_ROOMHF_MAX
  else if RoomHF < DSFX_I3DL2REVERB_ROOMHF_MIN then val := DSFX_I3DL2REVERB_ROOMHF_MIN
  else val := RoomHF;
  pInt.GetAllParameters(opt);
  opt.lRoomHF := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOI3DL2Reverb.GetRoomHF : Longint;
var
  opt : TDSFXI3DL2Reverb;
begin
  pInt.GetAllParameters(opt);
  Result := opt.lRoomHF;
end;

procedure TDCDMOI3DL2Reverb.SetRoomRolloffFactor(RoomRolloffFactor : Single);
var
  val : Single;
  opt : TDSFXI3DL2Reverb;
begin
  if RoomRolloffFactor > DSFX_I3DL2REVERB_ROOMROLLOFFFACTOR_MAX then val := DSFX_I3DL2REVERB_ROOMROLLOFFFACTOR_MAX
  else if RoomRolloffFactor < DSFX_I3DL2REVERB_ROOMROLLOFFFACTOR_MIN then val := DSFX_I3DL2REVERB_ROOMROLLOFFFACTOR_MIN
  else val := RoomRolloffFactor;
  pInt.GetAllParameters(opt);
  opt.flRoomRolloffFactor := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOI3DL2Reverb.GetRoomRolloffFactor : Single;
var
  opt : TDSFXI3DL2Reverb;
begin
  pInt.GetAllParameters(opt);
  Result := opt.flRoomRolloffFactor;
end;

procedure TDCDMOI3DL2Reverb.SetDecayTime(DecayTime : Single);
var
  val : Single;
  opt : TDSFXI3DL2Reverb;
begin
  if DecayTime > DSFX_I3DL2REVERB_DECAYTIME_MAX then val := DSFX_I3DL2REVERB_DECAYTIME_MAX
  else if DecayTime < DSFX_I3DL2REVERB_DECAYTIME_MIN then val := DSFX_I3DL2REVERB_DECAYTIME_MIN
  else val := DecayTime;
  pInt.GetAllParameters(opt);
  opt.flDecayTime := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOI3DL2Reverb.GetDecayTime : Single;
var
  opt : TDSFXI3DL2Reverb;
begin
  pInt.GetAllParameters(opt);
  Result := opt.flDecayTime;
end;

procedure TDCDMOI3DL2Reverb.SetDecayHFRatio(DecayHFRatio : Single);
var
  val : Single;
  opt : TDSFXI3DL2Reverb;
begin
  if DecayHFRatio > DSFX_I3DL2REVERB_DECAYHFRATIO_MAX then val := DSFX_I3DL2REVERB_DECAYHFRATIO_MAX
  else if DecayHFRatio < DSFX_I3DL2REVERB_DECAYHFRATIO_MIN then val := DSFX_I3DL2REVERB_DECAYHFRATIO_MIN
  else val := DecayHFRatio;
  pInt.GetAllParameters(opt);
  opt.flDecayHFRatio := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOI3DL2Reverb.GetDecayHFRatio : Single;
var
  opt : TDSFXI3DL2Reverb;
begin
  pInt.GetAllParameters(opt);
  Result := opt.flDecayHFRatio;
end;

procedure TDCDMOI3DL2Reverb.SetReflections(Reflections : Longint);
var
  val : Longint;
  opt : TDSFXI3DL2Reverb;
begin
  if Reflections > DSFX_I3DL2REVERB_REFLECTIONS_MAX then val := DSFX_I3DL2REVERB_REFLECTIONS_MAX
  else if Reflections < DSFX_I3DL2REVERB_REFLECTIONS_MIN then val := DSFX_I3DL2REVERB_REFLECTIONS_MIN
  else val := Reflections;
  pInt.GetAllParameters(opt);
  opt.lReflections := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOI3DL2Reverb.GetReflections : Longint;
var
  opt : TDSFXI3DL2Reverb;
begin
  pInt.GetAllParameters(opt);
  Result := opt.lReflections;
end;

procedure TDCDMOI3DL2Reverb.SetReflectionsDelay(ReflectionsDelay : Single);
var
  val : Single;
  opt : TDSFXI3DL2Reverb;
begin
  if ReflectionsDelay > DSFX_I3DL2REVERB_REFLECTIONSDELAY_MAX then val := DSFX_I3DL2REVERB_REFLECTIONSDELAY_MAX
  else if ReflectionsDelay < DSFX_I3DL2REVERB_REFLECTIONSDELAY_MIN then val := DSFX_I3DL2REVERB_REFLECTIONSDELAY_MIN
  else val := ReflectionsDelay;
  pInt.GetAllParameters(opt);
  opt.flReflectionsDelay := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOI3DL2Reverb.GetReflectionsDelay : Single;
var
  opt : TDSFXI3DL2Reverb;
begin
  pInt.GetAllParameters(opt);
  Result := opt.flReflectionsDelay;
end;

procedure TDCDMOI3DL2Reverb.SetReverb(Reverb : Longint);
var
  val : Longint;
  opt : TDSFXI3DL2Reverb;
begin
  if Reverb > DSFX_I3DL2REVERB_REVERB_MAX then val := DSFX_I3DL2REVERB_REVERB_MAX
  else if Reverb < DSFX_I3DL2REVERB_REVERB_MIN then val := DSFX_I3DL2REVERB_REVERB_MIN
  else val := Reverb;
  pInt.GetAllParameters(opt);
  opt.lReverb := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOI3DL2Reverb.GetReverb : Longint;
var
  opt : TDSFXI3DL2Reverb;
begin
  pInt.GetAllParameters(opt);
  Result := opt.lReverb;
end;

procedure TDCDMOI3DL2Reverb.SetReverbDelay(ReverbDelay : Single);
var
  val : Single;
  opt : TDSFXI3DL2Reverb;
begin
  if ReverbDelay > DSFX_I3DL2REVERB_REVERBDELAY_MAX then val := DSFX_I3DL2REVERB_REVERBDELAY_MAX
  else if ReverbDelay < DSFX_I3DL2REVERB_REVERBDELAY_MIN then val := DSFX_I3DL2REVERB_REVERBDELAY_MIN
  else val := ReverbDelay;
  pInt.GetAllParameters(opt);
  opt.flReverbDelay := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOI3DL2Reverb.GetReverbDelay : Single;
var
  opt : TDSFXI3DL2Reverb;
begin
  pInt.GetAllParameters(opt);
  Result := opt.flReverbDelay;
end;

procedure TDCDMOI3DL2Reverb.SetDiffusion(Diffusion : Single);
var
  val : Single;
  opt : TDSFXI3DL2Reverb;
begin
  if Diffusion > DSFX_I3DL2REVERB_DIFFUSION_MAX then val := DSFX_I3DL2REVERB_DIFFUSION_MAX
  else if Diffusion < DSFX_I3DL2REVERB_DIFFUSION_MIN then val := DSFX_I3DL2REVERB_DIFFUSION_MIN
  else val := Diffusion;
  pInt.GetAllParameters(opt);
  opt.flDiffusion := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOI3DL2Reverb.GetDiffusion : Single;
var
  opt : TDSFXI3DL2Reverb;
begin
  pInt.GetAllParameters(opt);
  Result := opt.flDiffusion;
end;

procedure TDCDMOI3DL2Reverb.SetDensity(Density : Single);
var
  val : Single;
  opt : TDSFXI3DL2Reverb;
begin
  if Density > DSFX_I3DL2REVERB_DENSITY_MAX then val := DSFX_I3DL2REVERB_DENSITY_MAX
  else if Density < DSFX_I3DL2REVERB_DENSITY_MIN then val := DSFX_I3DL2REVERB_DENSITY_MIN
  else val := Density;
  pInt.GetAllParameters(opt);
  opt.flDensity := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOI3DL2Reverb.GetDensity : Single;
var
  opt : TDSFXI3DL2Reverb;
begin
  pInt.GetAllParameters(opt);
  Result := opt.flDensity;
end;

procedure TDCDMOI3DL2Reverb.SetHFReference(HFReference : Single);
var
  val : Single;
  opt : TDSFXI3DL2Reverb;
begin
  if HFReference > DSFX_I3DL2REVERB_HFREFERENCE_MAX then val := DSFX_I3DL2REVERB_HFREFERENCE_MAX
  else if HFReference < DSFX_I3DL2REVERB_HFREFERENCE_MIN then val := DSFX_I3DL2REVERB_HFREFERENCE_MIN
  else val := HFReference;
  pInt.GetAllParameters(opt);
  opt.flHFReference := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOI3DL2Reverb.GetHFReference : Single;
var
  opt : TDSFXI3DL2Reverb;
begin
  pInt.GetAllParameters(opt);
  Result := opt.flHFReference;
end;

procedure TDCDMOI3DL2Reverb.Set_Quality(Quality : Longint);
var
  val : Longint;
begin
  if Quality > DSFX_I3DL2REVERB_QUALITY_MAX then val := DSFX_I3DL2REVERB_QUALITY_MAX
  else if Quality < DSFX_I3DL2REVERB_QUALITY_MIN then val := DSFX_I3DL2REVERB_QUALITY_MIN
  else val := Quality;
  pInt.SetQuality(val);
end;

function TDCDMOI3DL2Reverb.Get_Quality : Longint;
begin
  pInt.GetQuality(Result);
end;

procedure TDCDMOI3DL2Reverb.Set_Preset(fPreset : TDCDMOI3DL2ReverbPreset);
var
  Res : Cardinal;
begin
  Res := Cardinal(fPreset);
  pInt.SetPreset(Res);
end;

function TDCDMOI3DL2Reverb.Get_Preset : TDCDMOI3DL2ReverbPreset;
var
  Res : Cardinal;
begin
  pInt.GetPreset(Res);
  if Res > 29 then Result := None else Result := TDCDMOI3DL2ReverbPreset(Res);
end;

function TDCDMOI3DL2Reverb.Init(SampleRate : integer; Bits,Channels : Byte; Float : Boolean) : HRESULT;
begin
  // In DirectShow some Players reset the FPU after rendering.
  // Set it back here, just to be sure.
  Set8087CW((DCCW and $FCFF) or (512));
  Result := inherited Init(SampleRate,Bits,Channels,Float);
end;

procedure TDCDMOI3DL2Reverb.DoDSP(Buffer : Pointer; Size : integer);
begin
  // In DirectShow some Players reset the FPU after rendering.
  // Set it back here, just to be sure.
  Set8087CW((DCCW and $FCFF) or (512));
  inherited DoDSP(Buffer,Size)
end;
(*** IDCDMOI3DL2Reverb ********************************************************)
function TDCDMOI3DL2Reverb.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  aEnabled := Enabled;
  Result := S_OK;
end;

function TDCDMOI3DL2Reverb.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Enabled := aEnabled;
  Result := S_OK;
end;

function TDCDMOI3DL2Reverb.SetAllParameters(const pcDsFxI3DL2Reverb: TDSFXI3DL2Reverb): HResult; stdcall;
begin
  Result := pInt.SetAllParameters(pcDsFxI3DL2Reverb);
end;

function TDCDMOI3DL2Reverb.GetAllParameters(out pDsFxI3DL2Reverb: TDSFXI3DL2Reverb): HResult; stdcall;
begin
  Result := pInt.GetAllParameters(pDsFxI3DL2Reverb);
end;

function TDCDMOI3DL2Reverb.SetPreset(dwPreset: DWORD): HResult; stdcall;
begin
  Result := pInt.SetPreset(dwPreset);
end;

function TDCDMOI3DL2Reverb.GetPreset(out pdwPreset: DWORD): HResult; stdcall;
begin
  Result := pInt.GetPreset(pdwPreset);
end;

function TDCDMOI3DL2Reverb.SetQuality(lQuality: Longint): HResult; stdcall;
begin
  Result := pInt.SetQuality(lQuality);
end;

function TDCDMOI3DL2Reverb.GetQuality(out plQuality: Longint): HResult; stdcall;
begin
  Result := pInt.GetQuality(plQuality);
end;

initialization
  // FPU need to be set to Single or Double Precision or it will give Floating
  // Point Errors. 512 = Double .. 256 = Single
  // EDIT: FPU State is only set when this Filter has been created.
  DCCW := Get8087CW;

finalization
  // Restore the previuosly saved FPU state again.
  Set8087CW(DCCW);
end.
