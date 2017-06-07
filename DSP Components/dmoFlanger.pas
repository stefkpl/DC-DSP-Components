
    (*********************************************************************
     *  dmoFlanger.pas                                                   *
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
  @abstract(DMO - Flanger Filter.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dmoFlanger;

interface

uses
  Classes, dmoUtils, dmoConst, MMSystem, ActiveX, dspConst, Forms, DirectShow9,
  DirectSound, dmoBaseFilter, dmoInterfaces, Windows;

type
  { TDCDMOFlanger - An easy to use Wrapper Component for Microsofts DMO
    Flanger Filter. }
  TDCDMOFlanger = class(TDCDMOBaseFilter, IDCDMOFlanger)
  private
    {@exclude}
    pInt : IDirectSoundFXFlanger;
    {@exclude}
    procedure SetWetDryMix(DryMix : Single);
    {@exclude}
    function GetWetDryMix : Single;
    {@exclude}
    procedure SetDepth(Depth : Single);
    {@exclude}
    function GetDepth : Single;
    {@exclude}
    procedure SetFeedback(Feedback : Single);
    {@exclude}
    function GetFeedback : Single;
    {@exclude}
    procedure SetFrequency(Frequency : Single);
    {@exclude}
    function GetFrequency : Single;
    {@exclude}
    procedure SetWaveform(Waveform : Longint);
    {@exclude}
    function GetWaveform : Longint;
    {@exclude}
    procedure SetDelay(Delay : Single);
    {@exclude}
    function GetDelay : Single;
    {@exclude}
    procedure SetPhase(Phase : Longint);
    {@exclude}
    function GetPhase : Longint;
  public
    { Creates an Instance of TDCDMOFlanger. }
    constructor Create(AOwner : TComponent); override;
    { Destroys the Instance. }
    destructor Destroy; override;
    {@exclude}
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function SetAllParameters(const pcDsFxFlanger: TDSFXFlanger): HResult; stdcall;
    {@exclude}
    function GetAllParameters(out pDsFxFlanger: TDSFXFlanger): HResult; stdcall;
  published
    { Specifys the Wet and Dry Mix of the Filter.
      Minimum (DSFXFLANGER_WETDRYMIX_MIN) = 0.0.
      Maximum (DSFXFLANGER_WETDRYMIX_MAX) = 100.0. }
    property WetDryMix : Single read GetWetDryMix write SetWetDryMix;
    { Specifys the Depth of the Filter. Minimum (DSFXFLANGER_DEPTH_MIN) = 0.0.
      Maximum (DSFXFLANGER_DEPTH_MAX) = 100.0. }
    property Depth : Single read GetDepth write SetDepth;
    { Specifys the Feedback of the Filter. Minimum (DSFXFLANGER_FEEDBACK_MIN) = -99.0.
      Maximum (DSFXFLANGER_FEEDBACK_MAX) = 99.0. }
    property Feedback : Single read GetFeedback write SetFeedback;
    { Specifys the Frequency of the Filter. Minimum (DSFXFLANGER_FREQUENCY_MIN) = 0.0.
      Maximum (DSFXFLANGER_FREQUENCY_MAX) = 10.0. }
    property Frequency : Single read getFrequency write SetFrequency;
    { Specifys the Waveform Type of the Filter. Use DSFXFLANGER_WAVE_TRIANGLE
      for a Triangle Waveform and DSFXFLANGER_WAVE_SIN for a Sine Waveform. }
    property Waveform : Longint  read GetWaveform write SetWaveform;
    { Specifys the Delay of the Filter. Minimum (DSFXFLANGER_DELAY_MIN) = 0.0.
      Maximum (DSFXFLANGER_DELAY_MAX) = 4.0. }
    property Delay : Single  read GetDelay write SetDelay;
    { Specifys the Phase Angle. Minimum (DSFXFLANGER_PHASE_MIN) = 0.
      Maximum (DSFXFLANGER_PHASE_MAX) = 4. Use one of the following Values.
      DSFXFLANGER_PHASE_NEG_180 for a Phaseshift of -180 degrees.
      DSFXFLANGER_PHASE_NEG_90 for a Phaseshift of -90 degrees.
      DSFXFLANGER_PHASE_ZERO for no Phaseshift.
      DSFXFLANGER_PHASE_90 for a Phaseshift of 90 degrees.
      DSFXFLANGER_PHASE_180 for a Phaseshift of 180 degrees. }
    property Phase : Longint  read GetPhase write SetPhase;
  end;

implementation

constructor TDCDMOFlanger.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  InitDMO(GUID_DSFX_STANDARD_Flanger,IID_IDirectSoundFXFlanger,pInt);
end;

destructor TDCDMOFlanger.Destroy;
begin
  pInt := nil;
  inherited Destroy;
end;

procedure TDCDMOFlanger.SetWetDryMix(DryMix : Single);
var
  val : Single;
  opt : TDSFXFlanger;
begin
  if DryMix > DSFXFLANGER_WETDRYMIX_MAX then val := DSFXFLANGER_WETDRYMIX_MAX
  else if DryMix < DSFXFLANGER_WETDRYMIX_MIN then val := DSFXFLANGER_WETDRYMIX_MIN
  else val := DryMix;
  pInt.GetAllParameters(opt);
  opt.fWetDryMix := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOFlanger.GetWetDryMix : Single;
var
  opt : TDSFXFlanger;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fWetDryMix;
end;

procedure TDCDMOFlanger.SetDepth(Depth : Single);
var
  val : Single;
  opt : TDSFXFlanger;
begin
  if Depth > DSFXFLANGER_DEPTH_MAX then val := DSFXFLANGER_DEPTH_MAX
  else if Depth < DSFXFLANGER_DEPTH_MIN then val := DSFXFLANGER_DEPTH_MIN
  else val := Depth;
  pInt.GetAllParameters(opt);
  opt.fDepth := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOFlanger.GetDepth : Single;
var
  opt : TDSFXFlanger;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fDepth;
end;

procedure TDCDMOFlanger.SetFeedback(Feedback : Single);
var
  val : Single;
  opt : TDSFXFlanger;
begin
  if Feedback > DSFXFLANGER_FEEDBACK_MAX then val := DSFXFLANGER_FEEDBACK_MAX
  else if Feedback < DSFXFLANGER_FEEDBACK_MIN then val := DSFXFLANGER_FEEDBACK_MIN
  else val := Feedback;
  pInt.GetAllParameters(opt);
  opt.fFeedback := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOFlanger.GetFeedback : Single;
var
  opt : TDSFXFlanger;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fFeedback;
end;

procedure TDCDMOFlanger.SetFrequency(Frequency : Single);
var
  val : Single;
  opt : TDSFXFlanger;
begin
  if Frequency > DSFXFLANGER_FREQUENCY_MAX then val := DSFXFLANGER_FREQUENCY_MAX
  else if Frequency < DSFXFLANGER_FREQUENCY_MIN then val := DSFXFLANGER_FREQUENCY_MIN
  else val := Frequency;
  pInt.GetAllParameters(opt);
  opt.fFrequency := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOFlanger.GetFrequency : Single;
var
  opt : TDSFXFlanger;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fFrequency;
end;

procedure TDCDMOFlanger.SetWaveform(Waveform : Longint);
var
  val : Longint;
  opt : TDSFXFlanger;
begin
  if Waveform > DSFXFLANGER_WAVE_SIN then val := DSFXFLANGER_WAVE_SIN
  else if Waveform < DSFXFLANGER_WAVE_TRIANGLE then val := DSFXFLANGER_WAVE_TRIANGLE
  else val := Waveform;
  pInt.GetAllParameters(opt);
  opt.lWaveform := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOFlanger.GetWaveform : Longint;
var
  opt : TDSFXFlanger;
begin
  pInt.GetAllParameters(opt);
  Result := opt.lWaveform;
end;

procedure TDCDMOFlanger.SetDelay(Delay : Single);
var
  val : Single;
  opt : TDSFXFlanger;
begin
  if Delay > DSFXFLANGER_DELAY_MAX then val := DSFXFLANGER_DELAY_MAX
  else if Delay < DSFXFLANGER_DELAY_MIN then val := DSFXFLANGER_DELAY_MIN
  else val := Delay;
  pInt.GetAllParameters(opt);
  opt.fDelay := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOFlanger.GetDelay : Single;
var
  opt : TDSFXFlanger;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fDelay;
end;

procedure TDCDMOFlanger.SetPhase(Phase : Longint);
var
  val : Longint;
  opt : TDSFXFlanger;
begin
  if Phase > DSFXFLANGER_PHASE_MAX then val := DSFXFLANGER_PHASE_MAX
  else if Phase < DSFXFLANGER_PHASE_MIN then val := DSFXFLANGER_PHASE_MIN
  else val := Phase;
  pInt.GetAllParameters(opt);
  opt.lPhase := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOFlanger.GetPhase : Longint;
var
  opt : TDSFXFlanger;
begin
  pInt.GetAllParameters(opt);
  Result := opt.lPhase;
end;
(*** IDCDMOFlanger ************************************************************)
function TDCDMOFlanger.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  aEnabled := Enabled;
  Result := S_OK;
end;

function TDCDMOFlanger.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Enabled := aEnabled;
  Result := S_OK;
end;

function TDCDMOFlanger.SetAllParameters(const pcDsFxFlanger: TDSFXFlanger): HResult; stdcall;
begin
  Result := pInt.SetAllParameters(pcDsFxFlanger);
end;

function TDCDMOFlanger.GetAllParameters(out pDsFxFlanger: TDSFXFlanger): HResult; stdcall;
begin
  Result := pInt.GetAllParameters(pDsFxFlanger);
end;

end.
