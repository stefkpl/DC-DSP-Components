
    (*********************************************************************
     *  dmoChorus.pas                                                    *
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
  @abstract(DMO - Chorus Filter.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dmoChorus;

interface

uses
  Classes, dmoUtils, dmoConst, MMSystem, ActiveX, dspConst, Forms, DirectShow9,
  DirectSound, dmoBaseFilter, dmoInterfaces, Windows;

type
  { TDCDMOChorus - An easy to use Wrapper Component for Microsofts DMO
    Chorus Filter. }
  TDCDMOChorus = class(TDCDMOBaseFilter, IDCDMOChorus)
  private
    {@exclude}
    pInt : IDirectSoundFXChorus;
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
    { Creates an Instance of TDCDMOChorus. }
    constructor Create(AOwner : TComponent); override;
    { Destroys the Instance. }
    destructor Destroy; override;
    {@exclude}
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function SetAllParameters(const pcDsFxChorus: TDSFXChorus): HResult; stdcall;
    {@exclude}
    function GetAllParameters(out pDsFxChorus: TDSFXChorus): HResult; stdcall;
  published
    { Specifys the Wet and Dry Mix. Minimum (DSFXCHORUS_WETDRYMIX_MIN) = 0.0.
      Maximum (DSFXCHORUS_WETDRYMIX_MAX) = 100.0. }
    property WetDryMix : Single read GetWetDryMix write SetWetDryMix;
    { Specifys the Depth of the Filter. Minimum (DSFXCHORUS_DEPTH_MIN) = 0.0.
      Maximum (DSFXCHORUS_DEPTH_MAX) = 100.0. }
    property Depth : Single read GetDepth write SetDepth;
    { Specifys the Feedback of the Filter. Minimum (DSFXCHORUS_FEEDBACK_MIN) = -99.0.
      Maximum (DSFXCHORUS_FEEDBACK_MAX) = 99.0. }
    property Feedback : Single read GetFeedback write SetFeedback;
    { Specifys the Frequency of the Filter. Minimum (DSFXCHORUS_FREQUENCY_MIN) = 0.0.
      Maximum (DSFXCHORUS_FREQUENCY_MAX) = 10.0. }
    property Frequency : Single read getFrequency write SetFrequency;
    { Specifys the Waveform Type of the Filter. Use DSFXCHORUS_WAVE_TRIANGLE
      for a Triangle Waveform and DSFXCHORUS_WAVE_SIN for a Sine Waveform. }
    property Waveform : Longint  read GetWaveform write SetWaveform;
    { Specifys the Delay of the Filter. Minimum (DSFXCHORUS_DELAY_MIN) = 0.0.
      Maximum (DSFXCHORUS_DELAY_MAX) = 20.0. }
    property Delay : Single  read GetDelay write SetDelay;
    { Specifys the Phase Angle. Minimum (DSFXCHORUS_PHASE_MIN) = 0.
      Maximum (DSFXCHORUS_PHASE_MAX) = 4. Use one of the following Values.
      DSFXCHORUS_PHASE_NEG_180 for a Phaseshift of -180 degrees.
      DSFXCHORUS_PHASE_NEG_90 for a Phaseshift of -90 degrees.
      DSFXCHORUS_PHASE_ZERO for no Phaseshift.
      DSFXCHORUS_PHASE_90 for a Phaseshift of 90 degrees.
      DSFXCHORUS_PHASE_180 for a Phaseshift of 180 degrees. }
    property Phase : Longint  read GetPhase write SetPhase;
  end;

implementation

constructor TDCDMOChorus.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  InitDMO(GUID_DSFX_STANDARD_CHORUS,IID_IDirectSoundFXChorus,pInt);
end;

destructor TDCDMOChorus.Destroy;
begin
  pInt := nil;
  inherited Destroy;
end;

procedure TDCDMOChorus.SetWetDryMix(DryMix : Single);
var
  val : Single;
  opt : TDSFXChorus;
begin
  if DryMix > DSFXCHORUS_WETDRYMIX_MAX then val := DSFXCHORUS_WETDRYMIX_MAX
  else if DryMix < DSFXCHORUS_WETDRYMIX_MIN then val := DSFXCHORUS_WETDRYMIX_MIN
  else val := DryMix;
  pInt.GetAllParameters(opt);
  opt.fWetDryMix := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOChorus.GetWetDryMix : Single;
var
  opt : TDSFXChorus;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fWetDryMix;
end;

procedure TDCDMOChorus.SetDepth(Depth : Single);
var
  val : Single;
  opt : TDSFXChorus;
begin
  if Depth > DSFXCHORUS_DEPTH_MAX then val := DSFXCHORUS_DEPTH_MAX
  else if Depth < DSFXCHORUS_DEPTH_MIN then val := DSFXCHORUS_DEPTH_MIN
  else val := Depth;
  pInt.GetAllParameters(opt);
  opt.fDepth := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOChorus.GetDepth : Single;
var
  opt : TDSFXChorus;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fDepth;
end;

procedure TDCDMOChorus.SetFeedback(Feedback : Single);
var
  val : Single;
  opt : TDSFXChorus;
begin
  if Feedback > DSFXCHORUS_FEEDBACK_MAX then val := DSFXCHORUS_FEEDBACK_MAX
  else if Feedback < DSFXCHORUS_FEEDBACK_MIN then val := DSFXCHORUS_FEEDBACK_MIN
  else val := Feedback;
  pInt.GetAllParameters(opt);
  opt.fFeedback := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOChorus.GetFeedback : Single;
var
  opt : TDSFXChorus;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fFeedback;
end;

procedure TDCDMOChorus.SetFrequency(Frequency : Single);
var
  val : Single;
  opt : TDSFXChorus;
begin
  if Frequency > DSFXCHORUS_FREQUENCY_MAX then val := DSFXCHORUS_FREQUENCY_MAX
  else if Frequency < DSFXCHORUS_FREQUENCY_MIN then val := DSFXCHORUS_FREQUENCY_MIN
  else val := Frequency;
  pInt.GetAllParameters(opt);
  opt.fFrequency := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOChorus.GetFrequency : Single;
var
  opt : TDSFXChorus;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fFrequency;
end;

procedure TDCDMOChorus.SetWaveform(Waveform : Longint);
var
  val : Longint;
  opt : TDSFXChorus;
begin
  if Waveform > DSFXCHORUS_WAVE_SIN then val := DSFXCHORUS_WAVE_SIN
  else if Waveform < DSFXCHORUS_WAVE_TRIANGLE then val := DSFXCHORUS_WAVE_TRIANGLE
  else val := Waveform;
  pInt.GetAllParameters(opt);
  opt.lWaveform := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOChorus.GetWaveform : Longint;
var
  opt : TDSFXChorus;
begin
  pInt.GetAllParameters(opt);
  Result := opt.lWaveform;
end;

procedure TDCDMOChorus.SetDelay(Delay : Single);
var
  val : Single;
  opt : TDSFXChorus;
begin
  if Delay > DSFXCHORUS_DELAY_MAX then val := DSFXCHORUS_DELAY_MAX
  else if Delay < DSFXCHORUS_DELAY_MIN then val := DSFXCHORUS_DELAY_MIN
  else val := Delay;
  pInt.GetAllParameters(opt);
  opt.fDelay := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOChorus.GetDelay : Single;
var
  opt : TDSFXChorus;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fDelay;
end;

procedure TDCDMOChorus.SetPhase(Phase : Longint);
var
  val : Longint;
  opt : TDSFXChorus;
begin
  if Phase > DSFXCHORUS_PHASE_MAX then val := DSFXCHORUS_PHASE_MAX
  else if Phase < DSFXCHORUS_PHASE_MIN then val := DSFXCHORUS_PHASE_MIN
  else val := Phase;
  pInt.GetAllParameters(opt);
  opt.lPhase := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOChorus.GetPhase : Longint;
var
  opt : TDSFXChorus;
begin
  pInt.GetAllParameters(opt);
  Result := opt.lPhase;
end;
(*** IDCDMOChorus *************************************************************)
function TDCDMOChorus.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  aEnabled := Enabled;
  Result := S_OK;
end;

function TDCDMOChorus.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Enabled := aEnabled;
  Result := S_OK;
end;

function TDCDMOChorus.SetAllParameters(const pcDsFxChorus: TDSFXChorus): HResult; stdcall;
begin
  Result := pInt.SetAllParameters(pcDsFxChorus);
end;

function TDCDMOChorus.GetAllParameters(out pDsFxChorus: TDSFXChorus): HResult; stdcall;
begin
  Result := pInt.GetAllParameters(pDsFxChorus);
end;

end.
