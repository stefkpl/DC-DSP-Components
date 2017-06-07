
    (*********************************************************************
     *  dmoWavesReverb.pas                                               *
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
  @abstract(DMO - Waves Reverb Filter.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dmoWavesReverb;

interface

uses
  Classes, dmoUtils, dmoConst, MMSystem, ActiveX, dspConst, Forms, DirectShow9,
  DirectSound, dmoBaseFilter, dmoInterfaces, Windows;

type
  { TDCDMOWavesReverb - An easy to use Wrapper Component for Microsofts DMO
    WavesReverb Filter. }
  TDCDMOWavesReverb = class(TDCDMOBaseFilter, IDCDMOWavesReverb)
  private
    {@exclude}
    pInt : IDirectSoundFXWavesReverb;
    {@exclude}
    procedure SetInGain(InGain : Single);
    {@exclude}
    function GetInGain : Single;
    {@exclude}
    procedure SetReverbMix(ReverbMix : Single);
    {@exclude}
    function GetReverbMix : Single;
    {@exclude}
    procedure SetReverbTime(ReverbTime : Single);
    {@exclude}
    function GetReverbTime : Single;
    {@exclude}
    procedure SetHighFreqRTRatio(HighFreqRTRatio : Single);
    {@exclude}
    function GetHighFreqRTRatio : Single;
  public
    { Creates an Instance of TDCDMOWavesReverb. }
    constructor Create(AOwner : TComponent); override;
    { Destroys the Instance. }
    destructor Destroy; override;
    {@exclude}
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function SetAllParameters(const pcDsFxWavesReverb: TDSFXWavesReverb): HResult; stdcall;
    {@exclude}
    function GetAllParameters(out pDsFxWavesReverb: TDSFXWavesReverb): HResult; stdcall;
  published
    { Specifys InGain of the Filter.
      Minimum (DSFX_WAVESREVERB_INGAIN_MIN) = -96.0.
      Maximum (DSFX_WAVESREVERB_INGAIN_MAX) = 0.0.
      Default (DSFX_WAVESREVERB_INGAIN_DEFAULT) = 0.0. }
    property InGain : Single read GetInGain write SetInGain;
    { Specifys the Reverb Mix of the Filter.
      Minimum (DSFX_WAVESREVERB_REVERBMIX_MIN) = -96.0.
      Maximum (DSFX_WAVESREVERB_REVERBMIX_MAX) = 0.0.
      Default (DSFX_WAVESREVERB_REVERBMIX_DEFAULT) = 0.0. }
    property ReverbMix : Single read GetReverbMix write SetReverbMix;
    { Specifys the Reverb Time of the Filter.
      Minimum (DSFX_WAVESREVERB_REVERBTIME_MIN) = 0.001.
      Maximum (DSFX_WAVESREVERB_REVERBTIME_MAX) = 3000.0.
      Default (DSFX_WAVESREVERB_REVERBTIME_DEFAULT) = 1000.0. }
    property ReverbTime : Single read GetReverbTime write SetReverbTime;
    { Specifys the High Frequency Ratio.
      Minimum (DSFX_WAVESREVERB_HIGHFREQRTRATIO_MIN) = 0.001.
      Maximum (DSFX_WAVESREVERB_HIGHFREQRTRATIO_MAX) = 0.999.
      Default (DSFX_WAVESREVERB_HIGHFREQRTRATIO_DEFAULT) = 0.001. }
    property HighFreqRTRatio : Single read getHighFreqRTRatio write SetHighFreqRTRatio;
  end;

implementation

constructor TDCDMOWavesReverb.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  InitDMO(GUID_DSFX_WAVES_REVERB,IID_IDirectSoundFXWavesReverb,pInt);
end;

destructor TDCDMOWavesReverb.Destroy;
begin
  pInt := nil;
  inherited Destroy;
end;

procedure TDCDMOWavesReverb.SetInGain(InGain : Single);
var
  val : Single;
  opt : TDSFXWavesReverb;
begin
  if InGain > DSFX_WAVESREVERB_INGAIN_MAX then val := DSFX_WAVESREVERB_INGAIN_MAX
  else if InGain < DSFX_WAVESREVERB_INGAIN_MIN then val := DSFX_WAVESREVERB_INGAIN_MIN
  else val := InGain;
  pInt.GetAllParameters(opt);
  opt.fInGain := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOWavesReverb.GetInGain : Single;
var
  opt : TDSFXWavesReverb;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fInGain;
end;

procedure TDCDMOWavesReverb.SetReverbMix(ReverbMix : Single);
var
  val : Single;
  opt : TDSFXWavesReverb;
begin
  if ReverbMix > DSFX_WAVESREVERB_REVERBMIX_MAX then val := DSFX_WAVESREVERB_REVERBMIX_MAX
  else if ReverbMix < DSFX_WAVESREVERB_REVERBMIX_MIN then val := DSFX_WAVESREVERB_REVERBMIX_MIN
  else val := ReverbMix;
  pInt.GetAllParameters(opt);
  opt.fReverbMix := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOWavesReverb.GetReverbMix : Single;
var
  opt : TDSFXWavesReverb;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fReverbMix;
end;

procedure TDCDMOWavesReverb.SetReverbTime(ReverbTime : Single);
var
  val : Single;
  opt : TDSFXWavesReverb;
begin
  if ReverbTime > DSFX_WAVESREVERB_REVERBTIME_MAX then val := DSFX_WAVESREVERB_REVERBTIME_MAX
  else if ReverbTime < DSFX_WAVESREVERB_REVERBTIME_MIN then val := DSFX_WAVESREVERB_REVERBTIME_MIN
  else val := ReverbTime;
  pInt.GetAllParameters(opt);
  opt.fReverbTime := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOWavesReverb.GetReverbTime : Single;
var
  opt : TDSFXWavesReverb;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fReverbTime;
end;

procedure TDCDMOWavesReverb.SetHighFreqRTRatio(HighFreqRTRatio : Single);
var
  val : Single;
  opt : TDSFXWavesReverb;
begin
  if HighFreqRTRatio > DSFX_WAVESREVERB_HIGHFREQRTRATIO_MAX then val := DSFX_WAVESREVERB_HIGHFREQRTRATIO_MAX
  else if HighFreqRTRatio < DSFX_WAVESREVERB_HIGHFREQRTRATIO_MIN then val := DSFX_WAVESREVERB_HIGHFREQRTRATIO_MIN
  else val := HighFreqRTRatio;
  pInt.GetAllParameters(opt);
  opt.fHighFreqRTRatio := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOWavesReverb.GetHighFreqRTRatio : Single;
var
  opt : TDSFXWavesReverb;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fHighFreqRTRatio;
end;
(*** IDCDMOWavesReverb ********************************************************)
function TDCDMOWavesReverb.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  aEnabled := Enabled;
  Result := S_OK;
end;

function TDCDMOWavesReverb.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Enabled := aEnabled;
  Result := S_OK;
end;

function TDCDMOWavesReverb.SetAllParameters(const pcDsFxWavesReverb: TDSFXWavesReverb): HResult; stdcall;
begin
  Result := pInt.SetAllParameters(pcDsFxWavesReverb);
end;

function TDCDMOWavesReverb.GetAllParameters(out pDsFxWavesReverb: TDSFXWavesReverb): HResult; stdcall;
begin
  Result := pInt.GetAllParameters(pDsFxWavesReverb);
end;

end.
