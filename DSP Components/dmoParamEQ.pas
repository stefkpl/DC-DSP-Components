
    (*********************************************************************
     *  dmoParamEQ.pas                                                   *
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
  @abstract(DMO - Parametric Equalizer Filter.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dmoParamEQ;

interface

uses
  Classes, dmoUtils, dmoConst, MMSystem, ActiveX, dspConst, Forms, DirectShow9,
  DirectSound, dmoBaseFilter, dmoInterfaces, Windows;

type
  { TDCDMOParamEQ - An easy to use Wrapper Component for Microsofts DMO
    Parametric Equalizer Filter. }
  TDCDMOParamEQ = class(TDCDMOBaseFilter, IDCDMOParamEQ)
  private
    {@exclude}
    pInt : IDirectSoundFXParamEQ;
    {@exclude}
    procedure SetCenter(Center : Single);
    {@exclude}
    function GetCenter : Single;
    {@exclude}
    procedure SetBandwidth(Bandwidth : Single);
    {@exclude}
    function GetBandwidth : Single;
    {@exclude}
    procedure SetGain(Gain : Single);
    {@exclude}
    function GetGain : Single;
  public
    { Creates an Instance of TDCDMOParamEQ. }
    constructor Create(AOwner : TComponent); override;
    { Destroys the Instance. }
    destructor Destroy; override;
    {@exclude}
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function SetAllParameters(const pcDsFxParamEq: TDSFXParamEq): HResult; stdcall;
    {@exclude}
    function GetAllParameters(out pDsFxParamEq: TDSFXParamEq): HResult; stdcall;
  published
    { Specifys the Center Frequency of the Equalizer.
      Minimum (DSFXPARAMEQ_CENTER_MIN) = 80.0.
      Maximum (DSFXPARAMEQ_CENTER_MAX) = 16000.0. }
    property Center : Single read GetCenter write SetCenter;
    { Specifys the Bandwidth of the Equalizer.
      Minimum (DSFXPARAMEQ_BANDWIDTH_MIN) = 1.0.
      Maximum (DSFXPARAMEQ_BANDWIDTH_MAX) = 36.0. }
    property Bandwidth : Single read GetBandwidth write SetBandwidth;
    { Specifys the Gain of the Equalizer.
      Minimum (DSFXPARAMEQ_GAIN_MIN) = -15.0.
      Maximum (DSFXPARAMEQ_GAIN_MAX) = 15.0. }
    property Gain : Single read GetGain write SetGain;
  end;

implementation

constructor TDCDMOParamEQ.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  InitDMO(GUID_DSFX_STANDARD_ParamEQ,IID_IDirectSoundFXParamEQ,pInt);
end;

destructor TDCDMOParamEQ.Destroy;
begin
  pInt := nil;
  inherited Destroy;
end;

procedure TDCDMOParamEQ.SetGain(Gain : Single);
var
  val : Single;
  opt : TDSFXParamEQ;
begin
  if Gain > DSFXPARAMEQ_GAIN_MAX then val := DSFXPARAMEQ_GAIN_MAX
  else if Gain < DSFXPARAMEQ_GAIN_MIN then val := DSFXPARAMEQ_GAIN_MIN
  else val := Gain;
  pInt.GetAllParameters(opt);
  opt.fGain := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOParamEQ.GetGain : Single;
var
  opt : TDSFXParamEQ;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fGain;
end;

procedure TDCDMOParamEQ.SetBandwidth(Bandwidth : Single);
var
  val : Single;
  opt : TDSFXParamEQ;
begin
  if Bandwidth > DSFXPARAMEQ_BANDWIDTH_MAX then val := DSFXPARAMEQ_BANDWIDTH_MAX
  else if Bandwidth < DSFXPARAMEQ_BANDWIDTH_MIN then val := DSFXPARAMEQ_BANDWIDTH_MIN
  else val := Bandwidth;
  pInt.GetAllParameters(opt);
  opt.fBandwidth := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOParamEQ.GetBandwidth : Single;
var
  opt : TDSFXParamEQ;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fBandwidth;
end;

procedure TDCDMOParamEQ.SetCenter(Center : Single);
var
  val : Single;
  opt : TDSFXParamEQ;
begin
  if Center > DSFXPARAMEQ_CENTER_MAX then val := DSFXPARAMEQ_CENTER_MAX
  else if Center < DSFXPARAMEQ_CENTER_MIN then val := DSFXPARAMEQ_CENTER_MIN
  else val := Center;
  pInt.GetAllParameters(opt);
  opt.fCenter := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOParamEQ.GetCenter : Single;
var
  opt : TDSFXParamEQ;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fCenter;
end;
(*** IDCDMOParamEQ ************************************************************)
function TDCDMOParamEQ.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  aEnabled := Enabled;
  Result := S_OK;
end;

function TDCDMOParamEQ.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Enabled := aEnabled;
  Result := S_OK;
end;

function TDCDMOParamEQ.SetAllParameters(const pcDsFxParamEq: TDSFXParamEq): HResult; stdcall;
begin
  Result := pInt.SetAllParameters(pcDsFxParamEq);
end;

function TDCDMOParamEQ.GetAllParameters(out pDsFxParamEq: TDSFXParamEq): HResult; stdcall;
begin
  Result := pInt.GetAllParameters(pDsFxParamEq);
end;

end.
