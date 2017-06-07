
    (*********************************************************************
     *  dmoDistortion.pas                                                *
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
  @abstract(DMO - Distortion Filter.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dmoDistortion;

interface

uses
  Windows, Classes, dmoUtils, dmoConst, MMSystem, ActiveX, dspConst, Forms, DirectShow9,
  DirectSound, dmoBaseFilter, dmoInterfaces;

type
  { TDCDMODistortion - An easy to use Wrapper Component for Microsofts DMO
    Distortion Filter. }
  TDCDMODistortion = class(TDCDMOBaseFilter, IDCDMODistortion)
  private
    {@exclude}
    pInt : IDirectSoundFXDistortion;
    {@exclude}
    procedure SetGain(fGain : Single);
    {@exclude}
    function GetGain : Single;
    {@exclude}
    procedure SetEdge(fEdge : Single);
    {@exclude}
    function GetEdge : Single;
    {@exclude}
    procedure SetPostEQCenterFrequency(fPostEQCenterFrequency : Single);
    {@exclude}
    function GetPostEQCenterFrequency : Single;
    {@exclude}
    procedure SetPostEQBandwidth(fPostEQBandwidth : Single);
    {@exclude}
    function GetPostEQBandwidth : Single;
    {@exclude}
    procedure SetPreLowpassCutoff(fPreLowpassCutoff : Single);
    {@exclude}
    function GetPreLowpassCutoff : Single;
  public
    { Creates an Instance of TDCDMODistortion. }
    constructor Create(AOwner : TComponent); override;
    { Destroys the Instance. }
    destructor Destroy; override;
    {@exclude}
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function SetAllParameters(const pcDsFxDistortion: TDSFXDistortion): HResult; stdcall;
    {@exclude}
    function GetAllParameters(out pDsFxDistortion: TDSFXDistortion): HResult; stdcall;
  published
    { Specifys the Gain of the Filter. Minimum (DSFXDISTORTION_GAIN_MIN) = -60.0.
      Maximum (DSFXDISTORTION_GAIN_MAX) = 0.0. }
    property Gain : Single read GetGain write SetGain;
    { Specifys the Edgy of the Filter. Minimum (DSFXDISTORTION_EDGE_MIN) = 0.0.
      Maximum (DSFXDISTORTION_EDGE_MAX) = 100.0. }
    property Edge : Single read GetEdge write SetEdge;
    { Specifys the Center Frequency of the Distortion Equalizer.
      Minimum (DSFXDISTORTION_POSTEQCENTERFREQUENCY_MIN) = 100.0.
      Maximum (DSFXDISTORTION_POSTEQCENTERFREQUENCY_MAX) = 8000.0. }
    property PostEQCenterFrequency : Single read GetPostEQCenterFrequency write SetPostEQCenterFrequency;
    { Specifys the Bandwithd of the Equalizer.
      Minimum (DSFXDISTORTION_POSTEQBANDWIDTH_MIN) = 100.0.
      Maximum (DSFXDISTORTION_POSTEQBANDWIDTH_MAX) = 8000.0. }
    property PostEQBandwidth : Single read GetPostEQBandwidth write SetPostEQBandwidth;
    { Specifys the Lowpass Cutoff of the Filter.
      Minimum (DSFXDISTORTION_PRELOWPASSCUTOFF_MIN) = 100.0.
      Maximum (DSFXDISTORTION_PRELOWPASSCUTOFF_MAX) = 8000.0. }
    property PreLowpassCutoff : Single read GetPreLowpassCutoff write SetPreLowpassCutoff;
  end;

implementation

constructor TDCDMODistortion.Create(AOwner : TComponent);
var
  opt : TDSFXDistortion;
  CW : Word;
begin
  inherited Create(AOwner);
  InitDMO(GUID_DSFX_STANDARD_Distortion,IID_IDirectSoundFXDistortion,pInt);

  // When Debugging SetAllPArameters will raise an exception the first Time called.
  // Now disable Floating Point Exceptions on the FPU and run the initialisation.
  CW := Get8087CW;
  Set8087CW($133F);
  if Assigned(pInt) and (pInt.GetAllParameters(opt) = S_OK) then
    pInt.SetAllParameters(opt);
  // Set back the previously saved FPU State.
  Set8087CW(CW);
end;

destructor TDCDMODistortion.Destroy;
begin
  pInt := nil;
  inherited Destroy;
end;

procedure TDCDMODistortion.SetGain(fGain : Single);
var
  val : Single;
  opt : TDSFXDistortion;
begin
  if Gain > DSFXDISTORTION_GAIN_MAX then val := DSFXDISTORTION_GAIN_MAX
  else if Gain < DSFXDISTORTION_GAIN_MIN then val := DSFXDISTORTION_GAIN_MIN
  else val := Gain;
  if Assigned(pInt) and (pInt.GetAllParameters(opt) = S_OK) then
  begin
    opt.fGain := val;
    pInt.SetAllParameters(opt);
  end;
end;

function TDCDMODistortion.GetGain : Single;
var
  opt : TDSFXDistortion;
begin
  Result := 0;
  if Assigned(pInt) and (pInt.GetAllParameters(opt) = S_OK) then Result := opt.fGain;
end;

procedure TDCDMODistortion.SetEdge(fEdge : Single);
var
  val : Single;
  opt : TDSFXDistortion;
begin
  if fEdge > DSFXDISTORTION_EDGE_MAX then val := DSFXDISTORTION_EDGE_MAX
  else if fEdge < DSFXDISTORTION_EDGE_MIN then val := DSFXDISTORTION_EDGE_MIN
  else val := fEdge;
  if Assigned(pInt) and (pInt.GetAllParameters(opt) = S_OK) then
  begin
    opt.fEdge := val;
    pInt.SetAllParameters(opt);
  end;
end;

function TDCDMODistortion.GetEdge : Single;
var
  opt : TDSFXDistortion;
begin
  Result := 0;
  if Assigned(pInt) and (pInt.GetAllParameters(opt) = S_OK) then Result := opt.fEdge;
end;

procedure TDCDMODistortion.SetPostEQCenterFrequency(fPostEQCenterFrequency : Single);
var
  val : Single;
  opt : TDSFXDistortion;
begin
  if fPostEQCenterFrequency > DSFXDISTORTION_POSTEQCENTERFREQUENCY_MAX then val := DSFXDISTORTION_POSTEQCENTERFREQUENCY_MAX
  else if fPostEQCenterFrequency < DSFXDISTORTION_POSTEQCENTERFREQUENCY_MIN then val := DSFXDISTORTION_POSTEQCENTERFREQUENCY_MIN
  else val := fPostEQCenterFrequency;
  if Assigned(pInt) and (pInt.GetAllParameters(opt) = S_OK) then
  begin
    opt.fPostEQCenterFrequency := val;
    pInt.SetAllParameters(opt);
  end;
end;

function TDCDMODistortion.GetPostEQCenterFrequency : Single;
var
  opt : TDSFXDistortion;
begin
  Result := 0;
  if Assigned(pInt) and (pInt.GetAllParameters(opt) = S_OK) then Result := opt.fPostEQCenterFrequency;
end;

procedure TDCDMODistortion.SetPostEQBandwidth(fPostEQBandwidth : Single);
var
  val : Single;
  opt : TDSFXDistortion;
begin
  if fPostEQBandwidth > DSFXDISTORTION_POSTEQBANDWIDTH_MAX then val := DSFXDISTORTION_POSTEQBANDWIDTH_MAX
  else if fPostEQBandwidth < DSFXDISTORTION_POSTEQBANDWIDTH_MIN then val := DSFXDISTORTION_POSTEQBANDWIDTH_MIN
  else val := fPostEQBandwidth;
  if Assigned(pInt) and (pInt.GetAllParameters(opt) = S_OK) then
  begin
    opt.fPostEQBandwidth := val;
    pInt.SetAllParameters(opt);
  end;
end;

function TDCDMODistortion.GetPostEQBandwidth : Single;
var
  opt : TDSFXDistortion;
begin
  Result := 0;
  if Assigned(pInt) and (pInt.GetAllParameters(opt) = S_OK) then Result := opt.fPostEQBandwidth;
end;

procedure TDCDMODistortion.SetPreLowpassCutoff(fPreLowpassCutoff : Single);
var
  val : Single;
  opt : TDSFXDistortion;
begin
  if fPreLowpassCutoff > DSFXDISTORTION_PRELOWPASSCUTOFF_MAX then val := DSFXDISTORTION_PRELOWPASSCUTOFF_MAX
  else if fPreLowpassCutoff < DSFXDISTORTION_PRELOWPASSCUTOFF_MIN then val := DSFXDISTORTION_PRELOWPASSCUTOFF_MIN
  else val := fPreLowpassCutoff;
  if Assigned(pInt) and (pInt.GetAllParameters(opt) = S_OK) then
  begin
    opt.fPreLowpassCutoff := val;
    pInt.SetAllParameters(opt);
  end;
end;

function TDCDMODistortion.GetPreLowpassCutoff : Single;
var
  opt : TDSFXDistortion;
begin
  Result := 0;
  if Assigned(pInt) and (pInt.GetAllParameters(opt) = S_OK) then Result := opt.fPreLowpassCutoff;
end;
(*** IDCDMODistortion *********************************************************)
function TDCDMODistortion.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  aEnabled := Enabled;
  Result := S_OK;
end;

function TDCDMODistortion.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Enabled := aEnabled;
  Result := S_OK;
end;

function TDCDMODistortion.SetAllParameters(const pcDsFxDistortion: TDSFXDistortion): HResult; stdcall;
begin
  Result := pInt.SetAllParameters(pcDsFxDistortion);
end;

function TDCDMODistortion.GetAllParameters(out pDsFxDistortion: TDSFXDistortion): HResult; stdcall;
begin
  Result := pInt.GetAllParameters(pDsFxDistortion);
end;

end.
