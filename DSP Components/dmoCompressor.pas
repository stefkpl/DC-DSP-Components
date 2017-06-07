
    (*********************************************************************
     *  dmoCompressor.pas                                                *
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
  @abstract(DMO - Compressor Filter.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dmoCompressor;

interface

uses
  Classes, dmoUtils, dmoConst, MMSystem, ActiveX, dspConst, Forms, DirectShow9,
  DirectSound, dmoBaseFilter, dmoInterfaces, Windows;

type
  { TDCDMOCompressor - An easy to use Wrapper Component for Microsofts DMO
    Compressor Filter. }
  TDCDMOCompressor = class(TDCDMOBaseFilter, IDCDMOCompressor)
  private
    {@exclude}
    pInt : IDirectSoundFXCompressor;
    {@exclude}
    procedure SetGain(Gain : Single);
    {@exclude}
    function GetGain : Single;
    {@exclude}
    procedure SetAttack(Attack : Single);
    {@exclude}
    function GetAttack : Single;
    {@exclude}
    procedure SetRelease(Release : Single);
    {@exclude}
    function GetRelease : Single;
    {@exclude}
    procedure SetThreshold(Threshold : Single);
    {@exclude}
    function GetThreshold : Single;
    {@exclude}
    procedure SetRatio(Ratio : Single);
    {@exclude}
    function GetRatio : Single;
    {@exclude}
    procedure SetPredelay(Predelay : Single);
    {@exclude}
    function GetPredelay : Single;
  public
    { Creates an Instance of TDCDMOCompressor. }
    constructor Create(AOwner : TComponent); override;
    { Destroys the Instance. }
    destructor Destroy; override;
    {@exclude}
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function SetAllParameters(const pcDsFxCompressor: TDSFXCompressor): HResult; stdcall;
    {@exclude}
    function GetAllParameters(out pDsFxCompressor: TDSFXCompressor): HResult; stdcall;
  published
    { Specifys the Gain of the Filter. Minimum (DSFXCOMPRESSOR_GAIN_MIN) = -60.0.
      Maximum (DSFXCOMPRESSOR_GAIN_MAX) = 60.0. }
    property Gain : Single read GetGain write SetGain;
    { Specify the Attack Time in ms. Minimum (DSFXCOMPRESSOR_ATTACK_MIN) = 0.01.
      Maximum (DSFXCOMPRESSOR_ATTACK_MAX) = 500.0. }
    property Attack : Single read GetAttack write SetAttack;
    { Specifys the Release Time in ms. Minimum (DSFXCOMPRESSOR_RELEASE_MIN) = 50.0.
      Maximum (DSFXCOMPRESSOR_RELEASE_MAX) = 3000.0. }
    property Release : Single read GetRelease write SetRelease;
    { Specifyds the Threshold of the Filter. Minimum (DSFXCOMPRESSOR_THRESHOLD_MIN) = -60.0.
      Maximum (DSFXCOMPRESSOR_THRESHOLD_MAX) = 0.0. }
    property Threshold : Single read GetThreshold write SetThreshold;
    { Specifys the Ratio of the Filter. minimum (DSFXCOMPRESSOR_RATIO_MIN) = 1.0.
      Maximum (DSFXCOMPRESSOR_RATIO_MAX) = 100.0. }
    property Ratio : Single read GetRatio write SetRatio;
    { Specifys the Pre Delay of the Filter. Minimum (DSFXCOMPRESSOR_PREDELAY_MIN) = 0.0.
      Maximum (DSFXCOMPRESSOR_PREDELAY_MAX) = 4.0. }
    property PreDelay : Single read GetPreDelay write SetPreDelay;
  end;

implementation

constructor TDCDMOCompressor.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  InitDMO(GUID_DSFX_STANDARD_Compressor,IID_IDirectSoundFXCompressor,pInt);
end;

destructor TDCDMOCompressor.Destroy;
begin
  pInt := nil;
  inherited Destroy;
end;

procedure TDCDMOCompressor.SetGain(Gain : Single);
var
  val : Single;
  opt : TDSFXCompressor;
begin
  if Gain > DSFXCOMPRESSOR_GAIN_MAX then val := DSFXCOMPRESSOR_GAIN_MAX
  else if Gain < DSFXCOMPRESSOR_GAIN_MIN then val := DSFXCOMPRESSOR_GAIN_MIN
  else val := Gain;
  pInt.GetAllParameters(opt);
  opt.fGain := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOCompressor.GetGain : Single;
var
  opt : TDSFXCompressor;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fGain;
end;

procedure TDCDMOCompressor.SetAttack(Attack : Single);
var
  val : Single;
  opt : TDSFXCompressor;
begin
  if Attack > DSFXCOMPRESSOR_ATTACK_MAX then val := DSFXCOMPRESSOR_ATTACK_MAX
  else if Attack < DSFXCOMPRESSOR_ATTACK_MIN then val := DSFXCOMPRESSOR_ATTACK_MIN
  else val := Attack;
  pInt.GetAllParameters(opt);
  opt.fAttack := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOCompressor.GetAttack : Single;
var
  opt : TDSFXCompressor;
begin
  result := 0;
  if not assigned(pInt) then Exit;
  pInt.GetAllParameters(opt);
  Result := opt.fAttack;
end;

procedure TDCDMOCompressor.SetRelease(Release : Single);
var
  val : Single;
  opt : TDSFXCompressor;
begin
  if Release > DSFXCOMPRESSOR_RELEASE_MAX then val := DSFXCOMPRESSOR_RELEASE_MAX
  else if Release < DSFXCOMPRESSOR_RELEASE_MIN then val := DSFXCOMPRESSOR_RELEASE_MIN
  else val := Release;
  pInt.GetAllParameters(opt);
  opt.fRelease := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOCompressor.GetRelease : Single;
var
  opt : TDSFXCompressor;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fRelease;
end;

procedure TDCDMOCompressor.SetThreshold(Threshold : Single);
var
  val : Single;
  opt : TDSFXCompressor;
begin
  if Threshold > DSFXCOMPRESSOR_THRESHOLD_MAX then val := DSFXCOMPRESSOR_THRESHOLD_MAX
  else if Threshold < DSFXCOMPRESSOR_THRESHOLD_MIN then val := DSFXCOMPRESSOR_THRESHOLD_MIN
  else val := Threshold;
  pInt.GetAllParameters(opt);
  opt.fThreshold := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOCompressor.GetThreshold : Single;
var
  opt : TDSFXCompressor;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fThreshold;
end;

procedure TDCDMOCompressor.SetRatio(Ratio : Single);
var
  val : Single;
  opt : TDSFXCompressor;
begin
  if Ratio > DSFXCOMPRESSOR_RATIO_MAX then val := DSFXCOMPRESSOR_RATIO_MAX
  else if Ratio < DSFXCOMPRESSOR_RATIO_MIN then val := DSFXCOMPRESSOR_RATIO_MIN
  else val := Ratio;
  pInt.GetAllParameters(opt);
  opt.fRatio := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOCompressor.GetRatio : Single;
var
  opt : TDSFXCompressor;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fRatio;
end;

procedure TDCDMOCompressor.SetPredelay(Predelay : Single);
var
  val : Single;
  opt : TDSFXCompressor;
begin
  if Predelay > DSFXCOMPRESSOR_PREDELAY_MAX then val := DSFXCOMPRESSOR_PREDELAY_MAX
  else if Predelay < DSFXCOMPRESSOR_PREDELAY_MIN then val := DSFXCOMPRESSOR_PREDELAY_MIN
  else val := Predelay;
  pInt.GetAllParameters(opt);
  opt.fPredelay := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOCompressor.GetPredelay : Single;
var
  opt : TDSFXCompressor;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fPredelay;
end;
(*** IDCDMOCompressor *********************************************************)
function TDCDMOCompressor.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  aEnabled := Enabled;
  Result := S_OK;
end;

function TDCDMOCompressor.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Enabled := aEnabled;
  Result := S_OK;
end;

function TDCDMOCompressor.SetAllParameters(const pcDsFxCompressor: TDSFXCompressor): HResult; stdcall;
begin
  Result := pInt.SetAllParameters(pcDsFxCompressor);
end;

function TDCDMOCompressor.GetAllParameters(out pDsFxCompressor: TDSFXCompressor): HResult; stdcall;
begin
  Result := pInt.GetAllParameters(pDsFxCompressor);
end;

end.
