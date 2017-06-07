
    (*********************************************************************
     *  dspGargle.pas                                                    *
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
  @abstract(DMO - Gargle Filter.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Jul 07, 2003)
}

unit dmoGargle;

interface

uses
  Classes, dmoUtils, dmoConst, MMSystem, ActiveX, dspConst, Forms, DirectShow9,
  DirectSound, dmoBaseFilter, dmoInterfaces, Windows;

type
  { TDCDMOGargle - An easy to use Wrapper Component for Microsoft's DMO
    Gargle Filter. }
  TDCDMOGargle = class(TDCDMOBaseFilter, IDCDMOGargle)
  private
    {@exclude}
    pInt : IDirectSoundFXGargle;
    {@exclude}
    procedure SetRateHz(Rate : Cardinal);
    {@exclude}
    function GetRateHz : Cardinal;
    {@exclude}
    procedure SetWaveShape(WaveShape : Cardinal);
    {@exclude}
    function GetWaveShape : Cardinal;
  public
    { Creates an Instance of TDCDMOGargle. }
    constructor Create(AOwner : TComponent); override;
    { Destroys the Instance. }
    destructor Destroy; override;
    {@exclude}
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function SetAllParameters(const pcDsFxGargle: TDSFXGargle): HResult; stdcall;
    {@exclude}
    function GetAllParameters(out pDsFxGargle: TDSFXGargle): HResult; stdcall;
  published
    { Specifys the Frequency of the Gargle Filter.<br>
      Minimum (DSFXGARGLE_RATEHZ_MIN) = 1.<br>
      Maximum (DSFXGARGLE_RATEHZ_MAX) = 1000. }
    property RateHz : Cardinal read GetRateHz write SetRateHz;
    { Specifys the Wave Shaper of the Filter. Use DSFXGARGLE_WAVE_TRIANGLE
      for a Triangle Waveshape and DSFXGARGLE_WAVE_SQUARE for a Square Waveshape. }
    property WaveShape : Cardinal read GetWaveShape write SetWaveShape;
  end;

implementation

constructor TDCDMOGargle.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  InitDMO(GUID_DSFX_STANDARD_Gargle,IID_IDirectSoundFXGargle,pInt);
end;

destructor TDCDMOGargle.Destroy;
begin
  pInt := nil;
  inherited Destroy;
end;

procedure TDCDMOGargle.SetRateHz(Rate : Cardinal);
var
  val : Cardinal;
  opt : TDSFXGargle;
begin
  if Rate > DSFXGARGLE_RATEHZ_MAX then val := DSFXGARGLE_RATEHZ_MAX
  else if Rate < DSFXGARGLE_RATEHZ_MIN then val := DSFXGARGLE_RATEHZ_MIN
  else val := Rate;
  pInt.GetAllParameters(opt);
  opt.dwRateHz := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOGargle.GetRateHz : Cardinal;
var
  opt : TDSFXGargle;
begin
  pInt.GetAllParameters(opt);
  Result := opt.dwRateHz;
end;

procedure TDCDMOGargle.SetWaveShape(WaveShape : Cardinal);
var
  val : Cardinal;
  opt : TDSFXGargle;
begin
  val := WaveShape;
  if WaveShape > DSFXGARGLE_WAVE_SQUARE then val := DSFXGARGLE_WAVE_SQUARE;
  pInt.GetAllParameters(opt);
  opt.dwWaveShape := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOGargle.GetWaveShape : Cardinal;
var
  opt : TDSFXGargle;
begin
  pInt.GetAllParameters(opt);
  Result := opt.dwWaveShape;
end;
(*** IDCDMOFlanger ************************************************************)
function TDCDMOGargle.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  aEnabled := Enabled;
  Result := S_OK;
end;

function TDCDMOGargle.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Enabled := aEnabled;
  Result := S_OK;
end;

function TDCDMOGargle.SetAllParameters(const pcDsFxGargle: TDSFXGargle): HResult; stdcall;
begin
  Result := pInt.SetAllParameters(pcDsFxGargle);
end;

function TDCDMOGargle.GetAllParameters(out pDsFxGargle: TDSFXGargle): HResult; stdcall;
begin
  Result := pInt.GetAllParameters(pDsFxGargle);
end;

end.
