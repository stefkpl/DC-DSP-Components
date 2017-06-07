
    (*********************************************************************
     *  dmoEcho.pas                                                      *
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
  @abstract(DMO - Echo Filter.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dmoEcho;

interface

uses
  Classes, dmoUtils, dmoConst, MMSystem, ActiveX, dspConst, Forms, DirectShow9,
  DirectSound, dmoBaseFilter, dmoInterfaces, Windows;

type
  { TDCDMOEcho - An easy to use Wrapper Component for Microsofts DMO
    Echo Filter. }
  TDCDMOEcho = class(TDCDMOBaseFilter, IDCDMOEcho)
  private
    {@exclude}
    pInt : IDirectSoundFXEcho;
    {@exclude}
    procedure SetWetDryMix(WetDryMix : Single);
    {@exclude}
    function GetWetDryMix : Single;
    {@exclude}
    procedure SetFeedback(Feedback : Single);
    {@exclude}
    function GetFeedback : Single;
    {@exclude}
    procedure SetLeftDelay(LeftDelay : Single);
    {@exclude}
    function GetLeftDelay : Single;
    {@exclude}
    procedure SetRightDelay(RightDelay : Single);
    {@exclude}
    function GetRightDelay : Single;
    {@exclude}
    procedure SetPanDelay(PanDelay : Longint);
    {@exclude}
    function GetPanDelay : Longint;
  public
    { Creates an Instance of TDCDMOEcho. }
    constructor Create(AOwner : TComponent); override;
    { Destroys the Instance. }
    destructor Destroy; override;
    {@exclude}
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function SetAllParameters(const pcDsFxEcho: TDSFXEcho): HResult; stdcall;
    {@exclude}
    function GetAllParameters(out pDsFxEcho: TDSFXEcho): HResult; stdcall;
  published
    { Specifys the Wet and Dry Mix of the Filter.
      Minimum (DSFXECHO_WETDRYMIX_MIN) = 0.0.
      Maximum (DSFXECHO_WETDRYMIX_MAX) = 100.0. }
    property WetDryMix : Single read GetWetDryMix write SetWetDryMix;
    { Specifys the Feedback of the Filter.
      Minimum (DSFXECHO_FEEDBACK_MIN) = 0.0.
      Maximum (DSFXECHO_FEEDBACK_MAX) = 100.0. }
    property Feedback : Single read GetFeedback write SetFeedback;
    { Specifys the Left Delay of the Filter.
      Minimum (DSFXECHO_LEFTDELAY_MIN) = 1.0.
      Maximum (DSFXECHO_LEFTDELAY_MAX) = 2000.0. }
    property LeftDelay : Single read GetLeftDelay write SetLeftDelay;
    { Specifys the Right Delay of the Filter.
      Minimum (DSFXECHO_RIGHTDELAY_MIN) = 1.0.
      Maximum (DSFXECHO_RIGHTDELAY_MAX) = 2000.0. }
    property RightDelay : Single read GetRightDelay write SetRightDelay;
    { Specifys the Pan Delay of the Filter.
      Minimum (DSFXECHO_PANDELAY_MIN) = 0.
      Maximum (DSFXECHO_PANDELAY_MAX) = 1. }
    property PanDelay : Longint read GetPanDelay write SetPanDelay;
  end;

implementation

constructor TDCDMOEcho.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  InitDMO(GUID_DSFX_STANDARD_Echo,IID_IDirectSoundFXEcho,pInt);
end;

destructor TDCDMOEcho.Destroy;
begin
  pInt := nil;
  inherited Destroy;
end;

procedure TDCDMOEcho.SetWetDryMix(WetDryMix : Single);
var
  val : Single;
  opt : TDSFXEcho;
begin
  if WetDryMix > DSFXECHO_WETDRYMIX_MAX then val := DSFXECHO_WETDRYMIX_MAX
  else if WetDryMix < DSFXECHO_WETDRYMIX_MIN then val := DSFXECHO_WETDRYMIX_MIN
  else val := WetDryMix;
  pInt.GetAllParameters(opt);
  opt.fWetDryMix := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOEcho.GetWetDryMix : Single;
var
  opt : TDSFXEcho;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fWetDryMix;
end;

procedure TDCDMOEcho.SetFeedback(Feedback : Single);
var
  val : Single;
  opt : TDSFXEcho;
begin
  if Feedback > DSFXECHO_FEEDBACK_MAX then val := DSFXECHO_FEEDBACK_MAX
  else if Feedback < DSFXECHO_FEEDBACK_MIN then val := DSFXECHO_FEEDBACK_MIN
  else val := Feedback;
  pInt.GetAllParameters(opt);
  opt.fFeedback := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOEcho.GetFeedback : Single;
var
  opt : TDSFXEcho;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fFeedback;
end;

procedure TDCDMOEcho.SetLeftDelay(LeftDelay : Single);
var
  val : Single;
  opt : TDSFXEcho;
begin
  if LeftDelay > DSFXECHO_LEFTDELAY_MAX then val := DSFXECHO_LEFTDELAY_MAX
  else if LeftDelay < DSFXECHO_LEFTDELAY_MIN then val := DSFXECHO_LEFTDELAY_MIN
  else val := LeftDelay;
  pInt.GetAllParameters(opt);
  opt.fLeftDelay := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOEcho.GetLeftDelay : Single;
var
  opt : TDSFXEcho;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fLeftDelay;
end;

procedure TDCDMOEcho.SetRightDelay(RightDelay : Single);
var
  val : Single;
  opt : TDSFXEcho;
begin
  if RightDelay > DSFXECHO_RIGHTDELAY_MAX then val := DSFXECHO_RIGHTDELAY_MAX
  else if RightDelay < DSFXECHO_RIGHTDELAY_MIN then val := DSFXECHO_RIGHTDELAY_MIN
  else val := RightDelay;
  pInt.GetAllParameters(opt);
  opt.fRightDelay := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOEcho.GetRightDelay : Single;
var
  opt : TDSFXEcho;
begin
  pInt.GetAllParameters(opt);
  Result := opt.fRightDelay;
end;

procedure TDCDMOEcho.SetPanDelay(PanDelay : Longint);
var
  val : Longint;
  opt : TDSFXEcho;
begin
  if PanDelay > DSFXECHO_PANDELAY_MAX then val := DSFXECHO_PANDELAY_MAX
  else if PanDelay < DSFXECHO_PANDELAY_MIN then val := DSFXECHO_PANDELAY_MIN
  else val := PanDelay;
  pInt.GetAllParameters(opt);
  opt.lPanDelay := val;
  pInt.SetAllParameters(opt);
end;

function TDCDMOEcho.GetPanDelay : Longint;
var
  opt : TDSFXEcho;
begin
  pInt.GetAllParameters(opt);
  Result := opt.lPanDelay;
end;
(*** IDCDMOEcho ***************************************************************)
function TDCDMOEcho.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  aEnabled := Enabled;
  Result := S_OK;
end;

function TDCDMOEcho.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Enabled := aEnabled;
  Result := S_OK;
end;

function TDCDMOEcho.SetAllParameters(const pcDsFxEcho: TDSFXEcho): HResult; stdcall;
begin
  Result := pInt.SetAllParameters(pcDsFxEcho);
end;

function TDCDMOEcho.GetAllParameters(out pDsFxEcho: TDSFXEcho): HResult; stdcall;
begin
  Result := pInt.GetAllParameters(pDsFxEcho);
end;

end.
