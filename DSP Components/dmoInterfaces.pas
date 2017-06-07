
    (*********************************************************************
     *  dmoInterfaces.pas                                                *
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
  @abstract(Interface Declaration Unit of DMO Filters.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dmoInterfaces;

interface

uses
  Windows, DirectSound;

const
  { IDCDMOChorus identifier GUID. }
  IID_IDCDMOChorus      : TGUID = '{2F99E23A-5D6E-4B51-8269-49C46ADC745C}';
  { IDCDMOCompressor identifier GUID. }
  IID_IDCDMOCompressor  : TGUID = '{A5BA9E24-E017-48D2-9446-E42E5E1C9DCB}';
  { IDCDMODistortion identifier GUID. }
  IID_IDCDMODistortion  : TGUID = '{A09AE606-B22B-4C1B-8ACF-45285BC30925}';
  { IDCDMOEcho identifier GUID. }
  IID_IDCDMOEcho        : TGUID = '{22F654FD-7829-4BB8-9380-14EC6279D6D8}';
  { IDCDMOFlanger identifier GUID. }
  IID_IDCDMOFlanger     : TGUID = '{50CEC6C5-0A67-4BBA-BEC5-A59D86DFC12B}';
  { IDCDMOGargle identifier GUID. }
  IID_IDCDMOGargle      : TGUID = '{0F5FDB37-E3EA-4556-98E4-790345E28D8D}';
  { IDCDMOI3DL2Reverb identifier GUID. }
  IID_IDCDMOI3DL2Reverb : TGUID = '{22D00724-2D31-47E8-A5C4-3FF732224C9B}';
  { IDCDMOParamEQ identifier GUID. }
  IID_IDCDMOParamEQ     : TGUID = '{414E947F-0327-4D2F-B3CC-5AAEAB8EC4F2}';
  { IDCDMOWavesReverb identifier GUID. }
  IID_IDCDMOWavesReverb : TGUID = '{CB0809E2-8436-475A-B37E-F21BACDD5014}';

type
  { Interface used to control the DMO Filter from other programming languages.<br>
    implements IDirectSoundFXChorus. }
  IDCDMOChorus = interface(IDirectSoundFXChorus)
    ['{2F99E23A-5D6E-4B51-8269-49C46ADC745C}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
  end;

  { Interface used to control the DMO Filter from other programming languages.<br>
    implements IDirectSoundFXCompressor. }
  IDCDMOCompressor = interface(IDirectSoundFXCompressor)
    ['{A5BA9E24-E017-48D2-9446-E42E5E1C9DCB}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
  end;

  { Interface used to control the DMO Filter from other programming languages.<br>
    implements IDirectSoundFXDistortion. }
  IDCDMODistortion = interface(IDirectSoundFXDistortion)
    ['{A09AE606-B22B-4C1B-8ACF-45285BC30925}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
  end;

  { Interface used to control the DMO Filter from other programming languages.<br>
    implements IDirectSoundFXEcho. }
  IDCDMOEcho = interface(IDirectSoundFXEcho)
    ['{22F654FD-7829-4BB8-9380-14EC6279D6D8}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
  end;

  { Interface used to control the DMO Filter from other programming languages.<br>
    implements IDirectSoundFXFlanger. }
  IDCDMOFlanger = interface(IDirectSoundFXFlanger)
    ['{50CEC6C5-0A67-4BBA-BEC5-A59D86DFC12B}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
  end;

  { Interface used to control the DMO Filter from other programming languages.<br>
    implements IDirectSoundFXGargle. }
  IDCDMOGargle = interface(IDirectSoundFXGargle)
    ['{0F5FDB37-E3EA-4556-98E4-790345E28D8D}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
  end;

  { Interface used to control the DMO Filter from other programming languages.<br>
    implements IDirectSoundFXI3DL2Reverb. }
  IDCDMOI3DL2Reverb = interface(IDirectSoundFXI3DL2Reverb)
    ['{22D00724-2D31-47E8-A5C4-3FF732224C9B}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
  end;

  { Interface used to control the DMO Filter from other programming languages.<br>
    implements IDirectSoundFXParamEq. }
  IDCDMOParamEQ = interface(IDirectSoundFXParamEq)
    ['{414E947F-0327-4D2F-B3CC-5AAEAB8EC4F2}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
  end;

  { Interface used to control the DMO Filter from other programming languages.<br>
    implements IDirectSoundFXWavesReverb. }
  IDCDMOWavesReverb = interface(IDirectSoundFXWavesReverb)
    ['{CB0809E2-8436-475A-B37E-F21BACDD5014}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
  end;

implementation

end.
