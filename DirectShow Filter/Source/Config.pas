
    (*********************************************************************
     *  Config.pas                                                       *
     *                                                                   *
     *  This unit is Part of the DC-DSP Audio Filter v1.0                *
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

unit Config;

interface

uses
  Windows, DirectShow9, ActiveX, DSFilter, DSUtil, DCDSPTypes;

var
  hr : HRESULT;
  DCDSPFilter : IBaseFilter;

function configure : HResult; stdcall;

implementation

procedure Clear;
begin
  DCDSPFilter := nil;
  CoUninitialize;
end;

function configure : HResult; stdcall;
begin
  CoInitialize(nil);
  hr := CoCreateInstance(CLSID_DCDSPFilter, nil, CLSCTX_INPROC, IID_IBaseFilter, DCDSPFilter);
  if hr <> S_OK then
  begin
    Clear;
    MessageBox(0,'Error creating Filter (maybe not registered)','Error',MB_OK);
    Result := hr;
    Exit;
  end;
  ShowFilterPropertyPage(0,DCDSPFilter);
  Clear;
  Result := hr;
end;

end.
