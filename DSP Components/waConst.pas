
    (*********************************************************************
     *  waConst.pas                                                      *
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
  @abstract(Constants to be used with Winamp2 Plugin Wrappers.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit waConst;

interface

uses
  dspConst, Windows, Messages;

const
  {@exclude}
  UM_DC_LOAD = WM_USER + 1;
  {@exclude}
  UM_DC_FREE = WM_USER + 2;
  {@exclude}
  UM_DC_INIT = WM_USER + 3;
  {@exclude}
  UM_DC_QUIT = WM_USER + 4;
  {@exclude}
  UM_DC_CONFIG = WM_USER + 5;
  {@exclude}
  UM_DC_RENDER = WM_USER + 6;

type
  {@exclude}
  TDCWAVisualEvent = (veNone, veInit, veRender, veQuit);

  {@exclude}
  PWinampDSPModule = ^TWinampDSPModule ;
  {@exclude}
  TWinampDSPModule = record
    description : PChar;
    hMainWindow : integer;
    hDllInstance : integer;
    Config : procedure (This_Mod : PWinampDSPModule); cdecl;
    Init : function  (This_Mod : PWinampDSPModule) : Integer; cdecl;
    ModifySamples : function  (This_Mod : PWinampDSPModule; Samples : PSmallIntArray; numsamples, bps, nch, srate : Integer) : Integer; cdecl;
    Quit : procedure (This_Mod : PWinampDSPModule); cdecl;
    UserData : Pointer;
  end;

  {@exclude}
  PWinampDSPHeader = ^TWinampDSPHeader;
  {@exclude}
  TWinampDSPHeader = record
    Version : Integer;
    Description : PChar;
    GetModule : function (i : Integer) : PWinampDSPModule ; cdecl;
  end;

  {@exclude}
  PWinampVisModule = ^TWinampVisModule;
  {@exclude}
  TWinampVisModule = record
    description : PChar;
    hwndParent : integer;
    hDllInstance :integer;
    sRate,nCh,
    latencyMs,
    delayMs,
    spectrumNch,
    waveformNch : integer;
    spectrumData : array[0..1,0..575] of Char;
    waveformData : array[0..1,0..575] of Char;
    Config : procedure (this_mod : PWinampVisModule); cdecl;
    Init : function (this_mod : PWinampVisModule) : integer; cdecl;
    Render : function (this_mod : PWinampVisModule) : integer; cdecl;
    Quit : procedure (this_mod : PWinampVisModule); cdecl;
    userData : Pointer;
  end;

  {@exclude}
  PWinampVisHeader = ^TWinampVisHeader;
  {@exclude}
  TWinampVisHeader = record
    version : integer;
    description : PChar;
    getModule : function(i : integer) : PWinampVisModule; cdecl;
  end;

  PVisBuffer = ^TVisBuffer;
  TVisBuffer = array[0..1] of array[0..575] of Byte;
  
implementation

end.
