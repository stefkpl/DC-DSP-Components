
    (*********************************************************************
     *  DCVISPluginAPI.pas                                               *
     *                                                                   *
     *  This unit is Part of the DC-DSP Audio Filter v1.0                *
     *                                                                   *
     *  author    : Milenko Mitrovic                                     *
     *  email     : dcoder@dsp-worx.de                                   *
     *  web       : http://dsp-worx.de                                   *
     *  date      : 04-08-2003                                           *
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

unit DCVISPluginAPI;

interface

uses
  Windows;

const
  // Used to get the Version of the Plugin SDK
  DCSDKVerVIS    = $100;
  // The Windowwidth the Plugin Configuration and Aboutbox must have.
  DCWindowWidth  = 360;
  // The Windowheight the Plugin Configuration and Aboutbox must have.
  DCWindowHeight = 300;

type
  PDCVISModule = ^TDCVISModule;
  TDCVISModule = record
    // Description of this Module. Filled in by Plugin.
    Description  : PChar;
    // Samplerate of the Stream, Filled in by Application.
    SampleRate   : DWORD;
    // Number of Channels of the Stream. Filled in by Application.
    Channels     : Byte;
    // Tells the Application how many Times the "Render" function will be called. Filled in by Plugin.
    FramesPerSec : WORD;
    // Downmix all Spectrum Channels into 1. Set to 1 to Enable and 0 to Diable. Filled in by Plugin.
    MixSpectrum  : Byte;
    // Downmix all Spectrum Channels into 1. Set to 1 to Enable and 0 to Diable. Filled in by Plugin.
    MixWaveform  : Byte;
    // Tells the Application how many Spectrum Channels this Plugins Processes. 0 = Disabled. Filled in by Plugin.
    SpectrumChan : Byte;
    // Tells the Application how many Waveform Channels this Plugins Processes. 0 = Disabled. Filled in by Plugin.
    WaveformChan : Byte;
    SpectrumData : array[0..9,0..511] of WORD;
    WaveformData : array[0..9,0..511] of WORD;
    // Initializes the Visual window. If this call Fails, then Return -1 otherwise
    // return the Handle of the created Window. It´s important to return the
    // Window Handle, because it is used to Hook the WNDProc for the Mouse and
    // Keyboard Events. If no Window is needed for the Plugin, Return 0. 
    Init         : function(Index : integer; Width : integer; Height : integer) : HRESULT; stdcall;
    Quit         : function(Index : integer) : HRESULT; stdcall;
    ConfigInit   : function(Index : integer) : HRESULT; stdcall;
    ConfigClose  : function(Index : integer) : HRESULT; stdcall;
    Render       : function(Index : integer) : HRESULT; stdcall;
    // Resize is called to to Resize the VIS Window. NOT the Config/About Window.
    Resize       : function(Index : integer; Width : integer; Height : integer) : HRESULT; stdcall;
    // Used to Send some Title Informations to the Plugin. Filled in by Application.
    SetMediaName : function (Media : PChar) : HRESULT; stdcall;
  end;

  // Defines the main Plugin.
  PDCVISPluginHeader = ^TDCVISPluginHeader;
  TDCVISPluginHeader = record
    // Must be DCSDKVerVIS. Filled in by Plugin.
    Version        : WORD;
    // Description of this Plugin. Filled in by Plugin.
    Description    : PChar;
    // The ParentWindow of Config and About. Filled in by Application.
    Parent         : hWnd;
    // The ParentWindow of the main VIS Window. Filled in by Application.
    ParentRenderer : hWnd;
    // The Library Handle. Filled in by Application.
    hDllInstance   : hWnd;
    // Retrieves a Module. Used by the Application to get a Module.
    GetModule      : function(Index : integer) : PDCVISModule; stdcall;
    // Called when the About Dialog of a Module should be initialized and shown.
    AboutInit      : function : HRESULT; stdcall;
    // Called when a About Dialog is should be closed and freed.
    AboutClose     : function : HRESULT; stdcall;
  end;

implementation

end.
