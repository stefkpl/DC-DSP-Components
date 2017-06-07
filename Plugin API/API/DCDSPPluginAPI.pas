
    (*********************************************************************
     *  DCDSPPluginAPI.pas                                               *
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

unit DCDSPPluginAPI;

interface

uses
  Windows;

const
  // Used to get the Version of the Plugin SDK
  DCSDKVerDSP    = $100;
  // The Windowwidth the Plugin Configuration and Aboutbox must have.
  DCWindowWidth  = 360;
  // The Windowheight the Plugin Configuration and Aboutbox must have.
  DCWindowHeight = 300;

type
  // Defines one Plugin Module.
  PDCDSPModule = ^TDCDSPModule;
  TDCDSPModule = record
    // Description of this Module. Filled in by Plugin.
    Description   : PChar;
    // Called when a Module has been Enabled.
    Init          : function(Index : integer) : HRESULT; stdcall;
    // Called when a Module has been Disabled.
    Quit          : function(Index : integer) : HRESULT; stdcall;
    // Called when the Config Dialog of a Module should be initialized and shown.
    ConfigInit    : function(Index : integer) : HRESULT; stdcall;
    // Called when a Config Dialog is should be closed and freed.
    ConfigClose   : function(Index : integer) : HRESULT; stdcall;
    // Called to Modify a SampleBuffer.
    // Buffer : specifys the Pointer to the Buffer.
    // Size   : specifys the Size in bytes of the Buffer.
    // Channels : specifys the number of Channels.
    // Bits : specifys the Bits per Sample.
    // Frequency : specifys the Samplerate of the Stream.
    // Float : specifys whether a 32 Bit Buffer is Floating Point or integer.
    // The Result must be the new Size of the Buffer. If the Buffersize doesn´t
    // change, then set the Result to the Size Parameter.
    ModifySamples : function(Index : integer; Buffer : Pointer; Size : integer; Channels, Bits : Byte; Frequency : integer; Float : Boolean) : HRESULT; stdcall;
    // called when a new Mediafile is rendered or when a Seeking has occuered.
    // Use it to Flush the Buffers.
    Flush         : function : HRESULT; stdcall;
  end;

  // Defines the main Plugin.
  PDCDSPPluginHeader = ^TDCDSPPluginHeader;
  TDCDSPPluginHeader = record
    // Must be zpSDKVerDSP. Filled in by Plugin.
    Version      : WORD;
    // Description of this Plugin. Filled in by Plugin.
    Description  : PChar;
    // The ParentWindow of the Plugin. Filled in by Application.
    Parent       : hWnd;
    // The Library Handle. Filled in by Application.
    hDllInstance : hWnd;
    // Retrieves a Module. Used by the Application to get a Module.
    GetModule    : function(Index : integer) : PDCDSPModule; stdcall;
    // Called when the About Dialog of a Module should be initialized and shown.
    AboutInit    : function : HRESULT; stdcall;
    // Called when a About Dialog is should be closed and freed.
    AboutClose   : function : HRESULT; stdcall;
  end;

implementation

end.
