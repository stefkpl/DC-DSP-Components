
    (*********************************************************************
     *  dmoBaseFilter.pas                                                *
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
  @abstract(DMO - BaseFilter Component that other DMOs uses as BaseStruct.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dmoBaseFilter;

interface

uses
  Classes, dmoUtils, dmoConst, MMSystem, ActiveX, dspConst, Forms, DirectShow9,
  DirectSound;

type
  { TDCDMOBaseFilter - BaseFilter that every other DMO Filter inherits. }
  TDCDMOBaseFilter = class(TComponent)
  private
    {@exclude}
    fInitialized : Boolean;
    {@exclude}
    fName : String;
    {@exclude}
    fPropGuid : TGUID;
    {@exclude}
    fFormat : TWaveFormatEx;
    {@exclude}
    fSampleSize : Cardinal;
    {@exclude}
    fProcessMessages : Boolean;
    {@exclude}
    pDMO : IMediaObject;
    {@exclude}
    pInpl : IMediaObjectInPlace;
    {@exclude}
    fEnabled : Boolean;
  protected
    {@exclude}
    procedure DoDSP(Buffer : Pointer; Size : Integer);
  public
    { Initializes the DMO Filter. }
    procedure InitDMO(DMOGuid : TGUID; INTFGuid : TGUID; out Obj);
    { Destroys the Instance. }
    destructor Destroy; override;
    { Call this to show the Property Page of the DMO Filter. This is the easiest
      way to change the Paramterers of a DMO. The Owner Parameter should be the
      Main Application, but a Value of 0 works too. }
    procedure ShowPropertyPage(Owner : Cardinal);
    { Flushes the Internal Buffer of a DMO Filter. Call this after Setting a
      Mediatype or when seeking within an Audioplayer. }
    procedure Flush;
    { Call this Method to initialize the Mediatype. If this function Fails, then
      don´t do any Processing on the DMO. Since DirectX 9 all DMO Filters Supports
      8/16 Bit integer PCM and 32Bit Float Data with 1 or 2 Channels. Any other
      Audiotype will Fail. }
    function Init(SampleRate : integer; Bits,Channels : Byte; Float : Boolean) : HRESULT;
    { Call this Method to Process Audio Data. }
    procedure Process(Buffer : Pointer; Size : integer);
  published
    { Enables or Disables the Filter. }
    property Enabled : Boolean read fEnabled write fEnabled;
    { On large Buffersize Processing the Main Application wouldn´t respond until
      the DSP is done. by using "ProcessMessages" and "SampleSize" the DSP will
      be splitted into N(N = SampleSize) Number of Samples and then Processed.
      After every Process if ProcessMessages is Enabled Application.Processmessages
      will be called. }
    property ProcessMessages : Boolean read fProcessMessages write fProcessMessages;
    { Sets the SampleSize so that the Buffer will be splitted to Process an amount
      of Data only on every Process. Set SampleSize to 0 to disable it. Prevent
      using small size Values. Use at least 1024 or more. }
    property SampleSize : Cardinal read fSampleSize write fSampleSize;
  end;

implementation

procedure TDCDMOBaseFilter.InitDMO(DMOGuid : TGUID; INTFGuid : TGUID; out Obj);
var
  lName : TDMOName;
begin
  fInitialized := False;
  DMOGetName(DMOGuid,lName);
  fName := lName;
  CoCreateInstance(DMOGuid, nil, CLSCTX_INPROC_SERVER, IID_IMediaObject, pDMO);
  Assert(pDMO <> nil, 'The DMO ' + lName + ' Filter is not available on this System.');
  pDMO.QueryInterface(INTFGuid,Obj);
  Assert(Assigned(Pointer(Obj)), 'Error getting the DMO ' + lName + ' Filter Parameter Interface.');
  pDMO.QueryInterface(IID_IMediaObjectInPlace,pInpl);
  Assert(pInpl <> nil, 'Error getting the DMO ' + lName + ' Filter Inplace Processing Interface.');
  fProcessMessages := False;
  fSampleSize := DefaultSampleSize;
  fEnabled := False;
  fPropGuid := DMOGuid;
end;

destructor TDCDMOBaseFilter.Destroy;
begin
  pInpl := nil;
  pDMO := nil;
  inherited Destroy;
end;

procedure TDCDMOBaseFilter.Process(Buffer : Pointer; Size : Integer);
var
  SplitBuffer : PChar;
  SizeLeft : integer;
  SplitSize : integer;
  CurrentSize : integer;
begin
  if not fEnabled or not fInitialized then Exit;
  if fSampleSize = 0 then
  begin
    DoDSP(Buffer,Size);
  end else
  begin
    SplitBuffer := Buffer;
    SplitSize := fSampleSize * (fFormat.wBitsPerSample div 8) * fFormat.nChannels;
    SizeLeft := Size;
    while SizeLeft > 0 do
    begin
      if SizeLeft > SplitSize then CurrentSize := SplitSize
                              else CurrentSize := SizeLeft;
      DoDSP(@SplitBuffer[Size - SizeLeft],CurrentSize);
      if fProcessMessages then Application.ProcessMessages;
      dec(SizeLeft,SplitSize);
    end;
  end;
end;

procedure TDCDMOBaseFilter.ShowPropertyPage(Owner : Cardinal);
begin
  ShowDMOFilterPropertyPage(Owner,pDMO,fName);
end;

procedure TDCDMOBaseFilter.Flush;
begin
  pDMO.Flush;
end;

function TDCDMOBaseFilter.Init(SampleRate : integer; Bits,Channels : Byte; Float : Boolean) : HRESULT;
var
  mt : TDMOMediaType;
begin
  FillChar(mt,SizeOf(TDMOMediaType),0);
  Result := MoInitMediaType(@mt, SizeOf(TWaveFormatEx));
  fInitialized := False;
  if Succeeded(Result) then
  begin
    mt.majortype  := MEDIATYPE_Audio;
    if Float then mt.subtype := MEDIASUBTYPE_IEEE_FLOAT
             else mt.subtype := MEDIASUBTYPE_PCM;
    mt.formattype := FORMAT_WaveFormatEx;

    mt.cbFormat := SizeOf(TWaveFormatEx);
    mt.pbFormat := CoTaskMemAlloc(mt.cbFormat);
    with PWaveFormatEx(mt.pbFormat)^ do
    begin
      if Float then wFormatTag := WAVE_FORMAT_IEEE_FLOAT
               else wFormatTag := WAVE_FORMAT_PCM;
      nChannels := Channels;
      nSamplesPerSec := SampleRate;
      wBitsPerSample := Bits;
      nBlockAlign := (nChannels * wBitsPerSample) div 8;
      nAvgBytesPerSec := nSamplesPerSec * nBlockAlign;
      cbSize := 0;

      Result := pDMO.SetInputType(0, @mt, 0);
      if Succeeded(Result) then Result := pDMO.SetOutputType(0, @mt, 0);
    end;

    if Succeeded(Result) then Move(mt.pbFormat^,fFormat,SizeOf(TWaveFormatEx));

    MoFreeMediaType(@mt);

    fInitialized := Result = S_OK;
  end;
end;

procedure TDCDMOBaseFilter.DoDSP(Buffer : Pointer; Size : integer);
begin
  if not fEnabled or not fInitialized
    then Exit
    else pInpl.Process(Size,Buffer,0,DMO_INPLACE_NORMAL);
end;

end.
