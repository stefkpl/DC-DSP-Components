
    (*********************************************************************
     *  dspEchoDelay.pas                                                 *
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
  @abstract(DSP Filter that creates Echos/Delays of Audio Data.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dspEchoDelay;

interface

uses
  Windows, Classes, dspConst, dspUtils, Forms, Math, dspHighpass, dspInterfaces;

type
  { TDCEchoDelay - DSP Echo/Delay Filter that supprts multiple Echos. }
  TDCEchoDelay = class(TComponent, IDCEchoDelay)
  protected
    {@exclude}
    fKillMain : Boolean;
    {@exclude}
    fNumDelays : Byte;
    {@exclude}
    fDoHighpass : Boolean;
    {@exclude}
    fDelayAmp : WORD;
    {@exclude}
    fDelaySize : WORD;
    {@exclude}
    fEchoBuffer : Pointer;
    {@exclude}
    fEchoSwapBuffer : Pointer;
    {@exclude}
    fEBuffer : array[0.. MaxDelays -1] of Pointer;
    {@exclude}
    fEchoBufferCount : integer;
    {@exclude}
    fEchoPos : array[0..MaxDelays -1] of integer;
    {@exclude}
    fMultiDelays : Boolean;
    {@exclude}
    fHighPass : TDCHighpass;
    {@exclude}
    fSeperate : Boolean;
    {@exclude}
    fEnabled : Boolean;
    {@exclude}
    fProcessMessages : Boolean;
    {@exclude}
    fSampleSize : Cardinal;
    {@exclude}
    procedure DoDSP(Buffer : Pointer; Size : Integer; Samplerate : integer; Bits : Byte; Channels : Byte; Float : Boolean);
  public
    { Creates an Instance of TDCEchoDelay. }
    constructor Create(AOwner: TComponent); override;
    { Destroys the Instance. }
    destructor Destroy; override;
    { Call this Method to Process Audio Data. Buffer must be aligned on a 16 Byte
      boundary. Use the 'GetAlignedMemory' to create such a Buffer. Float is needed
      for 32 Bit Buffers, to determane whether the Buffer is Float or Integer. }
    procedure Process(Buffer : Pointer; Size : Integer; Samplerate : integer; Bits : Byte; Channels : Byte; Float : Boolean);
    { Initializes the Filter. }
    procedure Init(SampleRate : integer; Bits : Byte; Channels : Byte; MultiDelays : Boolean);
    { Flushes the Filter. }
    procedure Flush;
    {@exclude}
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function get_KillMain(out aKillMain: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_KillMain(aKillMain: BOOL): HRESULT; stdcall;
    {@exclude}
    function get_NumDelays(out aNumDelays: Byte): HRESULT; stdcall;
    {@exclude}
    function set_NumDelays(aNumDelays: Byte): HRESULT; stdcall;
    {@exclude}
    function get_Highpass(out aHighpass: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Highpass(aHighpass: BOOL): HRESULT; stdcall;
    {@exclude}
    function get_DelayAmp(out aDelayAmp: WORD): HRESULT; stdcall;
    {@exclude}
    function set_DelayAmp(aDelayAmp: WORD): HRESULT; stdcall;
    {@exclude}
    function get_Delay(out aDelay: WORD): HRESULT; stdcall;
    {@exclude}
    function set_Delay(aDelay: WORD): HRESULT; stdcall;
  published
    { Enables or Disables seperate Delays. If Enabled every Channel will
      be Amplified by it´s own Amplification Value. If Disabled every Channel
      will be Amplified with the Amplification Value of Channel 0. }
    property Seperate : Boolean read fSeperate write fSeperate;
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
    { Specifys whether the Main Signal should be deleted. }
    property KillMain : Boolean read fKillMain write fKillMain;
    { Specifys the Number of Delays. }
    property NumDelays : Byte read fNumDelays write fNumDelays;
    { Specifys whether the Delays will be passed through a Highpass Filter. }
    property Highpass : Boolean read fDoHighpass write fDoHighpass;
    { Specifys the Ampliffcation of the Echos. }
    property DelayAmp : WORD read fDelayAmp write fDelayAmp;
    { Specifys the Delay in Milliseconds of the Echos. }
    property Delay : WORD read fDelaySize write fDelaySize;
  end;

implementation

uses SysUtils;

constructor TDCEchoDelay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fHighPass := TDCHighpass.Create(Self);
  fHighPass.Cutoff[0] := 500;
  fHighPass.Seperate := False;
  fHighPass.Enabled := True;
  fSeperate := False;
  fEnabled := False;
  fProcessMessages := False;
  fNumDelays := 1;
  fSampleSize := DefaultSampleSize;
end;

destructor TDCEchoDelay.Destroy;
begin
  if fEchoBuffer <> nil then FreeMem(fEchoBuffer);
  if fEchoSwapBuffer <> nil then FreeMem(fEchoSwapBuffer);
  fHighPass.Free;
  inherited Destroy;
end;

procedure TDCEchoDelay.Process(Buffer : Pointer; Size : Integer; Samplerate : integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  SplitBuffer : PChar;
  SizeLeft : integer;
  SplitSize : integer;
  CurrentSize : integer;
begin
  if not fEnabled then Exit;
  if fSampleSize = 0 then
  begin
    DoDSP(Buffer,Size,Samplerate,Bits,Channels,Float);
  end else
  begin
    SplitBuffer := Buffer;
    SplitSize := fSampleSize * (Bits div 8) * Channels;
    SizeLeft := Size;
    while SizeLeft > 0 do
    begin
      if SizeLeft > SplitSize then CurrentSize := SplitSize
                              else CurrentSize := SizeLeft;
      DoDSP(@SplitBuffer[Size - SizeLeft],CurrentSize,Samplerate,Bits,Channels,Float);
      if fProcessMessages then Application.ProcessMessages;
      dec(SizeLeft,SplitSize);
    end;
  end;
end;

procedure TDCEchoDelay.DoDSP(Buffer : Pointer; Size : Integer; Samplerate : integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  Buf8,EBuf8,TBuf8 : PByteArray;
  Buf16,EBuf16,TBuf16 : PSmallIntArray;
  Buf32,EBuf32,TBuf32 : PFloatArray;
  Buf24,EBuf24,TBuf24 : PInteger24Array;
  Buf32i,EBuf32i,TBuf32i : PIntegerArray;
  i,c,n,z,tmp,epos,
  NumSamples : integer;
  dSize,
  dsize2 : integer;
  tmp2 : Single;
  tmp3 : int64;
begin
  if not fMultiDelays then
  begin
    fKillMain := True;
    fNumDelays := 1;
    fDoHighPass := False;
    fDelayAmp := 10000;
  end;
  NumSamples := Size div (Bits div 8) div Channels;
  dsize := Round(fDelaySize * Samplerate / 10000);
  epos := 0;

  case Bits of
    8:
    begin
      Buf8 := PByteArray(Buffer);
      TBuf8 := fEchoSwapBuffer;
      Move(Buf8^,TBuf8^,Size);
      if fDoHighPass then fHighPass.Process(TBuf8,Size,Samplerate,Bits,Channels,Float);
      for n := 0 to fNumDelays -1 do
      begin
        EBuf8 := PByteArray(fEBuffer[n]);
        for c := 0 to Channels -1 do
        begin
          epos := fEchoPos[n];
          dSize2 := dsize * (n+1);
          for i := 0 to NumSamples -1 do
          begin
            if fKillMain then tmp := Round((EBuf8^[epos * Channels + c] - 128) * (fDelayAmp / 10000))
                         else tmp := Round((Buf8^[i * Channels + c] - 128) + (EBuf8^[epos * Channels + c] - 128) * (fDelayAmp / 10000 / fNumDelays * (fNumDelays - n)));
            EBuf8^[epos * Channels + c] := TBuf8^[i * Channels + c];
            Buf8^[i * Channels + c] := clip_8(tmp) + 128;
            inc(epos);
            if (epos >= dsize2) then epos := 0;
          end;
        end;
        fEchoPos[n] := epos;
        if fKillMain then break;
      end;
    end;
    16:
    begin
      Buf16 := PSmallIntArray(Buffer);
      TBuf16 := fEchoSwapBuffer;
      Move(Buf16^,TBuf16^,Size);
      if fDoHighPass then fHighPass.Process(TBuf16,Size,Samplerate,Bits,Channels,Float);
      for n := 0 to fNumDelays -1 do
      begin
        EBuf16 := PSmallIntArray(fEBuffer[n]);
        for c := 0 to Channels -1 do
        begin
          epos := fEchoPos[n];
          dSize2 := dsize * (n+1);
          for i := 0 to NumSamples -1 do
          begin
            if fKillMain then tmp := Round(EBuf16^[epos * Channels + c] * (fDelayAmp / 10000))
                         else tmp := Round(Buf16^[i * Channels + c] + EBuf16^[epos * Channels + c] * (fDelayAmp / 10000 / fNumDelays * (fNumDelays - n)));
            EBuf16^[epos * Channels + c] := TBuf16^[i * Channels + c];
            Buf16^[i * Channels + c] := clip_16(tmp);
            inc(epos);
            if (epos >= dsize2) then epos := 0;
          end;
        end;
        fEchoPos[n] := epos;
        if fKillMain then break;
      end;
    end;
    24:
    begin
      Buf24 := PInteger24Array(Buffer);
      TBuf24 := fEchoSwapBuffer;
      Move(Buf24^,TBuf24^,Size);
      if fDoHighPass then fHighPass.Process(TBuf24,Size,Samplerate,Bits,Channels,Float);
      for n := 0 to fNumDelays -1 do
      begin
        EBuf24 := PInteger24Array(fEBuffer[n]);
        for c := 0 to Channels -1 do
        begin
          epos := fEchoPos[n];
          dSize2 := dsize * (n+1);
          for i := 0 to NumSamples -1 do
          begin
            if fKillMain then tmp := Round(Cvt24BitTo32(EBuf24^[epos * Channels + c]) * (fDelayAmp / 10000))
                         else tmp := Round(Cvt24BitTo32(Buf24^[i * Channels + c]) + Cvt24BitTo32(EBuf24^[epos * Channels + c]) * (fDelayAmp / 10000 / fNumDelays * (fNumDelays - n)));
            EBuf24^[epos * Channels + c] := TBuf24^[i * Channels + c];
            Buf24^[i * Channels + c] := Cvt32BitTo24(clip_24(tmp));
            inc(epos);
            if (epos >= dsize2) then epos := 0;
          end;
        end;
        fEchoPos[n] := epos;
        if fKillMain then break;
      end;
    end;
    32:
    begin
      if Float then
      begin
        Buf32 := PFloatArray(Buffer);
        TBuf32 := fEchoSwapBuffer;
        Move(Buf32^,TBuf32^,Size);
        if fDoHighPass then fHighPass.Process(TBuf32,Size,Samplerate,Bits,Channels,Float);
        for n := 0 to fNumDelays -1 do
        begin
          EBuf32 := PFloatArray(fEBuffer[n]);
          for c := 0 to Channels -1 do
          begin
            epos := fEchoPos[n];
            dSize2 := dsize * (n+1);
            for i := 0 to NumSamples -1 do
            begin
              if fKillMain then tmp2 := EBuf32^[epos * Channels + c] * (fDelayAmp / 10000)
                           else tmp2 := Buf32^[i * Channels + c] + EBuf32^[epos * Channels + c] * (fDelayAmp / 10000 / fNumDelays * (fNumDelays - n));
              EBuf32^[epos * Channels + c] := TBuf32^[i * Channels + c];
              Buf32^[i * Channels + c] := tmp2;
              inc(epos);
              if (epos >= dsize2) then epos := 0;
            end;
          end;
          fEchoPos[n] := epos;
          if fKillMain then break;
        end;
      end else
      begin
        Buf32i := PIntegerArray(Buffer);
        TBuf32i := fEchoSwapBuffer;
        Move(Buf32i^,TBuf32i^,Size);
        if fDoHighPass then fHighPass.Process(TBuf32i,Size,Samplerate,Bits,Channels,Float);
        for n := 0 to fNumDelays -1 do
        begin
          EBuf32i := PIntegerArray(fEBuffer[n]);
          for c := 0 to Channels -1 do
          begin
            epos := fEchoPos[n];
            dSize2 := dsize * (n+1);
            for i := 0 to NumSamples -1 do
            begin
              if fKillMain then tmp3 := Round(Int64(EBuf32i^[epos * Channels + c]) * (fDelayAmp / 10000))
                           else tmp3 := Round(Int64(Buf32i^[i * Channels + c]) + Int64(EBuf32i^[epos * Channels + c]) * (fDelayAmp / 10000 / fNumDelays * (fNumDelays - n)));
              EBuf32i^[epos * Channels + c] := TBuf32i^[i * Channels + c];
              Buf32i^[i * Channels + c] := clip_32(tmp3);
              inc(epos);
              if (epos >= dsize2) then epos := 0;
            end;
          end;
          fEchoPos[n] := epos;
          if fKillMain then break;
        end;
      end;
    end;
  end;
end;

procedure TDCEchoDelay.Flush;
var
  i : integer;
begin
  for i := 0 to MaxDelays -1 do fEchoPos[i] := 0;
  ZeroMemory(fEchoBuffer,fEchoBufferCount);
  fHighPass.Flush;
end;

procedure TDCEchoDelay.Init(SampleRate : integer; Bits : Byte; Channels : Byte; MultiDelays : Boolean);
var
  i : integer;
begin
  if fEchoBuffer <> nil then FreeMem(fEchoBuffer);
  fEchoBuffer := nil;
  if fEchoSwapBuffer <> nil then FreeMem(fEchoSwapBuffer);
  fEchoSwapBuffer := nil;
  fMultiDelays := MultiDelays;
  fEchoSwapBuffer := AllocMem(SampleRate * (Bits div 8) * Channels);
  for i := 0 to MaxDelays -1 do fEchoPos[i] := 0;
  fEchoBufferCount := SampleRate * (Bits div 8) * Channels * 3 * MaxDelays;
  if not fMultiDelays then fEchoBufferCount := fEchoBufferCount div 10;
  fEchoBuffer := AllocMem(fEchoBufferCount * 2);
  for i := 0 to MaxDelays -1 do fEBuffer[i] := @PByteArray(fEchoBuffer)^[i * (fEchoBufferCount div 10)];
end;
(*** IDCEchoDelay *************************************************************)
function TDCEchoDelay.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aEnabled := fEnabled;
end;

function TDCEchoDelay.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fEnabled := aEnabled;
end;

function TDCEchoDelay.get_KillMain(out aKillMain: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aKillMain := fKillMain;
end;

function TDCEchoDelay.set_KillMain(aKillMain: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fKillMain := aKillMain;
end;

function TDCEchoDelay.get_NumDelays(out aNumDelays: Byte): HRESULT; stdcall;
begin
  Result := S_OK;
  aNumDelays := fNumDelays;
end;

function TDCEchoDelay.set_NumDelays(aNumDelays: Byte): HRESULT; stdcall;
begin
  Result := S_OK;
  fNumDelays := aNumDelays;
end;

function TDCEchoDelay.get_Highpass(out aHighpass: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aHighpass := fDoHighPass;
end;

function TDCEchoDelay.set_Highpass(aHighpass: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fDoHighPass := aHighpass;
end;

function TDCEchoDelay.get_DelayAmp(out aDelayAmp: WORD): HRESULT; stdcall;
begin
  Result := S_OK;
  aDelayAmp := fDelayAmp;
end;

function TDCEchoDelay.set_DelayAmp(aDelayAmp: WORD): HRESULT; stdcall;
begin
  Result := S_OK;
  fDelayAmp := aDelayAmp;
end;

function TDCEchoDelay.get_Delay(out aDelay: WORD): HRESULT; stdcall;
begin
  Result := S_OK;
  aDelay := fDelaySize;
end;

function TDCEchoDelay.set_Delay(aDelay: WORD): HRESULT; stdcall;
begin
  Result := S_OK;
  fDelaySize := aDelay;
end;

end.
