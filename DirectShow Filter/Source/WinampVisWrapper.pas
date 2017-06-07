
    (*********************************************************************
     *  WinampVisWrapper.pas                                             *
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

unit WinampVisWrapper;

interface

uses
  Windows, DCDSPTypes, SysUtils, Utils, Classes, Messages, dspConst, dspUtils,
  dspFastFourier, waConst;

  procedure SetVisualPlugin(instance : integer;Path : String; Idx : integer);
  procedure ProcessVisualData(Buffer : Pointer; Size,Frequency,Bits,Channels,Interval : integer; Float : Boolean);
  procedure StopVisualPlugin(instance : integer);

implementation

var
  fBuffer : Pointer;
  fSize,
  fFrequency,
  fBits,
  fChannels,
  fInterval : integer;
  fFloat : Boolean;
  fVisHeader : PWinampVisHeader;
  fCurrentMod : PWinampVisModule;
  VisualFilename : String;
  VisualIndex : integer;
  VisualEvent : integer = 7;
  hasWave : Boolean;
  hasSpec : Boolean;
  inst : integer;
  tBuf : array[0..10000] of integer;
  fFFT : TDCFFT;
  ht : THANDLE;
  fDLLHandle2 : Cardinal;
  thid : THandle;

procedure ProcessWaveBuffer(Buffer : Pointer; Length : integer; WaveChannels, PluginChannels : Byte; Frequency : integer; Bits : Byte; NumBands : integer; PluginBuffer : Pointer);
var
  pBuf8  : PByteArray;
  pBuf16 : PSmallIntArray;
  pBuf24 : PInteger24Array;
  pBuf32 : PFloatArray;
  pBuf32i : PIntegerArray;
  NumSamples : integer;
  SamplesPerChannel : integer;
  i,c,c2 : integer;
  tmp : integer;
  tmp2 : integer;
  Res : PVisBuffer;
begin
  Res := PVisBuffer(PluginBuffer);
  NumSamples := Length div (Bits div 8);
  SamplesPerChannel := Numsamples div WaveChannels;

  if PluginChannels = 1 then
  begin
    case Bits of
      8:
      begin
        pBuf8 := PByteArray(Buffer);
        for i := 0 to (SamplesPerChannel -1) do
        begin
          tmp := 0;
          for c := 0 to (WaveChannels -1) do tmp := tmp + (pBuf8^[(i * WaveChannels) + c] - 128);
          tmp := tmp div WaveChannels;
          tBuf[i] := tmp;
        end;
      end;
      16:
      begin
        pBuf16 := PSmallIntArray(Buffer);
        for i := 0 to (SamplesPerChannel -1) do
        begin
          tmp := 0;
          for c := 0 to (WaveChannels -1) do tmp := tmp + (pBuf16^[(i * WaveChannels) + c] div 256);
          tmp := tmp div WaveChannels;
          tBuf[i] := tmp;
        end;
      end;
      24:
      begin
        pBuf24 := PInteger24Array(Buffer);
        for i := 0 to (SamplesPerChannel -1) do
        begin
          tmp := 0;
          for c := 0 to (WaveChannels -1) do tmp := tmp + (Cvt24BitTo32(pBuf24^[(i * WaveChannels) + c]) div 32768);
          tmp := tmp div WaveChannels;
          tBuf[i] := tmp;
        end;
      end;
      32:
      begin
        if fFloat then
        begin
          pBuf32 := PFloatArray(Buffer);
          for i := 0 to (SamplesPerChannel -1) do
          begin
            tmp := 0;
            for c := 0 to (WaveChannels -1) do tmp := tmp + Round(pBuf32^[(i * WaveChannels) + c] * 256);
            tmp := tmp div WaveChannels;
            tBuf[i] := tmp;
          end;
        end else
        begin
          pBuf32i := PIntegerArray(Buffer);
          for i := 0 to (SamplesPerChannel -1) do
          begin
            tmp := 0;
            for c := 0 to (WaveChannels -1) do tmp := tmp + (pBuf32i^[(i * WaveChannels) + c] div 8388352);
            tmp := tmp div WaveChannels;
            tBuf[i] := tmp;
          end;
        end;
      end;
    end;
    tmp2 := SamplesPerChannel div NumBands;
    if tmp2 < 1 then tmp2 := 1;
    for i := 0 to (NumBands -1) do
    begin
      tmp := 0;
      for c := 0 to tmp2 -1 do
      begin
        tmp := tmp + tBuf[(i * tmp2) + c];
      end;
      tmp := tmp div tmp2;
      if Res <> nil then Res[0][i] := Clip_8(tmp);
    end;
  end else
  begin // PluginChannels > 1 -- WaveChannels = 1
    for c2 := 0 to PluginChannels -1 do
    begin
      if (WaveChannels = 1) then
      begin
        case Bits of
          8:
          begin
            pBuf8 := PByteArray(Buffer);
            for i := 0 to (SamplesPerChannel -1) do
            begin
              tmp := 0;
              for c := 0 to (WaveChannels -1) do tmp := tmp + (pBuf8^[(i * WaveChannels) + c] - 128);
              tmp := tmp div WaveChannels;
              tBuf[i] := tmp;
            end;
          end;
          16:
          begin
            pBuf16 := PSmallIntArray(Buffer);
            for i := 0 to (SamplesPerChannel -1) do
            begin
              tmp := 0;
              for c := 0 to (WaveChannels -1) do tmp := tmp + (pBuf16^[(i * WaveChannels) + c] div 256);
              tmp := tmp div WaveChannels;
              tBuf[i] := tmp;
            end;
          end;
          24:
          begin
            pBuf24 := PInteger24Array(Buffer);
            for i := 0 to (SamplesPerChannel -1) do
            begin
              tmp := 0;
              for c := 0 to (WaveChannels -1) do tmp := tmp + (Cvt24BitTo32(pBuf24^[(i * WaveChannels) + c]) div 32768);
              tmp := tmp div WaveChannels;
              tBuf[i] := tmp;
            end;
          end;
          32:
          begin
            if fFloat then
            begin
              pBuf32 := PFloatArray(Buffer);
              for i := 0 to (SamplesPerChannel -1) do
              begin
                tmp := 0;
                for c := 0 to (WaveChannels -1) do tmp := tmp + Round(pBuf32^[(i * WaveChannels) + c] * 256);
                tmp := tmp div WaveChannels;
                tBuf[i] := tmp;
              end;
            end else
            begin
              pBuf32i := PIntegerArray(Buffer);
              for i := 0 to (SamplesPerChannel -1) do
              begin
                tmp := 0;
                for c := 0 to (WaveChannels -1) do tmp := tmp + (pBuf32i^[(i * WaveChannels) + c] div 8388352);
                tmp := tmp div WaveChannels;
                tBuf[i] := tmp;
              end;
            end;
          end;
        end;
        tmp2 := SamplesPerChannel div NumBands;
        if tmp2 < 1 then tmp2 := 1;
        for i := 0 to (NumBands -1) do
        begin
          tmp := 0;
          for c := 0 to tmp2 -1 do
          begin
            tmp := tmp + tBuf[(i * tmp2) + c];
          end;
          tmp := tmp div tmp2;
          if Res <> nil then Res[0][i] := Clip_8(tmp);
          if Res <> nil then Res[1][i] := Clip_8(tmp);
        end;
        break;
      end else
      begin // PluginChannels > 1 -- WaveChannels > 1
        case Bits of
          8:
          begin
            pBuf8 := PByteArray(Buffer);
            for i := 0 to (SamplesPerChannel -1) do
            begin
              tmp := (pBuf8^[(i * WaveChannels) + c2] - 128);
              tBuf[i] := tmp;
            end;
          end;
          16:
          begin
            pBuf16 := PSmallIntArray(Buffer);
            for i := 0 to (SamplesPerChannel -1) do
            begin
              tmp := (pBuf16^[(i * WaveChannels) + c2] div 256);
              tBuf[i] := tmp;
            end;
          end;
          24:
          begin
            pBuf24 := PInteger24Array(Buffer);
            for i := 0 to (SamplesPerChannel -1) do
            begin
              tmp := (Cvt24BitTo32(pBuf24^[(i * WaveChannels) + c2]) div 32768);
              tBuf[i] := tmp;
            end;
          end;
          32:
          begin
            if fFloat then
            begin
              pBuf32 := PFloatArray(Buffer);
              for i := 0 to (SamplesPerChannel -1) do
              begin
                tmp := Round(pBuf32^[(i * WaveChannels) + c2] * 256);
                tBuf[i] := tmp;
              end;
            end else
            begin
              pBuf32i := PIntegerArray(Buffer);
              for i := 0 to (SamplesPerChannel -1) do
              begin
                tmp := (pBuf32i^[(i * WaveChannels) + c2] div 8388352);
                tBuf[i] := tmp;
              end;
            end;
          end;
        end;
        tmp2 := SamplesPerChannel div NumBands;
        if tmp2 < 1 then tmp2 := 1;
        for i := 0 to (NumBands -1) do
        begin
          tmp := 0;
          for c := 0 to tmp2 -1 do
          begin
            tmp := tmp + tBuf[(i * tmp2) + c];
          end;
          tmp := tmp div tmp2;
          if Res <> nil then Res[c2][i] := Clip_8(tmp);
        end;
      end;
    end;
  end;
end;

procedure ProcessFFTBuffer(Buffer : Pointer; Length : integer; WaveChannels, PluginChannels : Byte; Frequency : integer; Bits : Byte; NumBands : integer; PluginBuffer : Pointer);
var
  pBuf8  : PByteArray;
  pBuf16 : PSmallIntArray;
  pBuf24 : PInteger24Array;
  pBuf32 : PFloatArray;
  pBuf32i : PIntegerArray;
  NumSamples : integer;
  SamplesPerChannel : integer;
  i,c,c2 : integer;
  tmp : integer;
  tmp2 : integer;
  Res : PVisBuffer;
begin
  Res := PVisBuffer(PluginBuffer);
  NumSamples := Length div (Bits div 8);
  SamplesPerChannel := Numsamples div WaveChannels;
  fFFT.Flush;

  if PluginChannels = 1 then
  begin
    case Bits of
      8:
      begin
        pBuf8 := PByteArray(Buffer);
        for i := 0 to (SamplesPerChannel -1) do
        begin
          tmp := 0;
          for c := 0 to (WaveChannels -1) do tmp := tmp + (pBuf8^[(i * WaveChannels) + c] - 128);
          tmp := tmp div WaveChannels;
          tBuf[i] := tmp;
        end;
      end;
      16:
      begin
        pBuf16 := PSmallIntArray(Buffer);
        for i := 0 to (SamplesPerChannel -1) do
        begin
          tmp := 0;
          for c := 0 to (WaveChannels -1) do tmp := tmp + (pBuf16^[(i * WaveChannels) + c] div 256);
          tmp := tmp div WaveChannels;
          tBuf[i] := tmp;
        end;
      end;
      24:
      begin
        pBuf24 := PInteger24Array(Buffer);
        for i := 0 to (SamplesPerChannel -1) do
        begin
          tmp := 0;
          for c := 0 to (WaveChannels -1) do tmp := tmp + (Cvt24BitTo32(pBuf24^[(i * WaveChannels) + c]) div 32768);
          tmp := tmp div WaveChannels;
          tBuf[i] := tmp;
        end;
      end;
      32:
      begin
        if fFloat then
        begin
          pBuf32 := PFloatArray(Buffer);
          for i := 0 to (SamplesPerChannel -1) do
          begin
            tmp := 0;
            for c := 0 to (WaveChannels -1) do tmp := tmp + Round(pBuf32^[(i * WaveChannels) + c] * 256);
            tmp := tmp div WaveChannels;
            tBuf[i] := tmp;
          end;
        end else
        begin
          pBuf32i := PIntegerArray(Buffer);
          for i := 0 to (SamplesPerChannel -1) do
          begin
            tmp := 0;
            for c := 0 to (WaveChannels -1) do tmp := tmp + (pBuf32i^[(i * WaveChannels) + c] div 8388352);
            tmp := tmp div WaveChannels;
            tBuf[i] := tmp;
          end;
        end;
      end;
    end;
    tmp2 := SamplesPerChannel div NumBands;
    if tmp2 < 1 then tmp2 := 1;
    for i := 0 to (NumBands -1) do
    begin
      tmp := 0;
      for c := 0 to tmp2 -1 do
      begin
        tmp := tmp + tBuf[(i * tmp2) + c];
      end;
      tmp := tmp div tmp2;
      fFFT.Complex[i].re := tmp;
    end;
    fFFT.FFT;
    for i := 0 to NumBands -1 do
    begin
      if Res <> nil then Res[0][i] := Clip_8(Round(FFTSum(fFFT.Complex[i].re,fFFT.Complex[i].im) / 20));
    end;
  end else
  begin // PluginChannels > 1 -- WaveChannels = 1
    for c2 := 0 to PluginChannels -1 do
    begin
      if (WaveChannels = 1) then
      begin
        case Bits of
          8:
          begin
            pBuf8 := PByteArray(Buffer);
            for i := 0 to (SamplesPerChannel -1) do
            begin
              tmp := 0;
              for c := 0 to (WaveChannels -1) do tmp := tmp + (pBuf8^[(i * WaveChannels) + c] - 128);
              tmp := tmp div WaveChannels;
              tBuf[i] := tmp;
            end;
          end;
          16:
          begin
            pBuf16 := PSmallIntArray(Buffer);
            for i := 0 to (SamplesPerChannel -1) do
            begin
              tmp := 0;
              for c := 0 to (WaveChannels -1) do tmp := tmp + (pBuf16^[(i * WaveChannels) + c] div 256);
              tmp := tmp div WaveChannels;
              tBuf[i] := tmp;
            end;
          end;
          24:
          begin
            pBuf24 := PInteger24Array(Buffer);
            for i := 0 to (SamplesPerChannel -1) do
            begin
              tmp := 0;
              for c := 0 to (WaveChannels -1) do tmp := tmp + (Cvt24BitTo32(pBuf24^[(i * WaveChannels) + c]) div 32768);
              tmp := tmp div WaveChannels;
              tBuf[i] := tmp;
            end;
          end;
          32:
          begin
            if fFloat then
            begin
              pBuf32 := PFloatArray(Buffer);
              for i := 0 to (SamplesPerChannel -1) do
              begin
                tmp := 0;
                for c := 0 to (WaveChannels -1) do tmp := tmp + Round(pBuf32^[(i * WaveChannels) + c] * 256);
                tmp := tmp div WaveChannels;
                tBuf[i] := tmp;
              end;
            end else
            begin
              pBuf32i := PIntegerArray(Buffer);
              for i := 0 to (SamplesPerChannel -1) do
              begin
                tmp := 0;
                for c := 0 to (WaveChannels -1) do tmp := tmp + (pBuf32i^[(i * WaveChannels) + c] div 8388352);
                tmp := tmp div WaveChannels;
                tBuf[i] := tmp;
              end;
            end;
          end;
        end;
        tmp2 := SamplesPerChannel div NumBands;
        if tmp2 < 1 then tmp2 := 1;
        for i := 0 to (NumBands -1) do
        begin
          tmp := 0;
          for c := 0 to tmp2 -1 do
          begin
            tmp := tmp + tBuf[(i * tmp2) + c];
          end;
          tmp := tmp div tmp2;
          fFFT.Complex[i].re := tmp;
        end;
        fFFT.FFT;
        for i := 0 to NumBands -1 do
        begin
          if Res <> nil then Res[0][i] := Clip_8(Round(FFTSum(fFFT.Complex[i].re,fFFT.Complex[i].im) / 20));
          if Res <> nil then Res[1][i] := Res[0][i];
        end;
        break;
      end else
      begin // PluginChannels > 1 -- WaveChannels > 1
        if c2 > 0 then fFFT.Flush;
        case Bits of
          8:
          begin
            pBuf8 := PByteArray(Buffer);
            for i := 0 to (SamplesPerChannel -1) do
            begin
              tmp := (pBuf8^[(i * WaveChannels) + c2] - 128);
              tBuf[i] := tmp;
            end;
          end;
          16:
          begin
            pBuf16 := PSmallIntArray(Buffer);
            for i := 0 to (SamplesPerChannel -1) do
            begin
             tmp := (pBuf16^[(i * WaveChannels) + c2] div 256);
             tBuf[i] := tmp;
            end;
          end;
          24:
          begin
            pBuf24 := PInteger24Array(Buffer);
            for i := 0 to (SamplesPerChannel -1) do
            begin
              tmp := Cvt24BitTo32(pBuf24^[(i * WaveChannels) + c2]) div 32768;
              tBuf[i] := tmp;
            end;
          end;
          32:
          begin
            if fFloat then
            begin
              pBuf32 := PFloatArray(Buffer);
              for i := 0 to (SamplesPerChannel -1) do
              begin
               tmp := Round(pBuf32^[(i * WaveChannels) + c2] * 256);
               tBuf[i] := tmp;
              end;
            end else
            begin
              pBuf32i := PIntegerArray(Buffer);
              for i := 0 to (SamplesPerChannel -1) do
              begin
                tmp := (pBuf32i^[(i * WaveChannels) + c2] div 8388352);
                tBuf[i] := tmp;
              end;
            end;
          end;
        end;
        tmp2 := SamplesPerChannel div NumBands;
        if tmp2 < 1 then tmp2 := 1;
        for i := 0 to (NumBands -1) do
        begin
          tmp := 0;
          for c := 0 to tmp2 -1 do
          begin
            tmp := tmp + tBuf[(i * tmp2) + c];
          end;
          tmp := tmp div tmp2;
          fFFT.Complex[i].re := tmp;
        end;
        fFFT.FFT;
        for i := 0 to NumBands -1 do
        begin
          if Res <> nil then Res[c2][i] := Clip_8(Round(FFTSum(fFFT.Complex[i].re,fFFT.Complex[i].im) / 20));
        end;
      end;
    end;
  end;
end;

function ThreadProc : DWORD; stdcall;
var
  fGetModule : function : Pointer; cdecl;
  Msg  : TMsg;
begin
  PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE);
  while True do
  begin
    case VisualEvent of
      1:
      begin
      try
        FreeLibrary(fDLLHandle2);
        fDLLHandle2 := LoadLibrary(PChar(VisualFilename));
        @fGetModule := GetProcAddress(fDLLHandle2,PChar('winampVisGetHeader'));
        fVisHeader := fGetModule;
        fCurrentMod := fVisHeader.getModule(VisualIndex);
        fCurrentMod.hDllInstance := fDLLHandle2;
        fCurrentMod.hwndParent := GetDesktopWindow;
        hasSpec := fCurrentMod.spectrumNch > 0;
        hasWave := fCurrentMod.waveformNch > 0;
        fCurrentMod.Init(fCurrentMod);
        VisualEvent := 0;
      except
      end;
      end;
      2:
      begin
        try
          if fCurrentMod <> nil then fCurrentMod.Quit(fCurrentMod);
          break;
        except
        end;
      end;
      3:
      begin
        if (fCurrentMod <> nil) then
        begin
          try
            fCurrentMod.sRate := fFrequency;
            fCurrentMod.nCh := fChannels;
            fCurrentMod.delayMs := fInterval;
            if hasWave then ProcessWaveBuffer(fBuffer,fSize,fChannels,fCurrentMod.waveformNch,fFrequency,fBits,576,@fCurrentMod.waveformData);
            if hasSpec then ProcessFFTBuffer(fBuffer,fSize,fChannels,fCurrentMod.spectrumNch,fFrequency,fBits,512,@fCurrentMod.spectrumData);
            fCurrentMod.Render(fCurrentMod);
          except
          end;
        end;
        VisualEvent := 0;
      end;
    end;

    if not PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
    begin
      WaitForSingleObject(ht,10);
    end else
    begin
      if msg.message = WM_QUIT then
      begin
      try
        msg.message := WM_NULL;
        if fCurrentMod <> nil then fCurrentMod.Quit(fCurrentMod);
        FreeLibrary(fDLLHandle2);
        break;
      except
      end;
      end;
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
  end;
  try
    if fCurrentMod <> nil then fCurrentMod.Quit(fCurrentMod);
    FreeLibrary(fDLLHandle2);
  except
  end;
  try
    FreeLibrary(fDLLHandle2);
  except
  end;
  VisualFilename := '';
  VisualEvent := 7;
  VisualIndex := 0;
  ht := 0;
  Result := 0;
  ExitThread(0);
end;

procedure SetVisualPlugin(Instance : integer; Path : String; Idx : integer);
begin
  if Inst = 0 then Inst := Instance;
  if (Instance = Inst) then
  begin
    PostThreadMessage(thid,WM_QUIT,0,0);
    TerminateThread(ht,1);
    VisualIndex := Idx;
    VisualFilename := Path;
    VisualEvent := 0;
    ht := CreateThread(nil,0,@ThreadProc,nil,0,thid);
    VisualEvent := 1;
  end;
end;

procedure StopVisualPlugin(instance : integer);
begin
  if (Inst = Instance) and (ht <> 0) then
  begin
    PostThreadMessage(thid,WM_QUIT,0,0);
    VisualFilename := '';
    VisualIndex := -1;
    VisualEvent := 0;
  end;
end;

procedure ProcessVisualData(Buffer : Pointer; Size,Frequency,Bits,Channels,Interval : integer; Float : Boolean);
begin
  fFloat := Float;
  fBuffer := Buffer;
  fSize := Size;
  fFrequency := Frequency;
  fBits := Bits;
  fChannels := Channels;
  fInterval := Interval;
  if VisualEvent <> 1 then VisualEvent := 3;
end;

initialization
  fFFT := TDCFFT.Create(nil);
  fFFT.FFTSize := fts1024;
  fFFT.ReOrder := True;

finalization
  fFFT.Free;

end.
