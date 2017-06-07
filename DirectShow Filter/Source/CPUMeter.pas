
    (*********************************************************************
     *  CPUMeter.pas                                                     *
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

unit CPUMeter;

interface

uses
  Windows, Classes;

type
  TCPUMeter = class
  private
    fThread : THandle;
    fThreadTime : Int64;
    fSystemTimeBegin : Int64;
    fThreadTimeBegin : Int64;
    fThreadTimeTotal : Int64;
  public
    constructor Create;
    procedure Reset;
    procedure StartMeasure;
    procedure StopMeasure;
    function Usage : Double;
    function Time : Int64;
  end;

implementation

constructor TCPUMeter.Create;
begin
  fThread := 0;
  Reset;
end;

procedure TCPUMeter.StartMeasure;
var
  lCreationTime : TFileTime;
  lExitTime : TFileTime;
  lKernelTime : TFileTime;
  lUserTime : TFileTime;
begin
  if (fThread <> 0) then StopMeasure;

  DuplicateHandle(GetCurrentProcess(), GetCurrentThread(), GetCurrentProcess(), @fThread, 0, true, DUPLICATE_SAME_ACCESS);

  if GetThreadTimes(fThread,lCreationTime,lExitTime,lKernelTime,lUserTime) then
    fThreadTimeBegin := int64(lKernelTime) + int64(lUserTime);
end;

procedure TCPUMeter.StopMeasure;
var
  lCreationTime : TFileTime;
  lExitTime : TFileTime;
  lKernelTime : TFileTime;
  lUserTime : TFileTime;
begin
  if GetThreadTimes(fThread,lCreationTime,lExitTime,lKernelTime,lUserTime) then
  begin
    fThreadTime       := fThreadTime + (int64(lKernelTime) + int64(lUserTime) - int64(fThreadTimeBegin));
    fThreadTimeTotal := fThreadTimeTotal + (int64(lKernelTime) + int64(lUserTime) - int64(fThreadTimeBegin));
  end;

  CloseHandle(fThread);

  fThread := 0;
  fThreadTimeBegin := 0;
end;

procedure TCPUMeter.Reset;
type
  PDWORDArray = ^TDWORDArray;
  TDWORDArray = array[0..1] of DWORD;
var
  lCreationTime : TFileTime;
  lExitTime : TFileTime;
  lKernelTime : TFileTime;
  lUserTime : TFileTime;
  lSysTime : TSystemTime;
  lFileTime : TFileTime;
begin
  GetSystemTime(lSysTime);
  if not SystemTimeToFileTime(lSysTime, lFileTime) then Exit;
  PDWORDArray(@fSystemTimeBegin)^[0] := lFileTime.dwLowDateTime;
  PDWORDArray(@fSystemTimeBegin)^[1] := lFileTime.dwHighDateTime;

  fThreadTime := 0;
  fThreadTimeBegin := 0;
  fThreadTimeTotal := 0;

  if (fThread <> 0) and
     GetThreadTimes(fThread,lCreationTime,lExitTime, lKernelTime, lUserTime)
     then fThreadTimeBegin := int64(lKernelTime) + int64(lUserTime);
end;

function TCPUMeter.Usage : Double;
var
  lCreationTime : TFileTime;
  lExitTime : TFileTime;
  lKernelTime : TFileTime;
  lUserTime : TFileTime;
  lSystemTimeEnd : TFileTime;
  lSysTime : TSystemTime;
begin
  GetSystemTime(lSysTime);
  SystemTimeToFileTime(lSysTime, lSystemTimeEnd);


  if (fThread <> 0) then
  begin
    if GetThreadTimes(fThread,lCreationTime,lExitTime,lKernelTime,lUserTime) then
    begin
      fThreadTime       := fThreadTime + (int64(lKernelTime) + int64(lUserTime) - int64(fThreadTimeBegin));
      fThreadTimeTotal := fThreadTimeTotal + (int64(lKernelTime) + int64(lUserTime) - int64(fThreadTimeBegin));
      fThreadTimeBegin  := int64(lKernelTime) + int64(lUserTime);
    end else
    begin
      fThreadTime := 0;
      fThreadTimeBegin := 0;
    end;
  end;

  if (Bool(int64(fSystemTimeBegin) - int64(lSystemTimeEnd)))
    then result := fThreadTime / (int64(lSystemTimeEnd) - int64(fSystemTimeBegin))
    else result := 0;

  fThreadTime := 0;
  fSystemTimeBegin := int64(lSystemTimeEnd);
end;

function TCPUMeter.time : int64;
var
  lCreationTime : TFileTime;
  lExitTime : TFileTime;
  lKernelTime : TFileTime;
  lUserTime : TFileTime;
begin
  if (fThread <> 0) then
  begin
    if GetThreadTimes(fThread,lCreationTime,lExitTime,lKernelTime,lUserTime) then
    begin
      fThreadTime       := fThreadTime + (int64(lKernelTime) + int64(lUserTime) - int64(fThreadTimeBegin));
      fThreadTimeTotal := fThreadTimeTotal + (int64(lKernelTime) + int64(lUserTime) - int64(fThreadTimeBegin));
      fThreadTimeBegin := int64(lKernelTime) + int64(lUserTime);
    end else
    begin
      fThreadTime := 0;
      fThreadTimeBegin := 0;
    end;
  end;

  Result := fThreadTimeTotal;
end;

end.
