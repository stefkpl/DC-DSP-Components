
    (*********************************************************************
     *  DirectShowUtils.pas                                              *
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

unit DirectShowUtils;

interface

uses
  SysUtils, Math, DSPack, DirectShow9;

  { Formats a String of an integer Time Value of MilliSeconds. }
  function CalcTimeMStoString(MilliSeconds : integer) : String;

  { DirectShow uses logarithmic Values to Set Amplification and Pan.
    This functions converts them to linear Values. This makes the
    Sliders smoother.
    Volume Slider must be in Range of 0..10000.
    Pan Slider must be in Range of -10000..10000. }
  function GetBasicAudioVolume(Value : integer) : integer;
  function SetBasicAudioVolume(Value : integer) : integer;
  function GetBasicAudioPan(Value : integer) : integer;
  function SetBasicAudioPan(Value : integer) : integer;

  { Checks if the DSPack VideoWindow is connected. }
  function IsVideoWindowConnected(VideoWindow : TVideoWindow)  : Boolean;

implementation

function CalcTimeMStoString(MilliSeconds : integer) : String;
var
  Hours,
  Minutes,
  Seconds : integer;
begin
  Hours := MilliSeconds div 3600000;
  MilliSeconds := MilliSeconds - (Hours * 3600000);
  Minutes := MilliSeconds div 60000;
  MilliSeconds := MilliSeconds - (Minutes * 60000);
  Seconds := MilliSeconds div 1000;
  MilliSeconds := MilliSeconds - (Seconds * 1000);
  Result := Format('%.2d:%.2d:%.2d:%.3d',[Hours,Minutes,Seconds,MilliSeconds]);
end;

function GetBasicAudioVolume(Value : integer) : integer;
begin
  Result := Round(Power(10,Value / 2500) * 10001 - 1);
end;

function SetBasicAudioVolume(Value : integer) : integer;
begin
  Result := Round(Log10((Value+1) / 10000) * 2500);
end;

function GetBasicAudioPan(Value : integer) : integer;
var
  f : ShortInt;
begin
  if Value > 0 then f := 1 else f := -1;
  Result := (10000 - (Round(Power(10,-Abs(Value) / 2500) * 10001 - 1))) * f;
end;

function SetBasicAudioPan(Value : integer) : integer;
var
  f : ShortInt;
begin
  if Value < 0 then f := 1 else f := -1;
  Result := Round(Log10(((10000 - Abs(Value))+1) / 10000) * 2500) * f;
end;

function IsVideoWindowConnected(VideoWindow : TVideoWindow)  : Boolean;
var
  pBF : IBaseFilter;
  EnumPins : IEnumPins;
  Pin : IPin;
  ToPin : IPin;
begin
  Result := False;
  pBF := (VideoWindow as IBaseFilter);
  if (pBF.EnumPins(EnumPins) = S_OK) and (EnumPins.Next(1,Pin,nil) = S_OK) then
  begin
     Pin.ConnectedTo(ToPin);
     Result := ToPin <> nil;
  end;
  EnumPins := nil;
  Pin := nil;
  ToPin := nil;
  pBF := nil;
end;

end.
