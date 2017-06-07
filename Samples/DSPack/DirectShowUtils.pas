
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

end.
