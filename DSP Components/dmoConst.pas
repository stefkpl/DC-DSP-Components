
    (*********************************************************************
     *  dmoConst.pas                                                     *
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
  @abstract(Contains some Default Values and Interfaces for MS DMO Filters.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dmoConst;

interface

uses
  Classes;

const
  {@exclude}
  WAVE_FORMAT_IEEE_FLOAT = $0003;
  {@exclude}
  WAVE_FORMAT_DOLBY_AC3_SPDIF = $0092;
  {@exclude}
  WAVE_FORMAT_DOLBY_AC3 = $2000;
  {@exclude}
  WAVE_FORMAT_DTS = $8;
  {@exclude}
  WAVE_FORMAT_DVD_DTS = $2001;
  {@exclude}
  IID_ISpecifyPropertyPages      : TGUID = '{B196B28B-BAB4-101A-B69C-00AA00341D07}';
  {@exclude}
  One_Second = 10000000;

type
  { Used by TDCDMOI3DL2Revverb to set and get the Preset. }
  TDCDMOI3DL2ReverbPreset = (
    Alley, Arena, Auditorium, Bathroom, CarpetedHallway, Cave, City, ConcertHall,
    Default, Forrest, Generic, Hallway, Hangar, LargeHall, LargeRoom, LivingRoom,
    MediumHall, MediumRoom, Mountains, PaddedCell, ParkingLot, Plane, Plate, Quarry,
    Room, SewerPipe, SmallRoom, StoneCorridor, StoneRoom, UnderWater, None
  );

implementation

end.
