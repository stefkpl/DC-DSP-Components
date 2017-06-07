
    (*********************************************************************
     *  dmoUtils.pas                                                     *
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
  @abstract(Contains Utilitys and Functions to work with MS DMO Filters.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dmoUtils;

interface

uses
  Windows, ActiveX, DirectShow9, dmoConst;

  { Used internal by the DMO Filters to show the Property Page of the DMO. }
  function ShowDMOFilterPropertyPage(Parent: Cardinal; Filter: IMediaObject; Name : String): HRESULT;

implementation

function ShowDMOFilterPropertyPage(Parent: Cardinal; Filter: IMediaObject; Name : String): HRESULT;
var
  SpecifyPropertyPages : ISpecifyPropertyPages;
  CAGUID : TCAGUID;
begin
  Result := S_FALSE;
  if Filter = nil then Exit;
  Result := Filter.QueryInterface(IID_ISpecifyPropertyPages, SpecifyPropertyPages);
  if Result <> S_OK then Exit;
  Result := SpecifyPropertyPages.GetPages(CAGUID);
  SpecifyPropertyPages := nil;
  if Result <> S_OK then Exit;
  Result := OleCreatePropertyFrame(Parent, 0, 0, StringToOleStr(Name), 1, @Filter, CAGUID.cElems, CAGUID.pElems, 0, 0, nil);
end;

end.
