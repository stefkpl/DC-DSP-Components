
    (*********************************************************************
     *  formPropertyPage.pas                                             *
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

unit formPropertyPage;

interface

{$I Compiler.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActiveX, DirectShow9, DSUtil, ComCtrls, StdCtrls, ExtCtrls, Buttons;

type
  TfrmPropertyPage = class(TForm, IPropertyPageSite)
    pnlControls: TPanel;
    btClose: TButton;
    pgPages: TPageControl;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  private
    fRefCount: Longint;
    fNumPages: integer;
    fPages: TList;
    fPanels: TList;
    fOwner: Pointer;
  protected
    // IPropertyPageSite
    function OnStatusChange(flags: Longint): HResult; stdcall;
    function GetLocaleID(out localeID: TLCID): HResult; stdcall;
    function GetPageContainer(out unk: IUnknown): HResult; stdcall;
    function TranslateAccelerator(msg: PMsg): HResult; stdcall;
    constructor Create(AOwner: Pointer); reintroduce;
    function CreatePage(Filter: IBaseFilter; aCaption: WideString): Boolean;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    class function CreateInstance(AOwner: Pointer; Filter: IBaseFilter; aCaption: WideString; out Instance: TfrmPropertyPage): boolean;
  end;

implementation

uses
  TrayIcon;

{$R *.dfm}

class function TfrmPropertyPage.CreateInstance(AOwner: Pointer; Filter: IBaseFilter; aCaption: WideString; out Instance: TfrmPropertyPage): boolean;
begin
  Instance := TfrmPropertyPage.Create(AOwner);
  if not Instance.CreatePage(Filter, aCaption) then
  begin
    Instance.Free;
    Instance := nil;
    Result := False;
    Exit;
  end;

  Result := True;
end;

constructor TfrmPropertyPage.Create(AOwner: Pointer);
begin
  inherited Create(nil);
  fOwner := AOwner;
end;

procedure TfrmPropertyPage.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

function TfrmPropertyPage.CreatePage(Filter: IBaseFilter; aCaption: WideString): Boolean;
var
  i: integer;
  rect: TRect;
  Info: TPropPageInfo;
  sheet: TTabSheet;
  mWidth, mHeight: integer;
  CAGUID: TCAGUID;
  Prop: ISpecifyPropertyPages;
  Page: IPropertyPage;
  Panel: TPanel;
begin
  Result := False;
  CAGUID.cElems := 0;

  if not Assigned(Filter) or (Filter.QueryInterface(IID_ISpecifyPropertyPages,Prop) <> S_OK) or
     not Assigned(Prop) then Exit;
  if Prop.GetPages(CAGUID) <> S_OK then
  begin
    Prop := nil;
    Exit;
  end;
  if CAGUID.cElems <= 0  then
  begin
    Exit;
  end;
  mWidth := 0;
  mHeight := 0;

  fNumPages := CAGUID.cElems;

  for i := 0 to CAGUID.cElems -1 do
  begin
    CoCreateInstance(CAGUID.pElems[i], nil, CLSCTX_INPROC_SERVER, IPropertyPage, Page);
    Page._AddRef;
    fPages.Add(Pointer(Page));

    Page.GetPageInfo(Info);
    Page.SetObjects(1,@Filter);
    Page.SetPageSite(Self as IPropertyPageSite);

    sheet := TTabSheet.Create(pgPages);
    sheet.PageControl := pgPages;
    sheet.Name := 'prop' + inttostr(i);
    sheet.Caption := Info.pszTitle;

    Panel := TPanel.CreateParented(sheet.Handle);
    Panel.Parent := sheet;
    Panel.Name := 'proppanel' + inttostr(i);
    Panel.BorderStyle := bsNone;
    Panel.Caption := '';
    Panel.BevelOuter := bvNone;
    Panel.BevelInner := bvNone;
    Panel.Left := 0;
    Panel.Top := 0;
    Panel.ParentBackground := False;
    fPanels.Add(Panel);

    rect.Left := 0;
    rect.Top := 0;
    rect.Right := info.size.cx;
    rect.Bottom := info.size.cy;
    rect.TopLeft.X := rect.Left;
    rect.TopLeft.Y := rect.Top;
    rect.BottomRight.X := rect.Right;
    rect.BottomRight.Y := rect.Bottom;

    if mWidth < info.size.cx then mWidth := info.size.cx;
    if mHeight < info.size.cy then mHeight := info.size.cy;
    Page.Activate(Panel.Handle, rect, False);
    Page.Show(SW_SHOWNORMAL);
    Page := nil;
  end;

  pgPages.TabIndex := 0;

  pgPages.Width := mWidth + (pgPages.Width - pgPages.Pages[0].Width) + pgPages.Pages[0].Left div 2;
  pgPages.Height := mHeight + (pgPages.Height - pgPages.Pages[0].Height) + pgPages.Pages[0].Left div 2;

  for i := 0 to CAGUID.cElems -1 do
  begin
    TPanel(fPanels.Items[i]).Width := mWidth;
    TPanel(fPanels.Items[i]).Height := mHeight;
  end;

  ClientWidth := pgPages.Width;
  ClientHeight := pgPages.Height + pnlControls.Height;

  Prop := nil;
  if (CAGUID.cElems > 0) and Assigned(CAGUID.pElems)
    then CoTaskMemFree(CAGUID.pElems);

  Result := True;
end;

function TfrmPropertyPage.OnStatusChange(flags: Longint): HResult;
begin
  Result := S_OK;
end;

function TfrmPropertyPage.GetLocaleID(out localeID: TLCID): HResult;
begin
  if not Assigned(@localeID) then
  begin
    Result := E_POINTER;
  end else
  begin
    LocaleID := GetUserDefaultLCID;
    Result := S_OK;
  end;
end;

function TfrmPropertyPage.GetPageContainer(out unk: IUnknown): HResult;
begin
  Result := E_NOTIMPL;
end;

function TfrmPropertyPage.TranslateAccelerator(msg: PMsg): HResult;
begin
  Result := E_NOTIMPL;
end;

procedure TfrmPropertyPage.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: integer;
  Page: IPropertyPage;
begin
  for i := 0 to fNumPages -1 do
  begin
    Page := IPropertyPage(fPages.Items[i]);
    Page.Show(SW_HIDE);
    Page.Deactivate;
    Page.SetObjects(0,nil);
    Page._Release;
    Page := nil;
    TPanel(fPanels.Items[i]).Destroy;
  end;

  fPages.Clear;
  FreeAndNil(fPages);
  fPanels.Clear;
  FreeAndNil(fPanels);

  TTrayIcon(fOwner).FormClosed;
  Action := caFree;
end;

procedure TfrmPropertyPage.FormCreate(Sender: TObject);
begin
  fNumPages := 0;
  fRefCount := 0;
  pnlControls.Align := alBottom;
  fPages := TList.Create;
  fPanels := TList.Create;
  Icon.Handle := LoadIcon(hInstance,MAKEINTRESOURCE(102));
end;

procedure TfrmPropertyPage.btCloseClick(Sender: TObject);
begin
  Close;
end;

end.
