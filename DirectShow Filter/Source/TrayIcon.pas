
    (*********************************************************************
     *  TrayIcon.pas                                                     *
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

unit TrayIcon;

interface

uses
  Windows, JvTrayIcon, Controls, Classes, formPropertyPage, Menus, Messages,
  SysUtils, Forms, Graphics, DirectShow9, DSUtil, ActiveX, Registry;

const
  UM_CLOSE_PROPPAGE = WM_USER + 1234;
  UM_SHOW_BALLOON = WM_USER + 1235;

type
  // own PopupList needed because of VCL's Threading shitness ...
  TMyPopupMenu = class(TPopupMenu)
  private
    FMyPopupList: TPopupList;
  public
    procedure Popup(X, Y: Integer); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TTrayIcon = class
  private
    fOwner: Pointer;
    fForm: TfrmPropertyPage;
    fTrayIcon: TJvTrayIcon;
    fPopup: TMyPopupMenu;
    procedure OnShowPropClick(Sender: TObject);
    procedure OnEnableAllFilterClick(Sender: TObject);
    procedure OnDisableAllFilterClick(Sender: TObject);
    procedure OnEnableAudioStreamClick(Sender: TObject);
    procedure OnEnablePresetClick(Sender: TObject);
    procedure OnShowPropertyPageClick(Sender: TObject);

    procedure OnClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function GetVisible: Boolean;
    procedure SetVisible(AVisible: Boolean);
    procedure BuildMenu;
    procedure ShowHideForm;
  public
    procedure CloseForm;
    procedure FormClosed;
    constructor Create(AOwner: Pointer);
    destructor Destroy; override;
    procedure ShowAudioStreams;
  published
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  TTrayReg = record
    TrayIcon: TTrayIcon;
    Owner: Pointer;
    Visible: Boolean;
    Thread: THandle;
    ThreadID: THandle;
  end;
  PTrayReg = ^TTrayReg;

  function TrayIconThreadProc(Tray: PTrayReg): DWORD; stdcall;

implementation

uses
  DSFilter;

function TrayIconThreadProc(Tray: PTrayReg): DWORD; stdcall;
var
  Msg  : TMsg;
begin
  Tray.TrayIcon := TTrayIcon.Create(Tray.Owner);
  if Tray.Visible then Tray.TrayIcon.Visible := True;

  CoInitialize(nil);

	while(GetMessage(msg, 0, 0, 0)) do
  begin
    if (msg.message = UM_CLOSE_PROPPAGE) then
    begin
      Tray.TrayIcon.CloseForm;
    end else
    if (msg.message = UM_SHOW_BALLOON) then
    begin
      Tray.TrayIcon.ShowAudioStreams;
    end;
		TranslateMessage(msg);
		DispatchMessage(msg);
	end;

  if Assigned(Tray.TrayIcon) then
  begin
    Tray.TrayIcon.Free;
    Tray.TrayIcon := nil;
  end;
  Tray.Thread := 0;
  Tray.ThreadID := 0;

  CoUninitialize;

  Result := 0;
end;

constructor TMyPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PopupList.Remove(Self);
  FMyPopupList := TPopupList.Create;
  FMyPopupList.Add(Self);
  Items.OnClick := DoPopup;
  AutoPopup := False;
end;

destructor TMyPopupMenu.Destroy;
begin
  inherited Destroy;
  FMyPopupList.Remove(Self);
  FMyPopupList.Free;
end;

procedure TMyPopupMenu.Popup(X, Y: Integer);
const
  Flags: array[Boolean, TPopupAlignment] of Word =
    ((TPM_LEFTALIGN, TPM_RIGHTALIGN, TPM_CENTERALIGN),
     (TPM_RIGHTALIGN, TPM_LEFTALIGN, TPM_CENTERALIGN));
  Buttons: array[TTrackButton] of Word = (TPM_RIGHTBUTTON, TPM_LEFTBUTTON);
var
  AFlags: Integer;
begin
  DoPopup(Self);
  AdjustBiDiBehavior;
  AFlags := Flags[UseRightToLeftAlignment, Alignment] or Buttons[TrackButton] or
    (Byte(MenuAnimation) shl 10);
  TrackPopupMenu(Items.Handle, AFlags, X, Y, 0 { reserved }, FMyPopupList.Window, nil);
end;

constructor TTrayIcon.Create(AOwner: Pointer);
begin
  inherited Create;
  fOwner := AOwner;

  fTrayIcon := TJvTrayIcon.Create(nil);
  fTrayIcon.Icon.Handle := LoadIcon(hInstance,MAKEINTRESOURCE(102));
  fTrayIcon.Hint := 'DC-DSP Filter';
  fTrayIcon.OnClick := OnClick;

  fPopup := TMyPopupMenu.Create(nil);
  fPopup.AutoPopup := False;
  fPopup.TrackButton := tbRightButton;
  fPopup.AutoHotkeys := maManual;
end;

destructor TTrayIcon.Destroy;
begin
  if Assigned(fForm) then
    fForm.Close;

  fTrayIcon.Free;
  fPopup.Items.Clear;
  fPopup.Free;
end;

function TTrayIcon.GetVisible: Boolean;
begin
  Result := fTrayIcon.Active;
end;

procedure TTrayIcon.SetVisible(AVisible: Boolean);
begin
  fTrayIcon.Active := AVisible;
end;

procedure TTrayIcon.FormClosed;
begin
  fForm := nil;
end;

procedure TTrayIcon.ShowHideForm;
begin
  if Assigned(fForm) then
  begin
    SetForegroundWindow(fForm.Handle);
//    fTrayIcon.BalloonHint( 'Test', 'Another Test', btInfo, 10);
  end else
  begin
    if not TfrmPropertyPage.CreateInstance(Pointer(Self), TDCDSPFilter(fOwner), 'DC-DSP Filter', fForm) then
    begin
      fForm.Free;
      fForm := nil;
    end else
    begin
      fForm.Show;
    end;
  end;

//  ShowFilterPropertyPage(0, TDCDSPFilter(FOwner) as IBaseFilter);
end;

procedure TTrayIcon.OnClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    ShowHideForm;
  end else
  if (Button = mbRight) then
  begin
    BuildMenu;
    SetForegroundWindow(fTrayIcon.Handle);
    fPopup.Popup(x,y);
    PostMessage(fTrayIcon.Handle, WM_NULL, 0, 0);
  end;
end;

procedure TTrayIcon.BuildMenu;
var
  Filter: TDCDSPFilter;
  cnt: Cardinal;
  pcnt: integer;
  i: integer;
  ppmt: PamMediaType;
  flags, lcid, group: DWORD;
  name: PWChar;
  obj, unk: IUnknown;
  pname: PChar;
  item, si: TMenuItem;
  curp: String;
  graph: IFilterGraph;
  Enum: IEnumfilters;
  filt: IBaseFilter;
  fi: TFilterInfo;
begin
  Pointer(Filter) := Pointer(FOwner);

  fPopup.Items.Clear;

  // Show Propertypage
  // -----------------
  // FilterGraph Filters ->  SubItems
  // -----------------
  // Audio Track 1
  // Audio Track 2
  // Audio Track 3
  // Audio Track 4
  // -----------------
  // Preset 1
  // Preset 2
  // -----------------
  // Enable All Filters
  // Disable All Filters

  // show Propertypage menu item
  fPopup.Items.Add(NewItem('Show Property Page', 0, false, true, OnShowPropClick, 0, 'showprop'));

  // Filters in Graph
  fPopup.Items.Add(NewLine);
  item := NewItem('Filtergraph', 0, false, true, nil, 0, 'filterprop');
  fPopup.Items.Add(item);
  graph := Filter.GetFilterGraph;
  i := 0;
  if (Assigned(graph)) then
  begin
    if Graph.EnumFilters(Enum) = S_Ok then
    begin
      while (Enum.Next(1, filt, nil) = S_OK) do
      begin
        filt.QueryFilterInfo(fi);
        fi.pGraph := nil;
        si := NewItem(fi.achName, 0, false, true, OnShowPropertyPageClick, 0, 'filterprop' + inttostr(i));
        si.Tag := i;
        item.Add(si);
        filt := nil;
        inc(i);
      end;
      Enum := nil;
    end;
    Graph := nil;
  end;

  if Filter.fEnableStreamSwitching and (Filter.Count(cnt) = S_OK) then
  begin
    if (Filter.fEnableStreamSwitchingInterface and (cnt <= 1)) then
    begin

    end else
    if (cnt > 0) then
    begin
      fPopup.Items.Add(NewLine);
      for i := 0 to cnt -1 do
      begin
         Filter.Info(i, ppmt, flags, lcid, group, name, obj, unk);
         item := NewItem(name, 0, flags and AMSTREAMSELECTINFO_ENABLED > 0, true, OnEnableAudioStreamClick, 0, 'enableaudiostr' + inttostr(i));
         item.Tag := i;
         fPopup.Items.Add(item);
         if Assigned(ppmt) then
         begin
           DeleteMediaType(ppmt);
           ppmt := nil;
         end;
         if Assigned(name) then
         begin
           CoTaskMemFree(name);
           name := nil;
         end;
         if Assigned(obj) then obj := nil;
         if Assigned(unk) then unk := nil;
         lcid := 0;
         group := 0;
         flags := 0;
      end;
    end;
  end;

  Filter.get_PresetCount(pcnt);
  if (pcnt > 0) then
  begin
    curp := '';
    with TRegistry.Create do
    begin
      Rootkey := HKEY_CURRENT_USER;
      if OpenKey('SOFTWARE\DSP-Worx\DC-DSP Filter',False) then
      begin
        if ValueExists('fLastPPPreset') then curp := ReadString('fLastPPPreset');
        CloseKey;
      end;
    end;
    fPopup.Items.Add(NewLine);
    for i := 0 to pcnt -1 do
    begin
      Filter.get_PresetName(i,pname);
      fPopup.Items.Add(NewItem(pname, 0, curp = pname, true, OnEnablePresetClick, 0, 'enablepreset' + inttostr(i)));
    end;
  end;

  //enable/disable all Filters
  fPopup.Items.Add(NewLine);
  fPopup.Items.Add(NewItem('Enable all Filter', 0, false, true, OnEnableAllFilterClick, 0, 'enableall'));
  fPopup.Items.Add(NewItem('Disable all Filter', 0, false, true, OnDisableAllFilterClick, 0, 'disableall'));

  Pointer(Filter) := nil;
end;

procedure TTrayIcon.OnShowPropClick(Sender: TObject);
begin
  ShowHideForm;
end;

procedure TTrayIcon.OnEnableAllFilterClick(Sender: TObject);
var
  Filter: TDCDSPFilter;
  cnt: integer;
  i: integer;
begin
  Pointer(Filter) := Pointer(FOwner);
  Filter.get_FilterCount(cnt);
  for i := 0 to cnt -1 do
    Filter.set_EnableFilter(i, true);
end;

procedure TTrayIcon.OnDisableAllFilterClick(Sender: TObject);
var
  Filter: TDCDSPFilter;
  cnt: integer;
  i: integer;
begin
  Pointer(Filter) := Pointer(FOwner);
  Filter.get_FilterCount(cnt);
  for i := 0 to cnt -1 do
    Filter.set_EnableFilter(i, false);
end;

procedure TTrayIcon.OnEnableAudioStreamClick(Sender: TObject);
var
  Filter: TDCDSPFilter;
begin
  Pointer(Filter) := Pointer(FOwner);
  Filter.Enable((Sender as TMenuItem).Tag, AMSTREAMSELECTENABLE_ENABLE);
end;

procedure TTrayIcon.OnEnablePresetClick(Sender: TObject);
var
  Filter: TDCDSPFilter;
begin
  Pointer(Filter) := Pointer(FOwner);
  Filter.set_LoadPreset(PChar((Sender as TMenuItem).Caption));
end;

procedure TTrayIcon.OnShowPropertyPageClick(Sender: TObject);
var
  Filter: TDCDSPFilter;
  graph: IFilterGraph;
  Enum: IEnumfilters;
  filt: IBaseFilter;
  i: integer;
begin
  Pointer(Filter) := Pointer(FOwner);
  graph := Filter.GetFilterGraph;

  i := (Sender as TMenuItem).Tag;
  if (Assigned(graph)) then
  begin
    if Graph.EnumFilters(Enum) = S_Ok then
    begin
      Enum.Skip(i);
      if Enum.Next(1, filt, nil) = S_OK then
        ShowFilterPropertyPage(GetDesktopWindow, filt);
      Enum := nil;
    end;
    Graph := nil;
  end;
end;

procedure TTrayIcon.CloseForm;
begin
  if Assigned(fForm) then
    fForm.Close;
  fForm := nil;
end;

procedure TTrayIcon.ShowAudioStreams;
var
  m: String;
  Filter: TDCDSPFilter;
  cnt: Cardinal;
  s: String;
  i: integer;
  ppmt: PamMediaType;
  flags, lcid, group: DWORD;
  name: PWChar;
  obj, unk: IUnknown;
begin
  Filter := TDCDSPFilter(FOwner);
  m := '';
  if (Filter.Count(cnt) = S_OK) then
  begin
    for i := 0 to cnt -1 do
    begin
       Filter.Info(i, ppmt, flags, lcid, group, name, obj, unk);
       if (flags and AMSTREAMSELECTINFO_ENABLED = AMSTREAMSELECTINFO_ENABLED)
         then s := '  ' + name;
       m := m + '  ' + name + #13#10;
       if Assigned(ppmt) then
       begin
         DeleteMediaType(ppmt);
         ppmt := nil;
       end;
       if Assigned(name) then
       begin
         CoTaskMemFree(name);
         name := nil;
       end;
       if Assigned(obj) then obj := nil;
       if Assigned(unk) then unk := nil;
       lcid := 0;
       group := 0;
       flags := 0;
    end;
  end;
  m := 'Available Languages:' + #13#10 + m;
  m := m + #13#10 + 'Selected Language: ' + #13#10 + s;
  fTrayIcon.BalloonHint('Multiple Audio Streams available', m, btInfo, 5000);
end;

end.
