
    (*********************************************************************
     *  PropWinamp.pas                                                   *
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

unit PropWinamp;

{$I Compiler.inc}

interface

uses
  Windows, Messages, Classes,DirectShow9, ExtCtrls, BaseClass, Forms, Buttons,
  StdCtrls, Controls, Dialogs, SysUtils, Registry, ControlDCDSPFilter, Utils,
  DCDSPTypes, waConst, ComCtrls;

type
  TFormPropWinamp = class(TFormPropertyPage)
    OpenDialog1: TOpenDialog;
    TabControl1: TTabControl;
    Label2: TLabel;
    GroupBox1: TGroupBox;
    btnDeleteWindow: TSpeedButton;
    btnChooseWindow: TSpeedButton;
    btnShowWindow: TSpeedButton;
    btnHideWindow: TSpeedButton;
    cmbWindows: TComboBox;
    chkPluginEnabled: TCheckBox;
    GroupBox2: TGroupBox;
    SpeedButton1: TSpeedButton;
    edPluginPath: TEdit;
    Button2: TButton;
    GroupBox3: TGroupBox;
    lbPlugins: TListBox;
    cmbPlugins: TComboBox;
    Button1: TButton;
    procedure btnSetDirectoryExecute(Sender: TObject);
    procedure lbPluginsClick(Sender: TObject);
    procedure cmbPluginsChange(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure chkPluginEnabledClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnDeleteWindowClick(Sender: TObject);
    procedure btnChooseWindowMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnChooseWindowMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnChooseWindowMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure btnShowWindowClick(Sender: TObject);
    procedure btnHideWindowClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure OnDeleteKlick(Sender: TObject);
  private
    bClass, bCaption : array[0..MAX_PATH -1] of Char;
    fMouseDown : Boolean;
    fDCDSPFilter : TDCDSPFilterControl;
    PluginPath : String;
    Plugins : array[0..99] of TDSPPlugin;
    PluginCount : integer;
    PluginIndex : integer;
    LastWnd : hWnd;
    function FileList (DirName : String) : TStringList;
    procedure ReloadDSPWindows;
  public
    function OnConnect(Unknown: IUnknown): HRESULT; override;
  end;

implementation

{$R *.DFM}

function TFormPropWinamp.FileList(DirName : String) : TStringList;
var
  srh: THandle;
  sr : TWin32FindData;
  tstr : TStringList;
  i : integer;
  LibHandle : Cardinal;
  GetModule : function : pointer; stdcall;
  DSPHeader : PWinampDSPHeader;
  LoadedPlugin : String;
begin
  LoadedPlugin := fDCDSPFilter.Get_DSPPlugin;
  PluginIndex := -1;
  tstr := TStringList.Create;
  tstr.Add('(None)');
  srh := FindFirstFile(PChar(Dirname + 'dsp_*.dll'),sr);
  i := 0;
  if srh <> INVALID_HANDLE_VALUE then
  begin
    repeat
      inc(i);
      Plugins[i - 1].FileName := DirName + sr.cFileName;
      LibHandle := LoadLibrary(PChar(Plugins[i - 1].FileName));
      if LibHandle <> 0 then
      begin
        try
          GetModule := GetProcAddress(LibHandle,pchar('winampDSPGetHeader2'));
        except
          GetModule := nil;
        end;
        if GetModule = nil then
        begin
          FreeLibrary(LibHandle);
//          Plugins[i - 1].FileName := '';
//          Plugins[i - 1].Desription := 'Failed loading Module ' + sr.cFileName;
            dec(i);
            continue;
        end else
        begin
          DSPHeader := GetModule;
          if DSPHeader.Version <> $20 then
          begin
            dec(i);
            continue;
          end;
          Plugins[i - 1].Desription := DSPHeader^.Description;
          FreeLibrary(LibHandle);
          if Lowercase((Plugins[i - 1].FileName)) = LowerCase(LoadedPlugin) then PluginIndex := i;
       end;
      end else
      begin
        Plugins[i - 1].FileName := '';
        Plugins[i - 1].Desription := 'Failed loading ' + sr.cFileName;
      end;
      tstr.Add(Plugins[i - 1].Desription)
    until FindNextFile(srh,sr) = False;
    Windows.FindClose(srh);
  end;
  PluginCount := i;
  Result := tstr;
end;

function TFormPropWinamp.OnConnect(Unknown: IUnKnown): HRESULT;
var
  Reg : TRegistry;
  i : integer;
  Desc : PChar;
begin
  fDCDSPFilter := TDCDSPFilterControl.Create(nil);
  fDCDSPFilter.DCDSPFilter := Unknown;
  Reg:=TRegistry.Create;
  Reg.Rootkey:=HKEY_CURRENT_USER;
  if Reg.OpenKey('SOFTWARE\DSP-Worx\DC-DSP Filter',False) then
  begin
    try
      PluginPath := Reg.ReadString('PluginsDirectory');
    except
    {$IFDEF WITH_WINAMP}
      PluginPath := GetWinampDir;
    {$ELSE}
      PluginPath := '';
    {$ENDIF}
    end;
    {$IFDEF WITH_WINAMP}
      if PluginPath = '' then PluginPath := GetWinampDir;
    {$ENDIF}
    if PluginPath <> '' then
    begin
      edPluginPath.Text := PluginPath;
      lbPlugins.Items := FileList(PluginPath);
      Refresh;
    end;
    Reg.CloseKey;
  end else
  begin
    {$IFDEF WITH_WINAMP}
      PluginPath := GetWinampDir;
    {$ELSE}
      PluginPath := '';
    {$ENDIF}
    if PluginPath <> '' then
    begin
      edPluginPath.Text := PluginPath;
      lbPlugins.Items := FileList(PluginPath);
      Refresh;
    end;
  end;
  Reg.Free;
  chkPluginEnabled.Checked := fDCDSPFilter.DSPPlugEnabled;
  if fDCDSPFilter.DSPPluginCount > 0 then
  begin
    for i := 0 to fDCDSPFilter.DSPPluginCount -1 do
    begin
      fDCDSPFilter.Get_DSPSubDescription(i,Desc);
      cmbPlugins.Items.Add(Desc);
    end;
    cmbPlugins.ItemIndex := fDCDSPFilter.DSPPlugin;
  end;
  ReloadDSPWindows;
  lbPlugins.ItemIndex := PluginIndex;
  if PluginIndex > -1 then cmbPlugins.ItemIndex := fDCDSPFilter.DSPPlugin;
  result := NOERROR;
end;

procedure TFormPropWinamp.btnSetDirectoryExecute(Sender: TObject);
begin
  OpenDialog1.InitialDir := PluginPath;
  if OpenDialog1.Execute then
  begin
    PluginPath := ExtractFilePath(opendialog1.FileName);
    edPluginPath.Text := PluginPath;
    lbPlugins.Items := FileList(PluginPath);
    Refresh;
  end;
end;

procedure TFormPropWinamp.lbPluginsClick(Sender: TObject);
var
  i : integer;
  Desc : PChar;
begin
  if lbPlugins.ItemIndex > 0 then
  begin
    fDCDSPFilter.Set_DSPPlugin(0,PChar(Plugins[lbPlugins.ItemIndex - 1].FileName));
    cmbPlugins.Clear;
    if fDCDSPFilter.DSPPluginCount > 0 then
    begin
      for i := 0 to fDCDSPFilter.DSPPluginCount -1 do
      begin
        fDCDSPFilter.Get_DSPSubDescription(i,Desc);
        cmbPlugins.Items.Add(Desc);
      end;
      cmbPlugins.ItemIndex := 0;
      cmbPluginsChange(Self);
    end;
  end else
  begin
    fDCDSPFilter.UnloadDSPPlugin;
    cmbPlugins.Clear;
  end;
end;

procedure TFormPropWinamp.cmbPluginsChange(Sender: TObject);
begin
  fDCDSPFilter.DSPPlugin := cmbPlugins.ItemIndex;
end;

procedure TFormPropWinamp.SpeedButton2Click(Sender: TObject);
begin
  fDCDSPFilter.Set_DSPPluginOwner(Handle);
  fDCDSPFilter.DSPShowConfig;
  fDCDSPFilter.Set_DSPPluginOwner(GetDesktopWindow);
end;

procedure TFormPropWinamp.chkPluginEnabledClick(Sender: TObject);
begin
  fDCDSPFilter.DSPPlugEnabled := chkPluginEnabled.Checked;
end;

procedure TFormPropWinamp.FormDestroy(Sender: TObject);
var
  Reg : TRegistry;
begin
  Reg:=TRegistry.Create;
  Reg.Rootkey:=HKEY_CURRENT_USER;
  if Reg.OpenKey('SOFTWARE\DSP-Worx\DC-DSP Filter',True) then
  begin
    Reg.WriteString('PluginsDirectory',PluginPath);
    Reg.CloseKey;
  end;
  Reg.Free;
  fDCDSPFilter.Free;
end;

procedure TFormPropWinamp.FormCreate(Sender: TObject);
begin
  ClientWidth := PPWidth;
  ClientHeight := PPHeight;
  Screen.Cursors[1] := LoadCursor(HInstance,MakeIntResource(11));
  btnChooseWindow.Cursor := 1;
end;

procedure TFormPropWinamp.FormShow(Sender: TObject);
begin
//  fDCDSPFilter.setDSPPluginOwner(Handle);
  if lbPlugins.ItemIndex = -1 then lbPlugins.ItemIndex := 0;  
//  SetWindowLong(Button1.Handle,GWL_STYLE,GetWindowLong(Button1.Handle,GWL_STYLE) or BS_FLAT);
//  SetWindowLong(Button2.Handle,GWL_STYLE,GetWindowLong(Button2.Handle,GWL_STYLE) or BS_FLAT);
end;

procedure TFormPropWinamp.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
//  fDCDSPFilter.setDSPPluginOwner(GetDesktopWindow);
end;

procedure TFormPropWinamp.ReloadDSPWindows;
var
  Reg : TRegistry;
  str : TStringList;
  i : integer;
begin
  cmbWindows.Clear;
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_CURRENT_USER;
  if Reg.OpenKeyReadOnly('SOFTWARE\DSP-Worx\DC-DSP Filter\WADSPWindows') then
  begin
    str := TStringList.Create;
    Reg.GetValueNames(str);
    if str.Count > 0 then
    begin
      for i := 0 to str.Count -1 do
      begin
        cmbWindows.Items.Add(str.Strings[i]);
      end;
      cmbWindows.ItemIndex := 0;
    end;
  end;
end;

procedure TFormPropWinamp.btnDeleteWindowClick(Sender: TObject);
var
  Res : String;
  Reg : TRegistry;
begin
  if cmbWindows.ItemIndex < 0 then
  begin
    MessageBox(Handle,'No Window selected','Error',MB_OK or MB_ICONERROR);
  end else
  begin
    if MessageDlg( 'Do you really want to delete this Window?',mtConfirmation, [mbYes, mbNo], 0) = mrNo then Exit;
    Res := cmbWindows.Items.Strings[cmbWindows.ItemIndex];
    Reg := TRegistry.Create;
    Reg.Rootkey:=HKEY_CURRENT_USER;
    if Reg.OpenKey('SOFTWARE\DSP-Worx\DC-DSP Filter\WADSPWindows',True) then
    begin
      if Reg.ValueExists(Res) then Reg.DeleteValue(Res);
    end;
    Reg.Free;
    ReloadDSPWindows;
  end;
end;

procedure TFormPropWinamp.btnChooseWindowMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then fMouseDown := True;
end;

procedure TFormPropWinamp.btnChooseWindowMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  str : String;
  Reg : TRegistry;
begin
  if fMouseDown then
  begin
    fMouseDown := False;
    label2.Caption := '';
    str := String(bClass)+ ',' + String(bCaption);
    if InputQuery('Do you want to add this Window ?','You can also modify this Value. First Parameter (everything before the commata) is the Window Class. The second Window (everything after the commata) is the Window Name. Do not delete the commata !!!',str) then
    begin
      Reg := TRegistry.Create;
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey('SOFTWARE\DSP-Worx\DC-DSP Filter\WADSPWindows',True) then
      begin
        Reg.WriteInteger(str,0);
        Reg.CloseKey;
      end;
      Reg.Free;
      ReloadDSPWindows;
    end;
    Refresh;
  end;
end;

procedure TFormPropWinamp.btnChooseWindowMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  wnd : hWnd;
  p : tpoint;
  fl : TFlashWInfo;
begin
  if fMouseDown then
  begin
    GetCursorPos(p);
    wnd := WindowFromPoint(p);
    if not IsWindow(wnd) then Exit;
    if LastWnd = wnd then Exit;
    fl.cbSize := sizeof(TFlashWInfo);
    fl.uCount := 10;
    fl.dwTimeout := 1;
    fl.hwnd := wnd;
    fl.dwFlags := FLASHW_ALL;
    FlashWindowEx(fl);
    GetClassName(wnd,@bClass,MAX_PATH);
    GetWindowText(wnd,@bCaption,MAX_PATH);
    label2.Caption := '"' + String(bClass)+ '","' + String(bCaption)+'"';
    lastwnd := wnd;
  end;
end;

procedure TFormPropWinamp.btnShowWindowClick(Sender: TObject);
begin
  if cmbWindows.ItemIndex < 0 then Exit;
{$IFDEF WITH_WINAMP}
  ShowWinampDSPWindow(cmbWindows.Items[cmbWindows.ItemIndex],SW_SHOW);
{$ENDIF}
end;

procedure TFormPropWinamp.btnHideWindowClick(Sender: TObject);
begin
  if cmbWindows.ItemIndex < 0 then Exit;
{$IFDEF WITH_WINAMP}
  ShowWinampDSPWindow(cmbWindows.Items[cmbWindows.ItemIndex],SW_HIDE);
{$ENDIF}
end;

procedure TFormPropWinamp.FormPaint(Sender: TObject);
begin
  UpdateWindow(lbPlugins.Handle);
end;

procedure TFormPropWinamp.OnDeleteKlick(Sender: TObject);
begin
  fDCDSPFilter.UnloadDSPPlugin;
  PluginPath := ' ';
  edPluginPath.Text := PluginPath;
  lbPlugins.Clear;
  lbPlugins.Items.Add('(None)');
  cmbPlugins.Clear;
  Refresh;
end;

initialization

  TBCClassFactory.CreatePropertyPage(TFormPropWinamp, CLSID_DCDSPFilterPropertyPageWinamp);

end.
