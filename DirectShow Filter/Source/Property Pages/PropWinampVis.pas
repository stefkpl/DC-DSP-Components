
    (*********************************************************************
     *  PropWinampVis.pas                                                *
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

unit PropWinampVis;

{$I Compiler.inc}

interface

uses
  Windows, Messages, Classes,DirectShow9, ExtCtrls, BaseClass, Forms, Buttons,
  StdCtrls, Controls, Dialogs, SysUtils, Registry, ControlDCDSPFilter, Utils,
  DCDSPTypes, Spin, waConst, ComCtrls;

type
  TFormPropWinampVis = class(TFormPropertyPage)
    OpenDialog1: TOpenDialog;
    TabControl1: TTabControl;
    Label3: TLabel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    SpeedButton1: TSpeedButton;
    edPluginPath: TEdit;
    Button2: TButton;
    lbPlugins: TListBox;
    cmbPlugins: TComboBox;
    Button1: TButton;
    Button3: TButton;
    Button4: TButton;
    Label2: TLabel;
    SpinEdit1: TSpinEdit;
    chkAutostart: TCheckBox;
    procedure SpeedButton1Click(Sender: TObject);
    procedure lbPluginsClick(Sender: TObject);
    procedure cmbPluginsChange(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure chkAutostartClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure OnDeleteDirectory(Sender: TObject);
  private
    fDCDSPFilter : TDCDSPFilterControl;
    PluginPath : String;
    Plugins : array[0..99] of TDSPPlugin;
    PluginCount : integer;
    PluginIndex : integer;
    CanChange : Boolean;
    function FileList (DirName : String) : TStringList;
  public
    fLibHandle : Cardinal;
    fGetModule : function : Pointer; cdecl;
    fVisHeader : PWinampVisHeader;
    fVisModule : PWinampVisModule;
    function OnConnect(Unknown: IUnknown): HRESULT; override;
  end;

implementation

{$R *.DFM}

function TFormPropWinampVis.FileList(DirName : String) : TStringList;
var
  srh: THandle;
  sr : TWin32FindData;
  tstr : TStringList;
  i : integer;
  LibHandle : Cardinal;
  GetModule : function : Pointer; cdecl;
  VisHeader : PWinampVisHeader;
  LoadedPlugin : String;
  LoadedPluginIndex : integer;
  PlugFound : Boolean;
begin
  PlugFound := False;
  fDCDSPFilter.Get_WinampVisPlugin(LoadedPlugin,LoadedPluginIndex);
  tstr := TStringList.Create;
  srh := FindFirstFile(PChar(Dirname + 'vis_*.dll'),sr);
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
          GetModule := GetProcAddress(LibHandle,pchar('winampVisGetHeader'));
        except
          GetModule := nil;
        end;
        if GetModule = nil then
        begin
          FreeLibrary(LibHandle);
          Plugins[i - 1].FileName := '';
          Plugins[i - 1].Desription := 'Failed loading Module ' + sr.cFileName;
        end else
        begin
          VisHeader := GetModule;
          Plugins[i - 1].Desription := VisHeader^.Description;
          FreeLibrary(LibHandle);
          if Lowercase((Plugins[i - 1].FileName)) = LowerCase(LoadedPlugin) then
          begin
            PluginIndex := i;
            PlugFound := True;
          end;
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
  if lbPlugins.Items.Count > 0 then
  begin
    if PlugFound then lbPlugins.ItemIndex := PluginIndex
                 else lbPlugins.ItemIndex := 0;
    lbPluginsClick(Self);
    if PlugFound then cmbPlugins.ItemIndex := LoadedPluginIndex;
  end;
  PluginCount := i;
  Result := tstr;
end;

function TFormPropWinampVis.OnConnect(Unknown: IUnKnown): HRESULT;
var
  Reg : TRegistry;
begin
  fDCDSPFilter := TDCDSPFilterControl.Create(nil);
  fDCDSPFilter.DCDSPFilter := Unknown;
  Reg:=TRegistry.Create;
  Reg.Rootkey:=HKEY_CURRENT_USER;
  CanChange := False;
  if Reg.OpenKey('SOFTWARE\DSP-Worx\DC-DSP Filter',False) then
  begin
    try
      PluginPath := Reg.ReadString('VisPluginsDirectory');
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
    {$ENDIF}
    if PluginPath <> '' then
    begin
      edPluginPath.Text := PluginPath;
      lbPlugins.Items := FileList(PluginPath);
      Refresh;
    end;
  end;
  Reg.Free;
  SpinEdit1.Value := fDCDSPFilter.WinampVisInterval;
  chkAutostart.Checked := fDCDSPFilter.AutoLoadWinampVis;
  result := NOERROR;
end;

procedure TFormPropWinampVis.SpeedButton1Click(Sender: TObject);
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

procedure TFormPropWinampVis.lbPluginsClick(Sender: TObject);
var
  i : integer;
begin
  if not CanChange then cmbPlugins.Clear;
  if lbPlugins.ItemIndex > -1 then
  begin
    if fLibHandle <> 0 then FreeLibrary(fLibHandle);
    fLibHandle := 0;
    fLibHandle := LoadLibrary(PChar(Plugins[lbPlugins.ItemIndex].FileName));
    if fLibHandle <> 0 then
    begin
      fGetModule := GetProcAddress(fLibHandle,pchar('winampVisGetHeader'));
      if fGetModule <> nil then
      begin
        fVisHeader := fGetModule;
        i := 0;
        fVisModule := fVisHeader.getModule(i);
        while fVisModule <> nil do
        begin
          if not CanChange then cmbPlugins.Items.Add(fVisModule.description);
          inc(i);
          fVisModule := fVisHeader.getModule(i);
        end;
        fVisModule := nil;
      end;
      if (cmbPlugins.Items.Count > 0) and not CanChange then
      begin
        cmbPlugins.ItemIndex := 0;
        cmbPluginsChange(Self);
      end;
    end;
  end;
end;

procedure TFormPropWinampVis.cmbPluginsChange(Sender: TObject);
begin
  if fVisHeader <> nil then
  begin
    CanChange := True;
    lbPluginsClick(Self);
    CanChange := False;
    fVisModule := fVisHeader.getModule(cmbPlugins.itemindex);
    if fVisModule <> nil then
    begin
      fVisModule.hDllInstance := fLibHandle;
      fVisModule.hwndParent := Handle; // GetDesktopWindow;
    end;
  end;
end;

procedure TFormPropWinampVis.SpeedButton2Click(Sender: TObject);
var
  z : integer;
begin
  if lbPlugins.ItemIndex < 0 then Exit;
  z := cmbPlugins.ItemIndex;
  lbPluginsClick(Self);
  cmbPlugins.ItemIndex := z;
  cmbPluginsChange(Self);
  if fVisModule <> nil then
  begin
//    fVisModule.hwndParent := Handle;
    fVisModule.Config(fVisModule);
//    fVisModule.hwndParent := GetDesktopWindow;
  end;
end;

procedure TFormPropWinampVis.FormDestroy(Sender: TObject);
var
  Reg : TRegistry;
begin
  Reg:=TRegistry.Create;
  Reg.Rootkey:=HKEY_CURRENT_USER;
  if Reg.OpenKey('SOFTWARE\DSP-Worx\DC-DSP Filter',True) then
  begin
    Reg.WriteString('VisPluginsDirectory',PluginPath);
    Reg.CloseKey;
  end;
  Reg.Free;
  fDCDSPFilter.Free;
end;

procedure TFormPropWinampVis.FormCreate(Sender: TObject);
begin
  ClientWidth := PPWidth;
  ClientHeight := PPHeight;
end;

procedure TFormPropWinampVis.FormShow(Sender: TObject);
begin
  if lbPlugins.Items.Count > 0 then
  begin
    if PluginIndex > 0 then lbPlugins.ItemIndex := PluginIndex -1 else lbPlugins.ItemIndex := 0;
    lbPluginsClick(Self);
  end;
end;

procedure TFormPropWinampVis.SpinEdit1Change(Sender: TObject);
begin
  fDCDSPFilter.WinampVisInterval := SpinEdit1.Value;
end;

procedure TFormPropWinampVis.Button3Click(Sender: TObject);
begin
  fDCDSPFilter.StopWinampVisPlugin;
end;

procedure TFormPropWinampVis.Button4Click(Sender: TObject);
begin
  if lbPlugins.ItemIndex < 0 then Exit;
  fDCDSPFilter.Set_WinampVisPlugin(Plugins[lbPlugins.ItemIndex].FileName,cmbPlugins.ItemIndex);
end;

procedure TFormPropWinampVis.chkAutostartClick(Sender: TObject);
begin
  fDCDSPFilter.AutoLoadWinampVis := chkAutostart.Checked;
end;

procedure TFormPropWinampVis.FormPaint(Sender: TObject);
begin
  UpdateWindow(lbPlugins.Handle);
end;

procedure TFormPropWinampVis.OnDeleteDirectory(Sender: TObject);
begin
  fDCDSPFilter.StopWinampVisPlugin;
  PluginPath := ' ';
  edPluginPath.Text := PluginPath;
  lbPlugins.Clear;
  cmbPlugins.Clear;
  Refresh;
end;

initialization

  TBCClassFactory.CreatePropertyPage(TFormPropWinampVis, CLSID_DCDSPFilterPropertyPageWinampVis);

end.
