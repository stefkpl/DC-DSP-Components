unit formDSPEqualizer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dspEqualizer, StdCtrls, ComCtrls, DynamicFilterList, dspConst,
  Buttons, Registry;

const
  EQPresetCount = 19;

  EQPresetNames : array[0..EQPresetCount -1] of String = (
    '(Default)','Classical','Club','Dance','Full Bass','Full Bass & Treble',
    'Full Treble','Labtop','Large Hall','Live','Loudness','Party','Pop',
    'Reggae','Rock','Ska','Soft','Soft Rock','Techno'
  );

  EQPreset : array[0..EQPresetCount -1] of array[0..9] of ShortInt =
  (
    (0,0,0,0,0,0,0,0,0,0),                   // (Default)
    (0,0,0,0,0,-10,40,40,40,50),             // Classical
    (0,0,-10,-20,-20,-20,-10,0,0,0),         // Club
    (-50,-40,-20,5,0,20,30,20,0,0),          // Dance
    (-50,-50,-50,-30,-10,10,30,50,50,50),    // Full Bass
    (-30,-20,0,30,20,-10,-40,-50,-50,-50),   // Full Bass & Treble
    (50,50,50,20,-10,-50,-80,-80,-80,-90),   // Full Treble
    (-15,-50,-25,20,10,-5,-15,-30,-50,-60),  // Labtop
    (-50,-50,-30,-20,0,20,30,30,0,0),        // Large Hall
    (30,0,-10,-15,-20,-20,-10,-5,-5,0),      // Live
    (-90,-70,-50,-20,0,-10,-20,-50,-70,-90), // Loudness
    (-30,-30,0,0,0,0,0,0,-30,-30),           // Party
    (10,-15,-25,-25,-15,10,20,20,10,10),     // Pop
    (0,0,5,30,0,-30,-30 ,0,0,0),             // Reggae
    (-40,-30,30,40,20,-20,-40,-40,-40,-40),  // Rock
    (10,20,10,0,-10,-20,-30,-35,-30,-25),    // Ska (whatever that means ...)
    (-15,-5,5,15,5,-15,-30,-40,-45,-50),     // Soft
    (-15,-15,-10,5,15,25,15,5,-10,-30),      // Soft Rock
    (-30,-30,0,30,20,0,-20,-30,-30,-20)      // Techno
  );


type
  PEQBandsVolume = ^TEQBandsVolume;
  TEQBandsVolume = array[0..9] of array[0..NumEqBands -1] of ShortInt;

  TfrmDSPEqualizer = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label11: TLabel;
    lab0: TLabel;
    lab1: TLabel;
    lab2: TLabel;
    lab3: TLabel;
    lab4: TLabel;
    lab5: TLabel;
    lab6: TLabel;
    lab7: TLabel;
    lab8: TLabel;
    lab9: TLabel;
    Label1: TLabel;
    Label10: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    btnResetPreset: TSpeedButton;
    btnDeletePreset: TSpeedButton;
    btnSavePreset: TSpeedButton;
    eq1: TTrackBar;
    eq2: TTrackBar;
    eq3: TTrackBar;
    eq4: TTrackBar;
    eq5: TTrackBar;
    eq6: TTrackBar;
    eq7: TTrackBar;
    eq8: TTrackBar;
    eq9: TTrackBar;
    eq0: TTrackBar;
    cmbEQChannels: TComboBox;
    cmbEqualizerPreset: TComboBox;
    cmbEqualizerMaxDB: TComboBox;
    procedure cmbEQChannelsChange(Sender: TObject);
    procedure eq0Change(Sender: TObject);
    procedure cmbEqualizerMaxDBChange(Sender: TObject);
    procedure btnSavePresetClick(Sender: TObject);
    procedure btnDeletePresetClick(Sender: TObject);
    procedure btnResetPresetClick(Sender: TObject);
    procedure cmbEqualizerPresetChange(Sender: TObject);
  private
    fPlugin : TDCEqualizer;
    fItem : TDCFilterItem;
    fSaving : PEQSavings;
    fEQChanging : Boolean;
    fPresetChange : Boolean;
    procedure WritePresets;
    procedure SetEqualizerPreset(Index : integer);
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDSPEqualizer: TfrmDSPEqualizer;

implementation

{$R *.dfm}

constructor TfrmDSPEqualizer.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
var
  str : TStringlist;
  i : integer;
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCEqualizer(Item.Filter);
  fItem := Item;
  fSaving := PEQSavings(fItem.ExtraBuffer);

  str := TStringlist.Create;
  with TRegistry.Create do
  begin
    Rootkey := HKEY_CURRENT_USER;
    if OpenKey('SOFTWARE\DSP-worx\DC-DSP Filter\EQPresets',False) then
    begin
      GetValueNames(str);
      if str.Count > 0 then
      begin
        for i := 0 to str.Count -1 do
        begin
          cmbEqualizerPreset.Items.Add(str.Strings[i]);
        end;
        CloseKey;
        Free;
      end else
      begin
        CloseKey;
        Free;
        WritePresets;
      end;
    end else
    begin
      Free;
      WritePresets;
    end;
  end;
  str.Free;

  if fSaving <> nil then
  begin
    fPresetChange := True;
    if fSaving.DB = 0 then fSaving.DB := 12;
    cmbEQChannels.ItemIndex := fSaving.ChannelPos;
    cmbEQChannelsChange(Self);
    cmbEqualizerMaxDB.ItemIndex := cmbEqualizerMaxDB.Items.IndexOf(inttostr(fSaving.DB));
    cmbEqualizerPreset.ItemIndex := cmbEqualizerPreset.Items.IndexOf(Trim(String(fSaving.Preset)));
    fPresetChange := False;
  end;
  Show;
end;

procedure TfrmDSPEqualizer.cmbEQChannelsChange(Sender: TObject);
var
  i, c : integer;
begin
  fEQChanging := True;
  i := cmbEQChannels.ItemIndex;
  fPlugin.Seperate := i > 0;
  if fSaving <> nil then fSaving.ChannelPos := i;
  if i > 0 then dec(i);
  for c := 0 to NumEQBands -1 do
    (FindComponent('eq'+inttostr(c)) as TTrackBar).Position := GetNdbEQ(0 - fPlugin.Band[i,EQFreq[c]],fSaving.DB);
  fEQChanging := False;
end;

procedure TfrmDSPEqualizer.eq0Change(Sender: TObject);
var
  i, z, c : integer;
  tb : TTrackBar;
begin
  i := cmbEQChannels.ItemIndex;
  if i > 0 then dec(i);
  if not fPresetChange then
  begin
    cmbEqualizerPreset.ItemIndex := -1;
    FillChar(fSaving.Preset,256,' ');
  end;
  for c := 0 to NumEQBands -1 do
  begin
    tb := FindComponent('eq'+inttostr(c)) as TTrackBar;
    if fSaving <> nil then fSaving.Band[i,c] := MakeNdbEQ(0 - tb.Position,fSaving.DB);
    if not fEQChanging then
      for z := EQFreq[c] to EQFreq[c+1] -1 do
        fPlugin.Band[i,z] := MakeNdbEQ(0 - tb.Position,fSaving.DB);
    (FindComponent('lab' + inttostr(c)) as TLabel).Caption := Inttostr(MakeNdbEQ((0 - tb.Position) div 5,fSaving.DB));
  end;
end;

procedure TfrmDSPEqualizer.cmbEqualizerMaxDBChange(Sender: TObject);
begin
  fSaving.DB := StrToInt(cmbEqualizerMaxDB.Text);
  if fSaving <> nil then fSaving.DB := fSaving.DB;
  eq0Change(Self);
end;

procedure TfrmDSPEqualizer.btnSavePresetClick(Sender: TObject);
var
  Res : String;
  f : integer;
  i, c : integer;
  fEQPreset : TEQBandsVolume;
begin
  Res := InputBox('Select Presetname','Type here the Name of the Preset','');
  if Res = '' then Exit;
  f := cmbEqualizerPreset.Items.IndexOf(Res);
  if f = 0 then
  begin
    MessageBox(Handle,'Can not overwrite (Default) Preset','Error',MB_OK or MB_ICONERROR);
    Exit;
  end else
  if f > 0 then
  begin
    if MessageDlg( 'Preset already exists. Do you want to overwrite it?',mtConfirmation, [mbYes, mbNo], 0) = mrNo then Exit;
  end;
  with TRegistry.Create do
  begin
    Rootkey := HKEY_CURRENT_USER;
    if OpenKey('SOFTWARE\DSP-worx\DC-DSP Filter\EQPresets',True) then
    begin
      for i := 0 to 9 do
        for c := 0 to 9 do
          fEQPreset[i,c] := fSaving.Band[i,c];
      if ValueExists(Res) then DeleteValue(Res);
      WriteBinaryData(Res,fEQPreset,sizeof(fEQPreset));
      if f < 0 then cmbEqualizerPreset.Items.Add(Res)
               else cmbEqualizerPreset.Items.Strings[f] := Res;
      cmbEqualizerPreset.ItemIndex := cmbEqualizerPreset.Items.IndexOf(Res);
      FillChar(fSaving.Preset,256,' ');
      if Length(Res) > 0 then Move(PChar(Res)^,fSaving.Preset,Length(Res));
    end;
    Free;
  end;
end;

procedure TfrmDSPEqualizer.btnDeletePresetClick(Sender: TObject);
var
  Res : String;
begin
  if cmbEqualizerPreset.ItemIndex < 0 then
  begin
    MessageBox(Handle,'No Preset selected','Error',MB_OK or MB_ICONERROR);
  end else
  if cmbEqualizerPreset.ItemIndex = 0 then
  begin
    MessageBox(Handle,'Can not delete "(Default)" Preset','Error',MB_OK or MB_ICONERROR);
  end else
  begin
    if MessageDlg( 'Do you really want to delete this Preset?',mtConfirmation, [mbYes, mbNo], 0) = mrNo then Exit;
    Res := cmbEqualizerPreset.Items.Strings[cmbEqualizerPreset.ItemIndex];
    with TRegistry.Create do
    begin
      Rootkey := HKEY_CURRENT_USER;
      if OpenKey('SOFTWARE\DSP-worx\DC-DSP Filter\EQPresets',True) and
         ValueExists(Res) then DeleteValue(Res);
      Free;
    end;
    cmbEqualizerPreset.Items.Delete(cmbEqualizerPreset.ItemIndex);
    cmbEqualizerPreset.ItemIndex := -1;
  end;
end;

procedure TfrmDSPEqualizer.btnResetPresetClick(Sender: TObject);
begin
  if MessageDlg('Do you really want to reset all Presets?',mtConfirmation, [mbYes, mbNo], 0) = mrNo then Exit;
  with TRegistry.Create do
  begin
    Rootkey := HKEY_CURRENT_USER;
    if KeyExists('SOFTWARE\DSP-worx\DC-DSP Filter\EQPresets') then
      DeleteKey('SOFTWARE\DSP-worx\DC-DSP Filter\EQPresets');
    Free;
  end;
  WritePresets;
  cmbEqualizerPresetChange(Self);
end;

procedure TfrmDSPEqualizer.cmbEqualizerPresetChange(Sender: TObject);
var
  str : String;
begin
  fPresetChange := True;
  SetEqualizerPreset(cmbEqualizerPreset.ItemIndex);
  str := cmbEqualizerPreset.Items.Strings[cmbEqualizerPreset.ItemIndex];
  FillChar(fSaving.Preset,256,' ');
  if (Length(str) > 0) then Move(PChar(str)^,fSaving.Preset,Length(str));
  fPresetChange := False;
end;

procedure TfrmDSPEqualizer.SetEqualizerPreset(Index : integer);
var
  Res : String;
  i, c, z : integer;
  fEQPreset : TEQBandsVolume;
begin
  Res := cmbEqualizerPreset.Items.Strings[Index];
  with TRegistry.Create do
  begin
    Rootkey := HKEY_CURRENT_USER;
    if OpenKey('SOFTWARE\DSP-worx\DC-DSP Filter\EQPresets',False) then
    begin
      if not ValueExists(Res) then
      begin
        Free;
        Exit;
      end;
      ReadBinaryData(Res,fEQPreset,sizeof(fEQPreset));
      Free;
    end else
    begin
      Free;
      Exit;
    end;
  end;

  fEQChanging := True;
  for i := 0 to MaxChannels -1 do
  begin
    for c := 0 to NumEQBands -1 do
    begin
      if fSaving <> nil then fSaving.Band[i,c] := MakeNdbEQ(0 - fEQPreset[i,c],fSaving.DB);
      for z := EQFreq[c] to EQFreq[c+1] -1 do
        fPlugin.Band[i,z] := MakeNdbEQ(0 - fEQPreset[i,c],fSaving.DB);
    end;
  end;
  cmbEQChannelsChange(Self);
  fEQChanging := False;
  eq0Change(Self);
end;

procedure TfrmDSPEqualizer.WritePresets;
var
  c,i,z : integer;
  fEQPreset : TEQBandsVolume;
  str : String;
begin
  with TRegistry.Create do
  begin
    Rootkey := HKEY_CURRENT_USER;
    if OpenKey('SOFTWARE\DSP-worx\DC-DSP Filter\EQPresets',True) then
    begin
      cmbEqualizerPreset.Clear;
      for c := 0 to EQPresetCount -1 do
      begin
        cmbEqualizerPreset.Items.Add(EQPresetNames[c]);
        for z := 0 to 9 do
          for i := 0 to 9 do fEQPreset[z,i] := EQPreset[c][i];
            WriteBinaryData(EQPresetNames[c],fEQPreset,sizeof(fEQPreset));
      end;
      cmbEqualizerPreset.ItemIndex := 0;
      str := cmbEqualizerPreset.Items.Strings[cmbEqualizerPreset.ItemIndex];
      FillChar(fSaving.Preset,256,' ');
      if Length(str) > 0 then Move(PChar(str)^,fSaving.Preset,Length(str));
      CloseKey;
    end;
    Free;
  end;
end;

end.
