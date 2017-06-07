unit formDSPEqualizer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, dspConst;

const
  NumEQBands = 10;

  EQFreq : array[0..NumEQBands] of Word = (
    0, 3, 9, 16, 29, 48, 100, 141, 280, 559, 1024
  );

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
  TfrmDSPEqualizer = class(TForm)
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
    chkEqualizerEnabled: TCheckBox;
    cmbEqualizerPreset: TComboBox;
    Label1: TLabel;
    Label10: TLabel;
    Label12: TLabel;
    procedure eq0Change(Sender: TObject);
    procedure chkEqualizerEnabledClick(Sender: TObject);
    procedure cmbEQChannelsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmbEqualizerPresetChange(Sender: TObject);
  private
    { Private declarations }
    fEQChanging : Boolean;
  public
    { Public declarations }
  end;

var
  frmDSPEqualizer: TfrmDSPEqualizer;

implementation

uses formMain;

{$R *.dfm}

procedure TfrmDSPEqualizer.eq0Change(Sender: TObject);
var
  i, z, c : integer;
  tb : TTrackBar;
begin
  i := cmbEQChannels.ItemIndex;
  if i > 0 then dec(i);
  for c := 0 to NumEQBands -1 do
  begin
    tb := FindComponent('eq'+inttostr(c)) as TTrackBar;
    if not fEQChanging then
      for z := EQFreq[c] to EQFreq[c+1] -1 do
        frmMain.DCEqualizer.Band[i,z] := 0 - tb.Position;
    (FindComponent('lab' + inttostr(c)) as TLabel).Caption := Inttostr(Round(0 - tb.Position / 5));
  end;
end;

procedure TfrmDSPEqualizer.chkEqualizerEnabledClick(Sender: TObject);
begin
  frmMain.DCEqualizer.Enabled := chkEqualizerEnabled.Checked;
end;

procedure TfrmDSPEqualizer.cmbEQChannelsChange(Sender: TObject);
var
  i, c : integer;
begin
  fEQChanging := True;
  i := cmbEQChannels.ItemIndex;
  frmMain.DCEqualizer.Seperate := i > 0;
  if i > 0 then dec(i);
  for c := 0 to NumEQBands -1 do
    (FindComponent('eq'+inttostr(c)) as TTrackBar).Position := 0 - frmMain.DCEqualizer.Band[i,EQFreq[c]];
  fEQChanging := False;
end;

procedure TfrmDSPEqualizer.FormCreate(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to EQPresetCount -1 do cmbEqualizerPreset.Items.Add(EQPresetNames[i]);
  cmbEqualizerPreset.ItemIndex := 0;
end;

procedure TfrmDSPEqualizer.cmbEqualizerPresetChange(Sender: TObject);
var
  i, c, z : integer;
begin
  for c := 0 to NumEQBands -1 do
  begin
    for z := EQFreq[c] to EQFreq[c+1] -1 do
      for i := 0 to MaxChannels -1 do frmMain.DCEqualizer.Band[i,z] := 0 - EQPreset[cmbEqualizerPreset.ItemIndex,c];
  end;
  cmbEQChannelsChange(Self);
end;

end.
