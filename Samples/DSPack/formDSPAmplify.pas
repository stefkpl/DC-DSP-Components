unit formDSPAmplify;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfrmDSPAmplify = class(TForm)
    PreAmp: TTrackBar;
    Label12: TLabel;
    cmbAmpPitch: TComboBox;
    Label13: TLabel;
    cmbAmpChannels: TComboBox;
    Label1: TLabel;
    chkPreAmp: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chkPreAmpClick(Sender: TObject);
    procedure cmbAmpChannelsChange(Sender: TObject);
    procedure PreAmpChange(Sender: TObject);
    procedure cmbAmpPitchChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDSPAmplify: TfrmDSPAmplify;

implementation

uses formMain;

{$R *.dfm}

procedure TfrmDSPAmplify.FormCreate(Sender: TObject);
begin
  Width := 80;
end;

procedure TfrmDSPAmplify.FormShow(Sender: TObject);
begin
  chkPreAmp.Checked := frmMain.DCAmplify.Enabled;
end;

procedure TfrmDSPAmplify.chkPreAmpClick(Sender: TObject);
begin
  frmMain.DCAmplify.Enabled := chkPreAmp.Checked;
end;

procedure TfrmDSPAmplify.cmbAmpChannelsChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbAmpChannels.ItemIndex;
  frmMain.DCAmplify.Seperate := i <> 0;
  if i > 0 then dec(i);
  PreAmp.Position := PreAmp.Max - (Int64(frmMain.DCAmplify.Volume[i]) div 10);
  PreAmpChange(Self);
end;

procedure TfrmDSPAmplify.PreAmpChange(Sender: TObject);
var
  i : integer;
begin
  label12.Caption := inttostr((PreAmp.Max - PreAmp.Position) div 10) + ' %';
  i := cmbAmpChannels.ItemIndex;
  if i > 0 then dec(i);
  frmMain.DCAmplify.Volume[i] := (PreAmp.Max - PreAmp.Position) * 10;
end;

procedure TfrmDSPAmplify.cmbAmpPitchChange(Sender: TObject);
var
  i : integer;
  z : integer;
begin
  PreAmp.Max := (cmbAmpPitch.ItemIndex + 1) * 1000;
  i := cmbAmpChannels.ItemIndex;
  if i > 0 then dec(i);
  z := PreAmp.Max - (Int64(frmMain.DCAmplify.Volume[i]) div 10);
  if z > PreAmp.Max then z := PreAmp.Max;
  PreAmp.Position := z;
  PreAmpChange(Self);
end;

end.
