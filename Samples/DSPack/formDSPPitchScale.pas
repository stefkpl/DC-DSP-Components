unit formDSPPitchScale;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Spin, dspConst;

type
  TfrmDSPPitchScale = class(TForm)
    Label12: TLabel;
    Label13: TLabel;
    tbPitchScale: TTrackBar;
    chkEnabled: TCheckBox;
    cmbFFT: TComboBox;
    spQuality: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure chkEnabledClick(Sender: TObject);
    procedure tbPitchScaleChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmbFFTChange(Sender: TObject);
    procedure spQualityChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDSPPitchScale: TfrmDSPPitchScale;

implementation

uses formMain;

{$R *.dfm}

procedure TfrmDSPPitchScale.chkEnabledClick(Sender: TObject);
begin
  frmMain.DCPitchScale.Enabled := chkEnabled.Checked;
end;

procedure TfrmDSPPitchScale.tbPitchScaleChange(Sender: TObject);
begin
  label13.Caption := Format('%.2fx', [tbPitchScale.Position / 1000]);
  frmMain.DCPitchScale.Pitch[0] := tbPitchScale.Position;
end;

procedure TfrmDSPPitchScale.FormShow(Sender: TObject);
begin
  spQuality.Value := frmMain.DCPitchScale.Quality[0];
  cmbFFT.ItemIndex := integer(frmMain.DCPitchScale.FFTSize);
end;

procedure TfrmDSPPitchScale.cmbFFTChange(Sender: TObject);
begin
  frmMain.DCPitchScale.FFTSize := TDCFFTSize(cmbFFT.ItemIndex);
end;

procedure TfrmDSPPitchScale.spQualityChange(Sender: TObject);
begin
  frmMain.DCPitchScale.Quality[0] := spQuality.Value;
end;

end.
