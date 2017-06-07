unit formDMOGargle;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Buttons, DirectSound;

type
  TfrmDMOGargle = class(TForm)
    SpeedButton1: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    rbSquare: TRadioButton;
    rbTriangle: TRadioButton;
    tbRateHz: TTrackBar;
    chkEnabled: TCheckBox;
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbSquareClick(Sender: TObject);
    procedure rbTriangleClick(Sender: TObject);
    procedure tbRateHzChange(Sender: TObject);
    procedure chkEnabledClick(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateValues;
  public
    { Public declarations }
  end;

var
  frmDMOGargle: TfrmDMOGargle;

implementation

uses formMain, dmoGargle;

{$R *.dfm}

procedure TfrmDMOGargle.UpdateValues;
begin
  rbSquare.Checked := frmMain.DCDMOGargle.WaveShape = DSFXGARGLE_WAVE_SQUARE;
  rbTriangle.Checked := frmMain.DCDMOGargle.WaveShape = DSFXGARGLE_WAVE_TRIANGLE;
  tbRateHz.Position := frmMain.DCDMOGargle.RateHz;
  chkEnabled.Checked := frmMain.DCDMOGargle.Enabled;
end;

procedure TfrmDMOGargle.SpeedButton1Click(Sender: TObject);
begin
  frmMain.DCDMOGargle.ShowPropertyPage(Handle);
  UpdateValues;
end;

procedure TfrmDMOGargle.FormShow(Sender: TObject);
begin
  UpdateValues;
end;

procedure TfrmDMOGargle.rbSquareClick(Sender: TObject);
begin
  frmMain.DCDMOGargle.WaveShape := DSFXGARGLE_WAVE_SQUARE;
end;

procedure TfrmDMOGargle.rbTriangleClick(Sender: TObject);
begin
  frmMain.DCDMOGargle.WaveShape := DSFXGARGLE_WAVE_TRIANGLE;
end;

procedure TfrmDMOGargle.tbRateHzChange(Sender: TObject);
begin
  frmMain.DCDMOGargle.RateHz := tbRateHz.Position;
  label3.Caption := inttostr(tbRateHz.Position);
end;

procedure TfrmDMOGargle.chkEnabledClick(Sender: TObject);
begin
  frmMain.DCDMOGargle.Enabled := chkEnabled.Checked;
end;

end.
