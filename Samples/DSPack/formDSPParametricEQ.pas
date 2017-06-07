unit formDSPParametricEQ;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfrmDSPParametricEQ = class(TForm)
    Label6: TLabel;
    Label7: TLabel;
    Label1: TLabel;
    Label8: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label15: TLabel;
    tbFrequency: TTrackBar;
    tbGain: TTrackBar;
    chkParametricEQEnabled: TCheckBox;
    tbQ: TTrackBar;
    cmbParametricEQChannels: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure chkParametricEQEnabledClick(Sender: TObject);
    procedure tbFrequencyChange(Sender: TObject);
    procedure cmbParametricEQChannelsChange(Sender: TObject);
  private
    CanChange : Boolean;
    procedure SetupValues;
    procedure UpdateControls;
  public
    { Public declarations }
  end;

var
  frmDSPParametricEQ: TfrmDSPParametricEQ;

implementation

uses formMain;

{$R *.dfm}

procedure TfrmDSPParametricEQ.SetupValues;
var
  i : integer;
begin
  label6.Caption := currtostr(tbFrequency.Position / 100) + ' Hz';
  label8.Caption := currtostr(tbGain.Position / 100) + ' db';
  label3.Caption := currtostr(tbQ.Position / 100);
  if CanChange then Exit;
  i := cmbParametricEQChannels.ItemIndex;
  if i > 0 then dec(i);
  frmMain.DCParametricEQ.Frequency[i] := tbFrequency.Position / 100;
  frmMain.DCParametricEQ.GainDB[i] := tbGain.Position / 100;
  frmMain.DCParametricEQ.Q[i] := tbQ.Position / 100;
end;

procedure TfrmDSPParametricEQ.UpdateControls;
var
  i : integer;
begin
  CanChange := True;
  i := cmbParametricEQChannels.ItemIndex;
  if i > 0 then dec(i);
  tbFrequency.Position := Round(frmMain.DCParametricEQ.Frequency[i] * 100);
  tbGain.Position := Round(frmMain.DCParametricEQ.GainDB[i] * 100);
  tbQ.Position := Round(frmMain.DCParametricEQ.Q[i] * 100);
  CanChange := False;
end;

procedure TfrmDSPParametricEQ.FormShow(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrmDSPParametricEQ.chkParametricEQEnabledClick(Sender: TObject);
begin
  frmMain.DCParametricEQ.Enabled := chkParametricEQEnabled.Checked;
end;

procedure TfrmDSPParametricEQ.tbFrequencyChange(Sender: TObject);
begin
  SetupValues;
end;

procedure TfrmDSPParametricEQ.cmbParametricEQChannelsChange(
  Sender: TObject);
begin
  frmMain.DCParametricEQ.Seperate := cmbParametricEQChannels.ItemIndex > 0;
  UpdateControls;
end;

end.
