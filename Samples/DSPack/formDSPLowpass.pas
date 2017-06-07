unit formDSPLowpass;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfrmDSPLowpass = class(TForm)
    tbLowPass: TTrackBar;
    chkLowPassEnabled: TCheckBox;
    Label10: TLabel;
    cmbLowPassChannels: TComboBox;
    Label2: TLabel;
    procedure tbLowPassChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chkLowPassEnabledClick(Sender: TObject);
    procedure cmbLowPassChannelsChange(Sender: TObject);
  private
    fStreamFrequency : integer;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDSPLowpass: TfrmDSPLowpass;

implementation

uses formMain;

{$R *.dfm}

procedure TfrmDSPLowpass.tbLowPassChange(Sender: TObject);
var
  i  :integer;
begin
  label2.Caption := inttostr(0) + ' - ' + inttostr((tbLowPass.Position * fStreamFrequency) div 10000) + ' Hz';
  i := cmbLowPassChannels.ItemIndex;
  if i > 0 then dec(i);
  frmMain.DCLowpass.Cutoff[i] := tbLowPass.Position;
end;

procedure TfrmDSPLowpass.FormShow(Sender: TObject);
begin
  fStreamFrequency := frmMain.DSPackDCDSPFilter.StreamInfo.Frequency div 2;
  tbLowPassChange(Self);
end;

procedure TfrmDSPLowpass.chkLowPassEnabledClick(Sender: TObject);
begin
  frmMain.DCLowpass.Enabled := chkLowPassEnabled.Checked;
end;

procedure TfrmDSPLowpass.cmbLowPassChannelsChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbLowPassChannels.ItemIndex;
  frmMain.DCLowpass.Seperate := i <> 0;
  if i > 0 then dec(i);
  tbLowPass.Position := frmMain.DCLowpass.Cutoff[i];
  tbLowPassChange(Self);
end;

end.
