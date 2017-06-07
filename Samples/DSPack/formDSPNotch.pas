unit formDSPNotch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfrmDSPNotch = class(TForm)
    tbNotch: TTrackBar;
    chkEnabled: TCheckBox;
    Label10: TLabel;
    cmbChannels: TComboBox;
    Label2: TLabel;
    procedure tbNotchChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chkEnabledClick(Sender: TObject);
    procedure cmbChannelsChange(Sender: TObject);
  private
    { Private declarations }
    fStreamFrequency : integer;
  public
    { Public declarations }
  end;

var
  frmDSPNotch: TfrmDSPNotch;

implementation

uses formMain;

{$R *.dfm}

procedure TfrmDSPNotch.tbNotchChange(Sender: TObject);
var
  i  :integer;
begin
  label2.Caption := inttostr((tbNotch.Position * fStreamFrequency) div 10000) + ' Hz';
  i := cmbChannels.ItemIndex;
  if i > 0 then dec(i);
  frmMain.DCNotch.Cutoff[i] := tbNotch.Position;
end;

procedure TfrmDSPNotch.FormShow(Sender: TObject);
begin
  fStreamFrequency := frmMain.DSPackDCDSPFilter.StreamInfo.Frequency div 2;
  tbNotchChange(Self);
end;

procedure TfrmDSPNotch.chkEnabledClick(Sender: TObject);
begin
  frmMain.DCNotch.Enabled := chkEnabled.Checked;
end;

procedure TfrmDSPNotch.cmbChannelsChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbChannels.ItemIndex;
  frmMain.DCNotch.Seperate := i <> 0;
  if i > 0 then dec(i);
  tbNotch.Position := frmMain.DCNotch.Cutoff[i];
  tbNotchChange(Self);
end;

end.
