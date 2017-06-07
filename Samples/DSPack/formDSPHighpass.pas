unit formDSPHighpass;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfrmDSPHighpass = class(TForm)
    tbHighPass: TTrackBar;
    chkHighPassEnabled: TCheckBox;
    Label11: TLabel;
    cmbHighPassChannels: TComboBox;
    Label4: TLabel;
    procedure tbHighPassChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chkHighPassEnabledClick(Sender: TObject);
    procedure cmbHighPassChannelsChange(Sender: TObject);
  private
    fStreamFrequency : integer;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDSPHighpass: TfrmDSPHighpass;

implementation

uses formMain, dspHighpass;

{$R *.dfm}

procedure TfrmDSPHighpass.tbHighPassChange(Sender: TObject);
var
  i : integer;
begin
  label4.Caption := inttostr((tbHighPass.Position * fStreamFrequency) div 10000) + ' - ' + inttostr(fStreamFrequency) + ' Hz';
  i := cmbHighPassChannels.ItemIndex;
  if i > 0 then dec(i);
  frmMain.DCHighpass.Cutoff[i] := tbHighPass.Position;
end;

procedure TfrmDSPHighpass.FormShow(Sender: TObject);
begin
  fStreamFrequency := frmMain.DSPackDCDSPFilter.StreamInfo.Frequency div 2;
  tbHighPassChange(Self);
end;

procedure TfrmDSPHighpass.chkHighPassEnabledClick(Sender: TObject);
begin
  frmMain.DCHighpass.Enabled := chkHighPassEnabled.Checked;
end;

procedure TfrmDSPHighpass.cmbHighPassChannelsChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbHighPassChannels.ItemIndex;
  frmMain.DCHighpass.Seperate := i <> 0;
  if i > 0 then dec(i);
  tbHighPass.Position := frmMain.DCHighpass.Cutoff[i];
  tbHighPassChange(Self);
end;

end.
