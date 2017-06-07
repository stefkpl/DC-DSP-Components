unit formDSPBandpass;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfrmDSPBandpass = class(TForm)
    Label10: TLabel;
    tbLow: TTrackBar;
    chkBandPassEnabled: TCheckBox;
    cmbBandPassChannels: TComboBox;
    Label1: TLabel;
    Label3: TLabel;
    tbHigh: TTrackBar;
    Label4: TLabel;
    procedure FormShow(Sender: TObject);
    procedure tbLowChange(Sender: TObject);
    procedure chkBandPassEnabledClick(Sender: TObject);
    procedure cmbBandPassChannelsChange(Sender: TObject);
  private
    { Private declarations }
    fStreamFrequency : integer;
    CanChange : Boolean;
  public
    { Public declarations }
  end;

var
  frmDSPBandpass: TfrmDSPBandpass;

implementation

uses formMain;

{$R *.dfm}

procedure TfrmDSPBandpass.FormShow(Sender: TObject);
begin
  fStreamFrequency := frmMain.DSPackDCDSPFilter.StreamInfo.Frequency div 2;
  CanChange := False;
  tbLow.Max := fStreamFrequency;
  tbHigh.Max := fStreamFrequency;
  cmbBandPassChannelsChange(Self);
  CanChange := True;
end;

procedure TfrmDSPBandpass.tbLowChange(Sender: TObject);
var
  i  :integer;
begin
  label3.Caption := inttostr(tbLow.Position) + ' - ' + inttostr(tbHigh.Position) + ' Hz';
  if not CanChange then Exit;
  i := cmbBandPassChannels.ItemIndex;
  if i > 0 then dec(i);
  frmMain.DCBandpass.CutoffLow[i] := tbLow.Position;
  frmMain.DCBandpass.CutoffHigh[i] := tbHigh.Position;
end;

procedure TfrmDSPBandpass.chkBandPassEnabledClick(Sender: TObject);
begin
  frmMain.DCBandpass.Enabled := chkBandPassEnabled.Checked;
end;

procedure TfrmDSPBandpass.cmbBandPassChannelsChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbBandPassChannels.ItemIndex;
  frmMain.DCBandpass.Seperate := i <> 0;
  if i > 0 then dec(i);
  CanChange := False;
  tbLow.Position := Round(frmMain.DCBandpass.CutoffLow[i]);
  tbHigh.Position := Round(frmMain.DCBandpass.CutoffHigh[i]);
  CanChange := True;
  tbLowChange(Self);
end;

end.
