unit formDSPTrueBass;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfrmDSPTrueBass = class(TForm)
    Label12: TLabel;
    Label13: TLabel;
    tbTrueBass: TTrackBar;
    chkEnabled: TCheckBox;
    cmbTrueBassChannels: TComboBox;
    Label8: TLabel;
    procedure chkEnabledClick(Sender: TObject);
    procedure cmbTrueBassChannelsChange(Sender: TObject);
    procedure tbTrueBassChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDSPTrueBass: TfrmDSPTrueBass;

implementation

uses formMain, dspTrueBass;

{$R *.dfm}

procedure TfrmDSPTrueBass.chkEnabledClick(Sender: TObject);
begin
  frmMain.DCTrueBass.Enabled := chkEnabled.Checked;
end;

procedure TfrmDSPTrueBass.cmbTrueBassChannelsChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbTrueBassChannels.ItemIndex;
  frmMain.DCTrueBass.Seperate := i <> 0;
  if i > 0 then dec(i);
  tbTrueBass.Position := frmMain.DCTrueBass.Volume[i];
  tbTrueBassChange(Self);
end;

procedure TfrmDSPTrueBass.tbTrueBassChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbTrueBassChannels.ItemIndex;
  if i > 0 then dec(i);
  label13.Caption := inttostr(tbTrueBass.Position div 100) + ' %';
  frmMain.DCTrueBass.Volume[i] := tbTrueBass.Position;
end;

end.
