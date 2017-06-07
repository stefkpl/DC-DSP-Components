unit formDSPTrebleEnhancer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfrmDSPTrebleEnhancer = class(TForm)
    Label12: TLabel;
    Label13: TLabel;
    tbTrebleEnhancer: TTrackBar;
    chkEnabled: TCheckBox;
    cmbTrebleEnhancerChannels: TComboBox;
    Label8: TLabel;
    procedure chkEnabledClick(Sender: TObject);
    procedure cmbTrebleEnhancerChannelsChange(Sender: TObject);
    procedure tbTrebleEnhancerChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDSPTrebleEnhancer: TfrmDSPTrebleEnhancer;

implementation

uses formMain;

{$R *.dfm}

procedure TfrmDSPTrebleEnhancer.chkEnabledClick(Sender: TObject);
begin
  frmMain.DCTrebleEnhancer.Enabled := chkEnabled.Checked;
end;

procedure TfrmDSPTrebleEnhancer.cmbTrebleEnhancerChannelsChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbTrebleEnhancerChannels.ItemIndex;
  frmMain.DCTrebleEnhancer.Seperate := i <> 0;
  if i > 0 then dec(i);
  tbTrebleEnhancer.Position := frmMain.DCTrebleEnhancer.Volume[i];
  tbTrebleEnhancerChange(Self);
end;

procedure TfrmDSPTrebleEnhancer.tbTrebleEnhancerChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbTrebleEnhancerChannels.ItemIndex;
  if i > 0 then dec(i);
  label13.Caption := inttostr(tbTrebleEnhancer.Position div 20) + ' %';
  frmMain.DCTrebleEnhancer.Volume[i] := tbTrebleEnhancer.Position;
end;

end.
