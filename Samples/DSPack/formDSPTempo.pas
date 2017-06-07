unit formDSPTempo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfrmDSPTempo = class(TForm)
    Label12: TLabel;
    Label13: TLabel;
    tbTempo: TTrackBar;
    chkEnabled: TCheckBox;
    procedure tbTempoChange(Sender: TObject);
    procedure chkEnabledClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDSPTempo: TfrmDSPTempo;

implementation

uses formMain;

{$R *.dfm}

procedure TfrmDSPTempo.tbTempoChange(Sender: TObject);
begin
  label13.Caption := Format('%.2fx', [tbTempo.Position / 1000]);
  frmMain.DCTempo.Tempo := tbTempo.Position;
end;

procedure TfrmDSPTempo.chkEnabledClick(Sender: TObject);
begin
  frmMain.DCTempo.Enabled := chkEnabled.Checked;
end;

end.
