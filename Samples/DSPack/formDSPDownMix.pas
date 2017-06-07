unit formDSPDownMix;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmDSPDownMix = class(TForm)
    chkEnabled: TCheckBox;
    procedure chkEnabledClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDSPDownMix: TfrmDSPDownMix;

implementation

uses formMain;

{$R *.dfm}

procedure TfrmDSPDownMix.chkEnabledClick(Sender: TObject);
begin
  frmMain.DCDownMix.Enabled := chkEnabled.Checked;
end;

end.
