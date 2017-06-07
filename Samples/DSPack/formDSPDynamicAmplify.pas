unit formDSPDynamicAmplify;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TfrmDSPDynamicAmplify = class(TForm)
    tbDynAmp: TTrackBar;
    chkDynAmp: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label7: TLabel;
    Label3: TLabel;
    Timer1: TTimer;
    Label4: TLabel;
    Label5: TLabel;
    tbAttack: TTrackBar;
    Label6: TLabel;
    Label8: TLabel;
    tbrelease: TTrackBar;
    procedure Timer1Timer(Sender: TObject);
    procedure chkDynAmpClick(Sender: TObject);
    procedure tbDynAmpChange(Sender: TObject);
    procedure tbAttackChange(Sender: TObject);
    procedure tbreleaseChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDSPDynamicAmplify: TfrmDSPDynamicAmplify;

implementation

uses formMain;

{$R *.dfm}

procedure TfrmDSPDynamicAmplify.Timer1Timer(Sender: TObject);
begin
  label3.Caption := Format('%.1fx', [frmMain.DCDynamicAmplify.CurrentAmplification]);
end;

procedure TfrmDSPDynamicAmplify.chkDynAmpClick(Sender: TObject);
begin
  frmMain.DCDynamicAmplify.Enabled := chkDynAmp.Checked;
end;

procedure TfrmDSPDynamicAmplify.tbDynAmpChange(Sender: TObject);
begin
  label7.Caption := Format('%.1fx', [tbDynAmp.Position / 1000]);
  frmMain.DCDynamicAmplify.MaxAmplification := tbDynAmp.Position;
end;

procedure TfrmDSPDynamicAmplify.tbAttackChange(Sender: TObject);
begin
  label5.Caption := Format('%.1fdb', [tbAttack.Position / 1000]);
  frmMain.DCDynamicAmplify.AttackTime := tbAttack.Position;
end;

procedure TfrmDSPDynamicAmplify.tbreleaseChange(Sender: TObject);
begin
  label8.Caption := Format('%.1fs', [tbrelease.Position / 1000]);
  frmMain.DCDynamicAmplify.ReleaseTime := tbrelease.Position;
end;

end.
