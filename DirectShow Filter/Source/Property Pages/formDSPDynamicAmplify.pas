unit formDSPDynamicAmplify;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dspDynamicAmplify, StdCtrls, ComCtrls, ExtCtrls, DynamicFilterList;

type
  TfrmDSPDynamicAmplify = class(TForm)
    Timer1: TTimer;
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    label7: TLabel;
    Label10: TLabel;
    label5: TLabel;
    Label11: TLabel;
    label8: TLabel;
    Label14: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    tbDynAmp: TTrackBar;
    tbAttack: TTrackBar;
    tbRelease: TTrackBar;
    procedure tbDynAmpChange(Sender: TObject);
    procedure tbAttackChange(Sender: TObject);
    procedure tbReleaseChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    fPlugin : TDCDynamicAmplify;
    fItem : TDCFilterItem;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDSPDynamicAmplify: TfrmDSPDynamicAmplify;

implementation

{$R *.dfm}

constructor TfrmDSPDynamicAmplify.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCDynamicAmplify(Item.Filter);
  fItem := Item;
  tbDynAmp.Position := fPlugin.MaxAmplification div 10;
  tbAttack.Position := fPlugin.AttackTime;
  tbRelease.Position := fPlugin.ReleaseTime;
  tbDynAmpChange(Self);
  tbAttackChange(Self);
  tbReleaseChange(Self);
  Show;
end;

procedure TfrmDSPDynamicAmplify.tbDynAmpChange(Sender: TObject);
begin
  label7.Caption := Format('%.1fx', [tbDynAmp.Position / 100]);
  fPlugin.MaxAmplification := tbDynAmp.Position * 10;
end;

procedure TfrmDSPDynamicAmplify.tbAttackChange(Sender: TObject);
begin
  label5.Caption := Format('%.1fdb', [tbAttack.Position / 1000]);
  fPlugin.AttackTime := tbAttack.Position;
end;

procedure TfrmDSPDynamicAmplify.tbReleaseChange(Sender: TObject);
begin
  label8.Caption := Format('%.1fs', [tbRelease.Position / 1000]);
  fPlugin.ReleaseTime := tbRelease.Position;
end;

procedure TfrmDSPDynamicAmplify.Timer1Timer(Sender: TObject);
begin
  label3.Caption := Format('%.1fx', [fPlugin.CurrentAmplification]);
end;

end.
