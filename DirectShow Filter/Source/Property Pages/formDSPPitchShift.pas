unit formDSPPitchShift;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dspPitchShift, StdCtrls, ComCtrls, DynamicFilterList;

type
  TfrmDSPPitchShift = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    Label13: TLabel;
    Label6: TLabel;
    tbPitchShift: TTrackBar;
    procedure tbPitchShiftChange(Sender: TObject);
  private
    fPlugin : TDCPitchShift;
    fItem : TDCFilterItem;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDSPPitchShift: TfrmDSPPitchShift;

implementation

{$R *.dfm}

constructor TfrmDSPPitchShift.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCPitchShift(Item.Filter);
  fItem := Item;
  tbPitchShift.Position := fPlugin.Pitch;
  label13.Caption := Format('%.2fx', [tbPitchShift.Position / 1000]);
  Show;
end;

procedure TfrmDSPPitchShift.tbPitchShiftChange(Sender: TObject);
begin
  label13.Caption := Format('%.2fx', [tbPitchShift.Position / 1000]);
  fPlugin.Pitch := tbPitchShift.Position;
end;

end.
