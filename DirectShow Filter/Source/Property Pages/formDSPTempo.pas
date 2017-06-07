unit formDSPTempo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dspTempo, StdCtrls, ComCtrls, DynamicFilterList;

type
  TfrmDSPTempo = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    Label13: TLabel;
    Label6: TLabel;
    tbTempo: TTrackBar;
    procedure tbTempoChange(Sender: TObject);
  private
    fPlugin : TDCTempo;
    fItem : TDCFilterItem;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDSPTempo: TfrmDSPTempo;

implementation

{$R *.dfm}

constructor TfrmDSPTempo.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCTempo(Item.Filter);
  fItem := Item;
  tbTempo.Position := fPlugin.Tempo;
  label13.Caption := Format('%.2fx', [tbTempo.Position / 1000]);
  Show;
end;

procedure TfrmDSPTempo.tbTempoChange(Sender: TObject);
begin
  label13.Caption := Format('%.2fx', [tbTempo.Position / 1000]);
  fPlugin.Tempo := tbTempo.Position;
end;

end.
