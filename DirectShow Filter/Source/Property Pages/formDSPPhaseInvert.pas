unit formDSPPhaseInvert;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dspPhaseInvert, StdCtrls, ComCtrls, DynamicFilterList, dspConst;

type
  TfrmDSPPhaseInvert = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    ch0: TCheckBox;
    ch1: TCheckBox;
    ch2: TCheckBox;
    ch3: TCheckBox;
    ch4: TCheckBox;
    ch5: TCheckBox;
    ch6: TCheckBox;
    ch7: TCheckBox;
    ch8: TCheckBox;
    ch9: TCheckBox;
    procedure chkPhaseInvEnabledClick(Sender: TObject);
  private
    fPlugin : TDCPhaseInvert;
    fItem : TDCFilterItem;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDSPPhaseInvert: TfrmDSPPhaseInvert;
  
implementation

{$R *.dfm}

constructor TfrmDSPPhaseInvert.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
var
  i : integer;
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCPhaseInvert(Item.Filter);
  fItem := Item;
  for i := 0 to MaxChannels -1 do
    (FindComponent('ch'+inttostr(i)) as TCheckBox).Checked := fPlugin.Invert[i];
  Show;
end;

procedure TfrmDSPPhaseInvert.chkPhaseInvEnabledClick(Sender: TObject);
begin
  fPlugin.Invert[(Sender as TCheckBox).Tag] := (Sender as TCheckBox).Checked;
end;

end.
