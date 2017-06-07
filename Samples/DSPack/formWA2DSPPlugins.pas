unit formWA2DSPPlugins;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmWA2DSPPlugins = class(TForm)
    Label1: TLabel;
    lbPlugins: TListBox;
    cmbPlugins: TComboBox;
    edPluginPath: TEdit;
    Button1: TButton;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button2Click(Sender: TObject);
    procedure lbPluginsClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cmbPluginsChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmWA2DSPPlugins: TfrmWA2DSPPlugins;

implementation

uses formMain, waDSPWrapper;

{$R *.dfm}

procedure TfrmWA2DSPPlugins.Button2Click(Sender: TObject);
var
  i : integer;
begin
  if OpenDialog1.Execute then
  begin
    lbPlugins.Clear;
    cmbPlugins.Clear;
    frmMain.DCWADSPWrapper.Plugins.Quit;
    frmMain.DCWADSPWrapper.Plugins.Clear;
    frmMain.DCWADSPWrapper.Plugins.Directory := ExtractFilePath(OpenDialog1.FileName);
    edPluginPath.Text := frmMain.DCWADSPWrapper.Plugins.Directory;
    lbPlugins.Items.Add('(None)');
    lbPlugins.ItemIndex := 0;
    if frmMain.DCWADSPWrapper.Plugins.Count > 0 then
      for i := 0 to frmMain.DCWADSPWrapper.Plugins.Count -1 do
        lbPlugins.Items.Add(frmMain.DCWADSPWrapper.Plugins.Items[i].PluginName);
  end;
end;

procedure TfrmWA2DSPPlugins.lbPluginsClick(Sender: TObject);
var
  i : integer;
begin
  cmbPlugins.Clear;
  frmMain.DCWADSPWrapper.Plugins.Quit;
  if lbPlugins.ItemIndex > 0 then
  begin
    for i := 0 to frmMain.DCWADSPWrapper.Plugins.Items[lbPlugins.ItemIndex -1].Count -1 do
      cmbPlugins.Items.Add(frmMain.DCWADSPWrapper.Plugins.Items[lbPlugins.ItemIndex -1].SubPluginName[i]);
    if cmbPlugins.Items.Count > 0 then cmbPlugins.ItemIndex := 0;
    cmbPluginsChange(Self);
  end;
end;

procedure TfrmWA2DSPPlugins.Button1Click(Sender: TObject);
begin
  if (lbPlugins.ItemIndex < 1) or (cmbPlugins.ItemIndex < 0) then Exit;
  frmMain.DCWADSPWrapper.Plugins.Items[lbPlugins.ItemIndex -1].Config;
end;

procedure TfrmWA2DSPPlugins.cmbPluginsChange(Sender: TObject);
begin
  frmMain.DCWADSPWrapper.Plugins.Quit;
  if (lbPlugins.ItemIndex < 1) or (cmbPlugins.ItemIndex < 0) then Exit;
  frmMain.DCWADSPWrapper.Plugins.Items[lbPlugins.ItemIndex -1].Init(cmbPlugins.ItemIndex);
end;

end.
