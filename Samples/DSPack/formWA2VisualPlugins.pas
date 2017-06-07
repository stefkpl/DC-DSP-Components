unit formWA2VisualPlugins;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Spin, StdCtrls;

type
  TfrmWA2VisualPlugins = class(TForm)
    Label1: TLabel;
    lbPlugins: TListBox;
    cmbPlugins: TComboBox;
    edPluginPath: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button2Click(Sender: TObject);
    procedure lbPluginsClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmWA2VisualPlugins: TfrmWA2VisualPlugins;

implementation

uses formMain, waVisualWrapper;

{$R *.dfm}

procedure TfrmWA2VisualPlugins.Button2Click(Sender: TObject);
var
  i : integer;
begin
  if OpenDialog1.Execute then
  begin
    lbPlugins.Clear;
    cmbPlugins.Clear;
    frmMain.DCWAVisualWrapper.Plugins.Quit;
    frmMain.DCWAVisualWrapper.Plugins.Clear;
    frmMain.DCWAVisualWrapper.Plugins.Directory := ExtractFilePath(OpenDialog1.FileName);
    if frmMain.DCWAVisualWrapper.Plugins.Count > 0 then
      for i := 0 to frmMain.DCWAVisualWrapper.Plugins.Count -1 do
        lbPlugins.Items.Add(frmMain.DCWAVisualWrapper.Plugins.Items[i].PluginName);
    if lbPlugins.Count > 0 then
    begin
      lbPlugins.ItemIndex := 0;
      lbPluginsClick(Self);
    end;
  end;
end;

procedure TfrmWA2VisualPlugins.lbPluginsClick(Sender: TObject);
var
  i : integer;
begin
  cmbPlugins.Clear;
  if lbPlugins.ItemIndex > -1 then
  begin
    for i := 0 to frmMain.DCWAVisualWrapper.Plugins.Items[lbPlugins.ItemIndex].PluginCount -1 do
      cmbPlugins.Items.Add(frmMain.DCWAVisualWrapper.Plugins.Items[lbPlugins.ItemIndex].SubPluginName[i]);
    if cmbPlugins.Items.Count > 0 then cmbPlugins.ItemIndex := 0;
  end;
end;

procedure TfrmWA2VisualPlugins.Button1Click(Sender: TObject);
begin
  if (lbPlugins.ItemIndex < 0) and (cmbPlugins.ItemIndex < 0) then Exit;
  frmMain.DCWAVisualWrapper.Plugins.Items[lbPlugins.ItemIndex].Config(cmbPlugins.ItemIndex);
end;

procedure TfrmWA2VisualPlugins.Button4Click(Sender: TObject);
begin
  if (lbPlugins.ItemIndex < 0) or (cmbPlugins.ItemIndex < 0) then Exit;
  frmMain.DCWAVisualWrapper.Plugins.Quit;
  frmMain.DCWAVisualWrapper.Plugins.Items[lbPlugins.ItemIndex].Init(cmbPlugins.ItemIndex);
end;

procedure TfrmWA2VisualPlugins.Button3Click(Sender: TObject);
begin
  if (lbPlugins.ItemIndex < 0) or (cmbPlugins.ItemIndex < 0) then Exit;
  frmMain.DCWAVisualWrapper.Plugins.Items[lbPlugins.ItemIndex].Quit;
end;

end.
