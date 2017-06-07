unit formWA2DSPPlugins;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmWA2DSPPlugins = class(TForm)
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
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    CanChange : Boolean;
    procedure UpdatePlugins(Path : String);
  public
    { Public declarations }
  end;

var
  frmWA2DSPPlugins: TfrmWA2DSPPlugins;

implementation

uses dsp1, waDSPWrapper;

{$R *.dfm}

procedure TfrmWA2DSPPlugins.UpdatePlugins(Path : String);
var
  i : integer;
begin
  lbPlugins.Clear;
  cmbPlugins.Clear;
  edPluginPath.Text := Wrapper.Directory;
  lbPlugins.Items.Add('(None)');
  lbPlugins.ItemIndex := 0;
  if Wrapper.Count > 0 then
    for i := 0 to Wrapper.Count -1 do
      lbPlugins.Items.Add(Wrapper.Items[i].PluginName);
end;

procedure TfrmWA2DSPPlugins.Button2Click(Sender: TObject);
var
  i : integer;
begin
  if OpenDialog1.Execute then
  begin
    lbPlugins.Clear;
    cmbPlugins.Clear;
    Wrapper.Quit;
    Wrapper.Clear;
    Wrapper.Directory := ExtractFilePath(OpenDialog1.FileName);
    edPluginPath.Text := Wrapper.Directory;
    lbPlugins.Items.Add('(None)');
    lbPlugins.ItemIndex := 0;
    if Wrapper.Count > 0 then
      for i := 0 to Wrapper.Count -1 do
        lbPlugins.Items.Add(Wrapper.Items[i].PluginName);
  end;
end;

procedure TfrmWA2DSPPlugins.lbPluginsClick(Sender: TObject);
var
  i : integer;
begin
  if CanChange and (lbPlugins.ItemIndex <> fLastPlugin) then
  begin
    cmbPlugins.Clear;
    Wrapper.Quit;
  end;
  if lbPlugins.ItemIndex > 0 then
  begin
    for i := 0 to Wrapper.Items[lbPlugins.ItemIndex -1].Count -1 do
      cmbPlugins.Items.Add(Wrapper.Items[lbPlugins.ItemIndex -1].SubPluginName[i]);
    if cmbPlugins.Items.Count > 0 then cmbPlugins.ItemIndex := 0;
    cmbPluginsChange(Self);
  end;
  if CanChange and (Wrapper.Count > 0) then
  begin
    fLastPlugin := lbPlugins.ItemIndex;
    if fLastPlugin > 0 then fLastPluginName := Wrapper.Items[fLastPlugin -1].Filename
                       else fLastPluginName := ''; 
  end;
end;

procedure TfrmWA2DSPPlugins.Button1Click(Sender: TObject);
begin
  if (lbPlugins.ItemIndex < 1) or (cmbPlugins.ItemIndex < 0) then Exit;
  Wrapper.Items[lbPlugins.ItemIndex -1].Config;
end;

procedure TfrmWA2DSPPlugins.cmbPluginsChange(Sender: TObject);
begin
  if not CanChange then Exit;
  if (lbPlugins.ItemIndex < 1) or (cmbPlugins.ItemIndex < 0) then Exit;
  Wrapper.Items[lbPlugins.ItemIndex -1].Init(cmbPlugins.ItemIndex);
  fLastPluginIndex := cmbPlugins.ItemIndex;
end;

procedure TfrmWA2DSPPlugins.FormShow(Sender: TObject);
begin
  CanChange := False;
  UpdatePlugins(Wrapper.Directory);
  if (fLastPlugin <> -1) and (fLastPluginIndex <> -1) then
  begin
    lbPlugins.ItemIndex := fLastPlugin;
    cmbPlugins.ItemIndex := fLastPluginIndex;
  end;
  lbPluginsClick(Self);
  CanChange := True;
end;

end.
