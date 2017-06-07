unit formPlugins;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ImgList, ComCtrls, CommCtrl, DynTreeNode;

type
  TzpPluginType = (ptNone, ptDSP, ptVisual);

  TfrmPlugins = class(TForm)
    pnlPluginsMain: TPanel;
    cmbPlugins: TComboBox;
    tvPlugins: TTreeView;
    pnlPlugins: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure tvPluginsChange(Sender: TObject; Node: TTreeNode);
    procedure cmbPluginsChange(Sender: TObject);
    procedure tvPluginsAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure tvPluginsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tvPluginsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    fCurrentDSPIndex,
    fCurrentDSPSubIndex,
    fCurrentVisualIndex,
    fCurrentVisualSubIndex : integer;
    PluginType : TzpPluginType;
  public
    procedure UpdateDSPPlugins;
    procedure UpdateVisualPlugins;
    procedure CloseAllWindows;
    { Public declarations }
  end;

var
  frmPlugins: TfrmPlugins;

implementation

uses formMain;

{$R *.dfm}

procedure TfrmPlugins.FormCreate(Sender: TObject);
begin
  PluginType := ptNone;
  AutoSize := True;
  CloseAllWindows;
  UpdateDSPPlugins;
end;

procedure TfrmPlugins.UpdateDSPPlugins;
var
  i, z : integer;
  tn, tn2 : TDynTreeNode;
begin
  if PluginType = ptDSP then Exit;
  tvPlugins.Items.Clear;
  CloseAllWindows;
  PluginType := ptDSP;
  SetWindowLong(tvPlugins.Handle, GWL_STYLE, GetWindowLong(tvPlugins.Handle, GWL_STYLE) or TVS_CHECKBOXES);
  if frmMain.DCDSPPluginRenderer.Plugins.Count = 0 then Exit;
  for i := 0 to frmMain.DCDSPPluginRenderer.Plugins.Count -1 do
  begin
    tn := TDynTreeNode(tvPlugins.Items.Add(nil,frmMain.DCDSPPluginRenderer.Plugins.Items[i].PluginName));
    for z := 0 to frmMain.DCDSPPluginRenderer.Plugins.Items[i].Count -1 do
    begin
      tn2 := TDynTreeNode(tvPlugins.Items.AddChild(tn,frmMain.DCDSPPluginRenderer.Plugins.Items[i].SubPluginName[z]));
      tn2.Checked := frmMain.DCDSPPluginRenderer.Plugins.Items[i].Enabled[z];
    end;
  end;
end;

procedure TfrmPlugins.UpdateVisualPlugins;
var
  i, z : integer;
  tn : TDynTreeNode;
  Sel : integer;
begin
  if PluginType = ptVisual then Exit;
  tvPlugins.Items.Clear;
  CloseAllWindows;
  PluginType := ptVisual;
  SetWindowLong(tvPlugins.Handle, GWL_STYLE, GetWindowLong(tvPlugins.Handle, GWL_STYLE) and not TVS_CHECKBOXES);
  if frmMain.DCVISPluginRenderer.Plugins.Count = 0 then Exit;
  tvPlugins.Items.AddChild(nil,'(None)');
  for i := 0 to frmMain.DCVISPluginRenderer.Plugins.Count -1 do
  begin
    tn := TDynTreeNode(tvPlugins.Items.Add(nil,frmMain.DCVISPluginRenderer.Plugins.Items[i].PluginName));
    for z := 0 to frmMain.DCVISPluginRenderer.Plugins.Items[i].Count -1 do
      tvPlugins.Items.AddChild(tn,frmMain.DCVISPluginRenderer.Plugins.Items[i].SubPluginName[z]);
  end;
  if (frmMain.DCVISPluginRenderer.Plugins.CurrentPlugin > -1) and (frmMain.DCVISPluginRenderer.Plugins.CurrentPluginIndex > -1) then
  begin
    // Setup the selection to the current Visual Plugin
    Sel := 2;
    for i := 0 to frmMain.DCVISPluginRenderer.Plugins.CurrentPlugin -1 do
      Sel := Sel + frmMain.DCVISPluginRenderer.Plugins.Items[i].Count + 1;
    Sel := Sel + frmMain.DCVISPluginRenderer.Plugins.CurrentPluginIndex;
    tvPlugins.Items.Item[Sel].Selected := True;
    Windows.SetFocus(tvPlugins.Selected.Handle);
  end;
end;

procedure TfrmPlugins.tvPluginsChange(Sender: TObject; Node: TTreeNode);
begin
  case PluginType of
    ptDSP:
    begin
      CloseAllWindows;
      if node.HasChildren then
      begin
        fCurrentDSPIndex := node.Index;
        fCurrentDSPSubIndex := -1;
        frmMain.DCDSPPluginRenderer.Plugins.Items[fCurrentDSPIndex].AboutInit;
      end else
      begin
        fCurrentDSPIndex := node.Parent.Index;
        fCurrentDSPSubIndex := node.Index;
        frmMain.DCDSPPluginRenderer.Plugins.Items[fCurrentDSPIndex].ConfigInit(fCurrentDSPSubIndex);
      end;
    end;
    ptVisual:
    begin
      CloseAllWindows;
      if node.HasChildren then
      begin
        if node.Index > 0 then
        begin
          fCurrentVisualIndex := node.Index -1;
          fCurrentVisualSubIndex := -1;
          frmMain.DCVISPluginRenderer.Plugins.Items[fCurrentVisualIndex].AboutInit;
        end;  
      end else
      begin
        if node.Parent.Index > 0 then
        begin
          fCurrentVisualIndex := node.Parent.Index -1;
          fCurrentVisualSubIndex := node.Index;
          frmMain.DCVISPluginRenderer.Plugins.Init(fCurrentVisualIndex,fCurrentVisualSubIndex,frmMain.VideoWindow.Width,frmMain.VideoWindow.Height);
          frmMain.DCVISPluginRenderer.Plugins.Items[fCurrentVisualIndex].ConfigInit(fCurrentVisualSubIndex);
        end else
        begin
          frmMain.DCVISPluginRenderer.Plugins.Quit;
        end;
      end;
    end;
  end;
end;

procedure TfrmPlugins.cmbPluginsChange(Sender: TObject);
begin
  case cmbPlugins.ItemIndex of
    0: UpdateDSPPlugins;
    1: UpdateVisualPlugins;
  end;
end;

procedure TfrmPlugins.tvPluginsAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
begin
  if Node.HasChildren then PaintImages := False;
end;

procedure TfrmPlugins.tvPluginsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  tn : TTreeNode;
begin
  // Fix for Checkbox hiding. Otherwise a click on "A" of "About" isn't recognized.
  tn := tvPlugins.GetNodeAt(x,y);
  if (Assigned(tn) and not tn.Selected and tn.HasChildren) then tn.Selected := True;
end;

procedure TfrmPlugins.tvPluginsClick(Sender: TObject);
var
  i, z : integer;
  Checked : Boolean;
  tn : TTreeNode;
  fCount : integer;
begin
  fCount := 0;
  case PluginType of
    ptDSP:
    begin
      for i := 0 to frmMain.DCDSPPluginRenderer.Plugins.Count -1 do
      begin
        tn := tvPlugins.Items[fCount];
        FCount := fCount + tn.Count+1;
        for z := 0 to frmMain.DCDSPPluginRenderer.Plugins.Items[i].Count -1 do
        begin
          Checked := TDynTreeNode(tn.Item[z]).Checked;
          if Checked <> frmMain.DCDSPPluginRenderer.Plugins.Items[i].Enabled[z] then
          begin
            if Checked then frmMain.DCDSPPluginRenderer.Plugins.Items[i].Init(z)
                       else frmMain.DCDSPPluginRenderer.Plugins.Items[i].Quit(z);
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmPlugins.FormDestroy(Sender: TObject);
begin
  CloseAllWindows;
end;

procedure TfrmPlugins.CloseAllWindows;
begin
  case PluginType of
    ptDSP:
    begin
      if frmMain.DCDSPPluginRenderer.Plugins.Count > 0 then
      begin
        if (fCurrentDSPIndex <> -1) and (fCurrentDSPSubIndex <> -1) then
        begin
          frmMain.DCDSPPluginRenderer.Plugins.Items[fCurrentDSPIndex].ConfigClose(fCurrentDSPSubIndex);
        end else if (fCurrentDSPIndex <> -1) and (fCurrentDSPSubIndex = -1) then
        begin
          frmMain.DCDSPPluginRenderer.Plugins.Items[fCurrentDSPIndex].AboutClose;
        end;
      end;
      fCurrentDSPIndex := -1;
      fCurrentDSPSubIndex := -1;
    end;
    ptVisual:
    begin
      if frmMain.DCVISPluginRenderer.Plugins.Count > 0 then
      begin
        if (fCurrentVisualIndex <> -1) and (fCurrentVisualSubIndex <> -1) then
        begin
          frmMain.DCVISPluginRenderer.Plugins.Items[fCurrentVisualIndex].ConfigClose;
        end else if (fCurrentVisualIndex <> -1) and (fCurrentVisualSubIndex = -1) then
        begin
          frmMain.DCVISPluginRenderer.Plugins.Items[fCurrentVisualIndex].AboutClose;
        end;
      end;
      fCurrentVisualIndex := -1;
      fCurrentVisualSubIndex := -1;
    end;
  end;
end;


end.
