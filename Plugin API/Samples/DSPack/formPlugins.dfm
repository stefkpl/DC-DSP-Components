object frmPlugins: TfrmPlugins
  Left = 190
  Top = 158
  BorderStyle = bsDialog
  Caption = 'Plugins'
  ClientHeight = 446
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlPluginsMain: TPanel
    Left = 146
    Top = 0
    Width = 362
    Height = 302
    BevelOuter = bvLowered
    TabOrder = 0
    object pnlPlugins: TPanel
      Left = 1
      Top = 1
      Width = 360
      Height = 300
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
    end
  end
  object cmbPlugins: TComboBox
    Left = 0
    Top = 280
    Width = 146
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 1
    Text = 'DSP Plugins'
    OnChange = cmbPluginsChange
    Items.Strings = (
      'DSP Plugins'
      'Visual Plugins')
  end
  object tvPlugins: TTreeView
    Left = 0
    Top = 0
    Width = 145
    Height = 279
    HideSelection = False
    Indent = 19
    ReadOnly = True
    RightClickSelect = True
    RowSelect = True
    TabOrder = 2
    OnAdvancedCustomDrawItem = tvPluginsAdvancedCustomDrawItem
    OnChange = tvPluginsChange
    OnClick = tvPluginsClick
    OnMouseDown = tvPluginsMouseDown
  end
end
