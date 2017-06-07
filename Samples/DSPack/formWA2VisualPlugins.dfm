object frmWA2VisualPlugins: TfrmWA2VisualPlugins
  Left = 192
  Top = 160
  BorderStyle = bsToolWindow
  Caption = ' Winamp2 Visual Plugins'
  ClientHeight = 268
  ClientWidth = 399
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 2
    Top = 2
    Width = 79
    Height = 13
    Caption = 'Plugins Directory'
  end
  object lbPlugins: TListBox
    Left = 0
    Top = 40
    Width = 394
    Height = 177
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbPluginsClick
  end
  object cmbPlugins: TComboBox
    Left = 0
    Top = 218
    Width = 394
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
  end
  object edPluginPath: TEdit
    Left = 0
    Top = 18
    Width = 319
    Height = 21
    TabStop = False
    ReadOnly = True
    TabOrder = 2
  end
  object Button1: TButton
    Left = 319
    Top = 242
    Width = 75
    Height = 21
    Caption = 'Configure'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 319
    Top = 18
    Width = 75
    Height = 21
    Caption = 'Browse ...'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 240
    Top = 242
    Width = 75
    Height = 21
    Caption = 'Stop'
    TabOrder = 5
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 160
    Top = 242
    Width = 75
    Height = 21
    Caption = 'Start'
    TabOrder = 6
    OnClick = Button4Click
  end
  object OpenDialog1: TOpenDialog
    Ctl3D = False
    Filter = 'Winamp Visual Plugins|vis_*.dll'
    Title = 'Choose Plugins Directory ...'
    Left = 272
    Top = 8
  end
end
