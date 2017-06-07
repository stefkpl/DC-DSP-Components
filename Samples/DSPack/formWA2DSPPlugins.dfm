object frmWA2DSPPlugins: TfrmWA2DSPPlugins
  Left = 192
  Top = 160
  BorderStyle = bsToolWindow
  Caption = ' Winamp2 DSP Plugins'
  ClientHeight = 239
  ClientWidth = 395
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
    Items.Strings = (
      '(None)')
    TabOrder = 0
    OnClick = lbPluginsClick
  end
  object cmbPlugins: TComboBox
    Left = 0
    Top = 218
    Width = 319
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = cmbPluginsChange
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
    Top = 218
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
  object OpenDialog1: TOpenDialog
    Ctl3D = False
    Filter = 'Winamp DSP Plugins|dsp_*.dll'
    Title = 'Choose Plugins Directory ...'
    Left = 8
    Top = 248
  end
end
