object frmWA2DSPPlugins: TfrmWA2DSPPlugins
  Left = 192
  Top = 160
  BorderStyle = bsNone
  Caption = ' Winamp2 DSP Plugins'
  ClientHeight = 300
  ClientWidth = 360
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lbPlugins: TListBox
    Left = 3
    Top = 27
    Width = 354
    Height = 245
    ItemHeight = 13
    Items.Strings = (
      '(None)')
    TabOrder = 0
    OnClick = lbPluginsClick
  end
  object cmbPlugins: TComboBox
    Left = 3
    Top = 276
    Width = 275
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = cmbPluginsChange
  end
  object edPluginPath: TEdit
    Left = 3
    Top = 3
    Width = 275
    Height = 21
    TabStop = False
    ReadOnly = True
    TabOrder = 2
  end
  object Button1: TButton
    Left = 282
    Top = 276
    Width = 75
    Height = 21
    Caption = 'Configure'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 282
    Top = 3
    Width = 75
    Height = 21
    Hint = 'Set Plugins Directory'
    Caption = 'Browse ...'
    ParentShowHint = False
    ShowHint = True
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
