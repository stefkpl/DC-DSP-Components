object frmDSPChannelOrder: TfrmDSPChannelOrder
  Left = 192
  Top = 156
  BorderStyle = bsToolWindow
  Caption = ' Channel Order'
  ClientHeight = 147
  ClientWidth = 399
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
  object Label8: TLabel
    Left = 11
    Top = 20
    Width = 54
    Height = 13
    Caption = 'Channel In '
  end
  object Label9: TLabel
    Left = 133
    Top = 20
    Width = 59
    Height = 13
    Caption = 'Channel Out'
  end
  object Label1: TLabel
    Left = 8
    Top = 48
    Width = 377
    Height = 73
    AutoSize = False
    WordWrap = True
  end
  object chkEnableChannelOrder: TCheckBox
    Left = 8
    Top = 124
    Width = 65
    Height = 17
    Caption = 'Enabled'
    TabOrder = 0
    OnClick = chkEnableChannelOrderClick
  end
  object cmbChannelSwitchIn: TComboBox
    Left = 69
    Top = 17
    Width = 43
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = cmbChannelSwitchInChange
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8')
  end
  object cmbChannelSwitchOut: TComboBox
    Left = 197
    Top = 17
    Width = 43
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = cmbChannelSwitchOutChange
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8')
  end
  object sbDefaultChannel: TButton
    Left = 312
    Top = 16
    Width = 75
    Height = 21
    Caption = 'Default'
    TabOrder = 3
    OnClick = sbDefaultChannelClick
  end
end
