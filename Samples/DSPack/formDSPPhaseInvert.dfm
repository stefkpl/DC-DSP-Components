object frmDSPPhaseInvert: TfrmDSPPhaseInvert
  Left = 192
  Top = 160
  BorderStyle = bsToolWindow
  Caption = ' Phase Invert'
  ClientHeight = 70
  ClientWidth = 166
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label5: TLabel
    Left = 10
    Top = 16
    Width = 39
    Height = 13
    Caption = 'Channel'
  end
  object chkPhaseInvert: TCheckBox
    Left = 8
    Top = 48
    Width = 65
    Height = 17
    Caption = 'Enabled'
    TabOrder = 0
    OnClick = chkPhaseInvertClick
  end
  object cmbPhaseInvertChannels: TComboBox
    Left = 62
    Top = 12
    Width = 41
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 1
    Text = '0'
    OnChange = cmbPhaseInvertChannelsChange
    Items.Strings = (
      '0'
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8'
      '9')
  end
  object chkPhaseInvEnabled: TCheckBox
    Left = 114
    Top = 14
    Width = 49
    Height = 17
    Caption = 'Invert'
    TabOrder = 2
    OnClick = chkPhaseInvEnabledClick
  end
end
