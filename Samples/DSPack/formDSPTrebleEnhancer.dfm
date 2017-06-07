object frmDSPTrebleEnhancer: TfrmDSPTrebleEnhancer
  Left = 192
  Top = 160
  BorderStyle = bsToolWindow
  Caption = ' Treble Enhancer'
  ClientHeight = 85
  ClientWidth = 350
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label12: TLabel
    Left = 8
    Top = 13
    Width = 35
    Height = 13
    Caption = 'Volume'
  end
  object Label13: TLabel
    Left = 328
    Top = 13
    Width = 17
    Height = 13
    Alignment = taRightJustify
    Caption = '0 %'
  end
  object Label8: TLabel
    Left = 240
    Top = 58
    Width = 42
    Height = 13
    Caption = 'Channel '
  end
  object tbTrebleEnhancer: TTrackBar
    Left = 72
    Top = 8
    Width = 225
    Height = 33
    Max = 2000
    Frequency = 200
    TabOrder = 0
    OnChange = tbTrebleEnhancerChange
  end
  object chkEnabled: TCheckBox
    Left = 8
    Top = 56
    Width = 73
    Height = 17
    Caption = 'Enabled'
    TabOrder = 1
    OnClick = chkEnabledClick
  end
  object cmbTrebleEnhancerChannels: TComboBox
    Left = 288
    Top = 54
    Width = 49
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 2
    Text = 'All'
    OnChange = cmbTrebleEnhancerChannelsChange
    Items.Strings = (
      'All'
      'CH0'
      'CH1'
      'CH2'
      'CH3'
      'CH4'
      'CH5'
      'CH6'
      'CH7'
      'CH8'
      'CH9')
  end
end
