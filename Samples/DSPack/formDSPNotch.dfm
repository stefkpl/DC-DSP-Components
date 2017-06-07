object frmDSPNotch: TfrmDSPNotch
  Left = 192
  Top = 160
  BorderStyle = bsToolWindow
  Caption = ' Notch'
  ClientHeight = 80
  ClientWidth = 396
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
  object Label10: TLabel
    Left = 168
    Top = 52
    Width = 42
    Height = 13
    Caption = 'Channel '
  end
  object Label2: TLabel
    Left = 280
    Top = 53
    Width = 104
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0 %'
  end
  object tbNotch: TTrackBar
    Left = 4
    Top = 5
    Width = 386
    Height = 37
    Hint = '0 %'
    Max = 10000
    ParentShowHint = False
    Frequency = 250
    Position = 10000
    ShowHint = False
    TabOrder = 0
    OnChange = tbNotchChange
  end
  object chkEnabled: TCheckBox
    Left = 8
    Top = 50
    Width = 65
    Height = 17
    Caption = 'Enabled'
    TabOrder = 1
    OnClick = chkEnabledClick
  end
  object cmbChannels: TComboBox
    Left = 216
    Top = 48
    Width = 49
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 2
    Text = 'All'
    OnChange = cmbChannelsChange
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
