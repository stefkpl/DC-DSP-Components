object frmDSPFlanger: TfrmDSPFlanger
  Left = 192
  Top = 160
  BorderStyle = bsToolWindow
  Caption = ' Flanger'
  ClientHeight = 116
  ClientWidth = 433
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label6: TLabel
    Left = 352
    Top = 17
    Width = 64
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0 sec'
  end
  object Label7: TLabel
    Left = 8
    Top = 50
    Width = 62
    Height = 13
    Caption = 'Sweeplength'
  end
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 50
    Height = 13
    Caption = 'Frequency'
  end
  object Label8: TLabel
    Left = 368
    Top = 56
    Width = 48
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0 ms'
  end
  object Label15: TLabel
    Left = 328
    Top = 92
    Width = 42
    Height = 13
    Caption = 'Channel '
  end
  object tbFrequency: TTrackBar
    Left = 72
    Top = 5
    Width = 273
    Height = 37
    Max = 10000
    Min = 1
    ParentShowHint = False
    Frequency = 300
    Position = 1
    ShowHint = False
    TabOrder = 0
    OnChange = tbFrequencyChange
  end
  object tbDelay: TTrackBar
    Left = 72
    Top = 43
    Width = 273
    Height = 37
    Max = 100
    Min = 1
    ParentShowHint = False
    Frequency = 3
    Position = 1
    ShowHint = False
    TabOrder = 1
    OnChange = tbDelayChange
  end
  object chkEnabled: TCheckBox
    Left = 8
    Top = 88
    Width = 63
    Height = 17
    Caption = 'Enabled'
    TabOrder = 2
    OnClick = chkEnabledClick
  end
  object cmbChannels: TComboBox
    Left = 376
    Top = 86
    Width = 49
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 3
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
  object chkInvertEnabled: TCheckBox
    Left = 82
    Top = 88
    Width = 95
    Height = 17
    Caption = 'Phaseinvert'
    TabOrder = 4
    OnClick = chkInvertEnabledClick
  end
end
