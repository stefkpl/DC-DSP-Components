object frmDSPPhaser: TfrmDSPPhaser
  Left = 193
  Top = 152
  BorderStyle = bsToolWindow
  Caption = ' Phaser'
  ClientHeight = 280
  ClientWidth = 393
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
  object Label6: TLabel
    Left = 336
    Top = 17
    Width = 48
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0 %'
  end
  object Label7: TLabel
    Left = 8
    Top = 56
    Width = 29
    Height = 13
    Caption = 'Depth'
  end
  object Label1: TLabel
    Left = 8
    Top = 17
    Width = 60
    Height = 13
    Caption = 'Dry/Wet Mix'
  end
  object Label8: TLabel
    Left = 336
    Top = 56
    Width = 48
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0 %'
  end
  object Label2: TLabel
    Left = 8
    Top = 96
    Width = 33
    Height = 13
    Caption = 'Stages'
  end
  object Label3: TLabel
    Left = 336
    Top = 96
    Width = 48
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0'
  end
  object Label9: TLabel
    Left = 8
    Top = 136
    Width = 48
    Height = 13
    Caption = 'Feedback'
  end
  object Label10: TLabel
    Left = 336
    Top = 136
    Width = 48
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0 %'
  end
  object Label11: TLabel
    Left = 8
    Top = 176
    Width = 55
    Height = 13
    Caption = 'Start Phase'
  end
  object Label12: TLabel
    Left = 336
    Top = 176
    Width = 48
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0'
  end
  object Label13: TLabel
    Left = 8
    Top = 216
    Width = 50
    Height = 13
    Caption = 'Frequency'
  end
  object Label14: TLabel
    Left = 336
    Top = 216
    Width = 48
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0'
  end
  object Label15: TLabel
    Left = 288
    Top = 260
    Width = 42
    Height = 13
    Caption = 'Channel '
  end
  object tbDryWetMix: TTrackBar
    Left = 72
    Top = 5
    Width = 265
    Height = 37
    Max = 255
    ParentShowHint = False
    Frequency = 8
    ShowHint = False
    TabOrder = 0
    TickMarks = tmBoth
    OnChange = tbDryWetMixChange
  end
  object tbDepth: TTrackBar
    Left = 72
    Top = 43
    Width = 265
    Height = 37
    Max = 255
    ParentShowHint = False
    Frequency = 8
    ShowHint = False
    TabOrder = 1
    TickMarks = tmBoth
    OnChange = tbDryWetMixChange
  end
  object chkPhaserEnabled: TCheckBox
    Left = 8
    Top = 256
    Width = 63
    Height = 17
    Caption = 'Enabled'
    TabOrder = 2
    OnClick = chkPhaserEnabledClick
  end
  object tbStages: TTrackBar
    Left = 72
    Top = 83
    Width = 265
    Height = 37
    Max = 255
    ParentShowHint = False
    Frequency = 8
    ShowHint = False
    TabOrder = 3
    TickMarks = tmBoth
    OnChange = tbDryWetMixChange
  end
  object tbFeedback: TTrackBar
    Left = 72
    Top = 123
    Width = 265
    Height = 37
    Max = 100
    ParentShowHint = False
    Frequency = 4
    ShowHint = False
    TabOrder = 4
    TickMarks = tmBoth
    OnChange = tbDryWetMixChange
  end
  object tbStartPhase: TTrackBar
    Left = 72
    Top = 163
    Width = 265
    Height = 37
    Max = 360
    Min = -360
    ParentShowHint = False
    Frequency = 22
    ShowHint = False
    TabOrder = 5
    TickMarks = tmBoth
    OnChange = tbDryWetMixChange
  end
  object tbFrequency: TTrackBar
    Left = 72
    Top = 203
    Width = 265
    Height = 37
    Max = 1000
    Min = 1
    ParentShowHint = False
    Frequency = 30
    Position = 1
    ShowHint = False
    TabOrder = 6
    TickMarks = tmBoth
    OnChange = tbDryWetMixChange
  end
  object cmbPhaserChannels: TComboBox
    Left = 336
    Top = 254
    Width = 49
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 7
    Text = 'All'
    OnChange = cmbPhaserChannelsChange
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
